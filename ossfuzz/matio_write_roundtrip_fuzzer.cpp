/*
 * matio_write_roundtrip_fuzzer — round-trip harness for the write/serialise
 * side of matio (MATLAB MAT file I/O library).
 *
 * Threat model
 * ------------
 * - Untrusted input: the raw bytes of a MAT file, i.e. an attacker-supplied
 *   document (the same trust boundary as the existing read-only harnesses).
 * - Trust boundary: Mat_VarReadNext() parsing that file into matvar_t
 *   structures. Variable names, field names, dimensions, ranks, classes,
 *   sparsity patterns and values are all attacker-controlled.
 * - What this harness adds: every existing matio fuzz target opens files
 *   MAT_ACC_RDONLY only, so the entire serialise direction — Mat_CreateVer,
 *   Mat_VarWrite, Mat_VarWriteAppend, Mat_VarWriteInfo, Mat_VarDelete for the
 *   v4 / v5 (plain and zlib-compressed) and v7.3 (HDF5) containers — was
 *   dark. Here the attacker-influenced structures are serialised into a new
 *   file (version and compression chosen from the input), appended to with
 *   the read-write append machinery, partially deleted, and finally
 *   re-opened and re-parsed. Serialising attacker-influenced content and
 *   re-reading the result also exercises read-side edge branches with
 *   structurally valid but unusual files.
 * - What counts as a bug: any memory-safety violation (ASan/MSan/UBSan
 *   finding) while serialising or re-parsing attacker-influenced content.
 *
 * Bounds (keep the harness fast and allocation-bounded):
 * - at most kMaxVars variables read, at most kMaxOutVars written per file,
 * - each variable capped at kMaxVarBytes via Mat_VarGetSize,
 * - one output file, one append pass, one re-read pass,
 * - FuzzedDataProvider options are consumed from the tail of the input so
 *   the MAT header (front bytes) stays intact for parsing.
 */

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <unistd.h>
#include <vector>

#include <fuzzer/FuzzedDataProvider.h>

#include "fuzzer_temp_file.h"
#include "matio.h"

namespace
{

constexpr size_t kMaxVars = 16;       /* variables read from the input file */
constexpr size_t kMaxOutVars = 8;     /* variables written per pass */
constexpr size_t kMaxVarBytes = 1u << 20; /* 1 MiB per variable */

} // namespace

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if ( size < 4 )
        return 0;

    FuzzedDataProvider fdp(data, size);
    FuzzerTemporaryFile in_file(data, size);

    Mat_LogInit("matio_write_roundtrip_fuzzer");

    /* Stage 1: parse the untrusted file into bounded matvar_t structures. */
    mat_t *src = Mat_Open(in_file.filename(), MAT_ACC_RDONLY);
    if ( src == NULL )
        return 0;

    std::vector<matvar_t *> vars;
    matvar_t *matvar;
    while ( vars.size() < kMaxVars && (matvar = Mat_VarReadNext(src)) != NULL ) {
        if ( Mat_VarGetSize(matvar) > kMaxVarBytes ) {
            Mat_VarFree(matvar);
            continue;
        }
        vars.push_back(matvar);
    }

    /* Snapshot one directory name for the delete pass (mat->dir is freed by
     * Mat_Close, so copy it first). */
    size_t ndir = 0;
    char *const *dir = Mat_GetDir(src, &ndir);
    char *first_name = NULL;
    if ( dir != NULL && ndir > 0 && dir[0] != NULL )
        first_name = strdup(dir[0]);
    Mat_Close(src);

    if ( vars.empty() ) {
        free(first_name);
        return 0;
    }

    /* Options (consumed from the tail of the input). */
    enum mat_ft version = MAT_FT_MAT5;
    switch ( fdp.ConsumeIntegralInRange<int>(0, 2) ) {
        case 0:  version = MAT_FT_MAT4;  break;
        case 1:  version = MAT_FT_MAT5;  break;
        default: version = MAT_FT_MAT73; break;
    }
    enum matio_compression comp =
        fdp.ConsumeBool() ? MAT_COMPRESSION_ZLIB : MAT_COMPRESSION_NONE;

    /* Stage 2: serialise the first batch into a fresh file. */
    char out_name[] = "/tmp/matio_wrt_rt.XXXXXX";
    int out_fd = mkstemp(out_name);
    if ( out_fd < 0 ) {
        for ( size_t i = 0; i < vars.size(); i++ )
            Mat_VarFree(vars[i]);
        free(first_name);
        return 0;
    }
    (void)close(out_fd);

    size_t nout = (vars.size() + 1) / 2;
    if ( nout > kMaxOutVars )
        nout = kMaxOutVars;

    mat_t *out = Mat_CreateVer(out_name, NULL, version);
    if ( out != NULL ) {
        for ( size_t i = 0; i < nout; i++ )
            Mat_VarWrite(out, vars[i], comp);
        /* Info-only (placeholder) write of the last variable. */
        if ( nout > 0 )
            Mat_VarWriteInfo(out, vars[nout - 1]);
        Mat_Close(out);
    }

    /* Stage 3: re-open read-write and append the rest, then delete one
     * variable by its (attacker-supplied) name. */
    mat_t *app = Mat_Open(out_name, MAT_ACC_RDWR);
    if ( app != NULL ) {
        for ( size_t i = nout; i < vars.size(); i++ ) {
            int dim = fdp.ConsumeIntegralInRange<int>(0, 1) + 1;
            (void)Mat_VarWriteAppend(app, vars[i], comp, dim);
        }
        if ( first_name != NULL )
            (void)Mat_VarDelete(app, first_name);
        Mat_Close(app);
    }

    /* Stage 4: re-open and re-parse everything that was written. */
    mat_t *rt = Mat_Open(out_name, MAT_ACC_RDONLY);
    if ( rt != NULL ) {
        while ( (matvar = Mat_VarReadNext(rt)) != NULL ) {
            (void)Mat_VarGetSize(matvar);
            Mat_VarFree(matvar);
        }
        Mat_Close(rt);
    }

    for ( size_t i = 0; i < vars.size(); i++ )
        Mat_VarFree(vars[i]);
    free(first_name);
    (void)unlink(out_name);
    return 0;
}
