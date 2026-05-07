#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include "fuzzer_temp_file.h"
#include "matio.h"

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    FuzzerTemporaryFile temp_file(data, size);

    Mat_LogInit("matio_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if ( mat == nullptr ) {
        return 0;
    }

    // Inspect the file-level metadata.
    enum mat_acc mat_acess_mode = Mat_GetFileAccessMode(mat);
    const char *mat_file_name = Mat_GetFilename(mat);
    const char *mat_header = Mat_GetHeader(mat);
    enum mat_ft mat_version = Mat_GetVersion(mat);

    matvar_t *matvar = nullptr;
    while ( (matvar = Mat_VarReadNextInfo(mat)) != nullptr ) {
        Mat_VarReadDataAll(mat, matvar);
        Mat_VarGetSize(matvar);
        unsigned nfields = Mat_VarGetNumberOfFields(matvar);
        char *const *field_names = Mat_VarGetStructFieldnames(matvar);
        Mat_VarPrint(matvar, 0);
        Mat_VarPrint(matvar, 1);
        matvar_t *matvar_deep_copy = Mat_VarDuplicate(matvar, 1);
        Mat_VarFree(matvar);
        if ( matvar_deep_copy != nullptr ) {
            Mat_VarFree(matvar_deep_copy);
        }
    }

    Mat_Rewind(mat);

    Mat_Close(mat);
    return 0;
}
