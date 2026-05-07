#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include "fuzzer_temp_file.h"
#include "matio.h"

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    FuzzerTemporaryFile temp_file(data, size);

    Mat_LogInit("matio_read_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if ( mat == nullptr ) {
        return 0;
    }

    size_t n = 0;
    char *const *dir = Mat_GetDir(mat, &n);

    for ( size_t i = 0; i < n; i++ ) {
        matvar_t *matvar = Mat_VarRead(mat, dir[i]);
        Mat_VarFree(matvar);
    }

    Mat_Close(mat);
    return 0;
}
