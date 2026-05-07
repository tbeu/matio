#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include "fuzzer_temp_file.h"
#include "matio.h"

extern "C" int
LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    FuzzerTemporaryFile temp_file(data, size);

    Mat_LogInit("matio_readdataall_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if ( mat == nullptr ) {
        return 0;
    }

    matvar_t *matvar = nullptr;
    while ( (matvar = Mat_VarReadNextInfo(mat)) != nullptr ) {
        Mat_VarReadDataAll(mat, matvar);
        Mat_VarFree(matvar);
    }

    Mat_Close(mat);
    return 0;
}
