// Copyright 2019 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef MATIO_WRAP_H_
#define MATIO_WRAP_H_

#include <cstddef>
#include <cstdint>
#include <cstdlib>

#include "matio.h"

int
MatioRead(const char *file_name)
{
    Mat_LogInit("ossfuzz");
    mat_t *matfd = Mat_Open(file_name, MAT_ACC_RDONLY);
    if ( matfd == nullptr ) {
        return 0;
    }

    enum mat_acc mat_acess_mode = Mat_GetFileAccessMode(matfd);
    const char *mat_file_name = Mat_GetFilename(matfd);
    const char *mat_header = Mat_GetHeader(matfd);
    enum mat_ft mat_version = Mat_GetVersion(matfd);

    size_t n = 0;
    char *const *dir = Mat_GetDir(matfd, &n);

    matvar_t *matvar = nullptr;
    while ( (matvar = Mat_VarReadNextInfo(matfd)) != nullptr ) {
        Mat_VarReadDataAll(matfd, matvar);
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

    Mat_Rewind(matfd);
    while ( (matvar = Mat_VarReadNext(matfd)) != nullptr ) {
        Mat_VarFree(matvar);
    }

    Mat_Rewind(matfd);
    for ( size_t i = 0; i < n; i++ ) {
        matvar = Mat_VarRead(matfd, dir[i]);
        Mat_VarFree(matvar);
    }

    Mat_Close(matfd);

    return 0;
}

#endif
