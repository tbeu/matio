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
    mat_t *matfd = Mat_Open(file_name, MAT_ACC_RDONLY);
    if ( matfd == nullptr ) {
        return 0;
    }

    size_t n = 0;
    Mat_GetDir(matfd, &n);
    Mat_Rewind(matfd);

    matvar_t *matvar = nullptr;
    while ( (matvar = Mat_VarReadNextInfo(matfd)) != nullptr ) {
        Mat_VarReadDataAll(matfd, matvar);
        Mat_VarGetSize(matvar);
        Mat_VarFree(matvar);
    }

    Mat_Close(matfd);

    return 0;
}

#endif
