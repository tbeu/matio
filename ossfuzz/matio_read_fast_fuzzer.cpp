// Copyright 2019 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Fast read-only fuzzer that skips Mat_VarPrint(matvar, 1). The original
// matio_fuzzer's Mat_VarPrint with print_data=1 stringifies every element,
// so a mutated input claiming dims=[1<<30, 1<<30] hits libfuzzer's
// per-input timeout before any further inputs can be tried. Skipping the
// print exposes the parser/reader paths without the printer wedge,
// letting libfuzzer make progress on coverage of mat.c / mat5.c / mat73.c
// reader code that the print-heavy harness can't reach.

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>

#include "fuzzer_temp_file.h"
#include "matio.h"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    FuzzerTemporaryFile temp_file(data, size);

    Mat_LogInit("matio_read_fast_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if (mat == nullptr) return 0;

    // Inspect the file-level metadata.
    Mat_GetFileAccessMode(mat);
    Mat_GetFilename(mat);
    Mat_GetHeader(mat);
    Mat_GetVersion(mat);

    size_t n = 0;
    char *const *dir = Mat_GetDir(mat, &n);
    (void)dir;

    // First pass: iterate variables via ReadNextInfo, then ReadDataAll.
    matvar_t *var;
    int loops = 0;
    while ((var = Mat_VarReadNextInfo(mat)) != nullptr && loops < 64) {
        Mat_VarReadDataAll(mat, var);
        Mat_VarGetSize(var);
        Mat_VarGetNumberOfFields(var);
        Mat_VarGetStructFieldnames(var);
        // Only print metadata, never the data — print_data=0 stays bounded.
        Mat_VarPrint(var, 0);
        // Exercise duplication path (deep copy traversal).
        matvar_t *copy = Mat_VarDuplicate(var, 1);
        if (copy != nullptr) Mat_VarFree(copy);
        Mat_VarFree(var);
        ++loops;
    }

    // Second pass via ReadNext (full data read in one go).
    Mat_Rewind(mat);
    loops = 0;
    while ((var = Mat_VarReadNext(mat)) != nullptr && loops < 64) {
        Mat_VarFree(var);
        ++loops;
    }

    // Third pass: read each variable by name from the directory.
    Mat_Rewind(mat);
    if (dir != nullptr && n > 0 && n < 64) {
        for (size_t i = 0; i < n; ++i) {
            if (dir[i] != nullptr) {
                var = Mat_VarRead(mat, dir[i]);
                if (var != nullptr) Mat_VarFree(var);
            }
        }
    }

    Mat_Close(mat);
    return 0;
}
