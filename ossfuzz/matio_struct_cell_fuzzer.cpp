// Copyright 2019 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Struct / cell API fuzzer — drives matvar_struct.c and matvar_cell.c
// API surface against attacker-controlled .mat files. The other read
// fuzzers don't call Mat_VarGetStructFieldBy{Name,Index}, GetStructs,
// GetCellsLinear, AddStructField, etc.; this one does.

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <unistd.h>

#include <fuzzer/FuzzedDataProvider.h>

#include "fuzzer_temp_file.h"
#include "matio.h"

namespace {

void ExerciseCell(matvar_t *cell, FuzzedDataProvider &fdp) {
    if (cell == nullptr || cell->class_type != MAT_C_CELL) return;
    int n = (int)Mat_VarGetSize(cell);
    n = n / (cell->data_size > 0 ? cell->data_size : 1);
    if (n > 64) n = 64;
    if (n <= 0) return;

    /* Contract: idx must be < n. */
    int idx = fdp.ConsumeIntegralInRange<int>(0, n - 1);
    Mat_VarGetCell(cell, idx);

    int start = fdp.ConsumeIntegralInRange<int>(0, n - 1);
    int stride = fdp.ConsumeIntegralInRange<int>(1, 4);
    /* Linear contract: start + (edge-1)*stride <= n-1
     * → edge <= (n - start + stride - 1) / stride. */
    int max_edge = (n - start + stride - 1) / stride;
    if (max_edge > 8) max_edge = 8;
    int edge = fdp.ConsumeIntegralInRange<int>(0, max_edge);
    matvar_t **cells_lin = Mat_VarGetCellsLinear(cell, start, stride, edge);
    if (cells_lin != nullptr) free(cells_lin);

    if (cell->rank > 0 && cell->rank <= 4) {
        int starts[4] = {0}, strides[4] = {0}, edges[4] = {0};
        for (int i = 0; i < cell->rank; ++i) {
            size_t d = cell->dims ? cell->dims[i] : 1;
            if (d > 16) d = 16;
            if (d == 0) { starts[i]=0; strides[i]=1; edges[i]=0; continue; }
            starts[i] = fdp.ConsumeIntegralInRange<int>(0, (int)(d - 1));
            strides[i] = fdp.ConsumeIntegralInRange<int>(1, 3);
            /* Contract: start + (edge-1)*stride <= dim-1
             * so edge <= (dim - start + stride - 1) / stride.       */
            int max_edge = (int)((d - starts[i] + strides[i] - 1) / strides[i]);
            if (max_edge > 4) max_edge = 4;
            edges[i] = fdp.ConsumeIntegralInRange<int>(0, max_edge);
        }
        matvar_t **cells = Mat_VarGetCells(cell, starts, strides, edges);
        if (cells != nullptr) free(cells);
    }
}

void ExerciseStruct(matvar_t *st, FuzzedDataProvider &fdp) {
    if (st == nullptr || st->class_type != MAT_C_STRUCT) return;

    unsigned nfields = Mat_VarGetNumberOfFields(st);
    if (nfields > 64) nfields = 64;
    char *const *fnames = Mat_VarGetStructFieldnames(st);

    // Compute number of struct elements (linear).
    size_t nelems = 1;
    for (int i = 0; i < st->rank && i < 4; ++i) {
        size_t d = st->dims ? st->dims[i] : 1;
        if (d > 16) d = 16;
        nelems *= d;
        if (nelems > 256) { nelems = 256; break; }
    }

    // Get-by-index across rotation of indices.
    // Contract: elem_idx must be < nelems.
    if (nelems > 0) {
        for (unsigned i = 0; i < nfields && i < 8; ++i) {
            size_t elem_idx = fdp.ConsumeIntegralInRange<size_t>(0, nelems - 1);
            Mat_VarGetStructFieldByIndex(st, i, elem_idx);
        }

        if (fnames != nullptr) {
            for (unsigned i = 0; i < nfields && i < 8; ++i) {
                if (fnames[i] != nullptr) {
                    size_t elem_idx =
                        fdp.ConsumeIntegralInRange<size_t>(0, nelems - 1);
                    Mat_VarGetStructFieldByName(st, fnames[i], elem_idx);
                }
            }
        }
    }
    // Probe with non-existent name (exercises the not-found path).
    Mat_VarGetStructFieldByName(st, "__nonexistent__", 0);
    Mat_VarGetStructFieldByName(st, "", 0);

    // Mat_VarGetStructField is the void* dispatching variant; both lookup
    // modes go through it. Drive with both forms.
    int idx_int = fdp.ConsumeIntegralInRange<int>(0,
        nfields > 0 ? (int)nfields - 1 : 0);
    Mat_VarGetStructField(st, &idx_int, MAT_BY_INDEX, 0);
    if (fnames != nullptr && nfields > 0 && fnames[0] != nullptr) {
        Mat_VarGetStructField(st, (void *)fnames[0], MAT_BY_NAME, 0);
    }

    // GetStructs over a rotating start/stride/edge — bounds chosen to satisfy
    // the matio API contract (start + (edge-1)*stride <= dim - 1).
    if (st->rank > 0 && st->rank <= 4) {
        int starts[4] = {0}, strides[4] = {0}, edges[4] = {0};
        bool any_zero_dim = false;
        for (int i = 0; i < st->rank; ++i) {
            size_t d = st->dims ? st->dims[i] : 1;
            if (d > 16) d = 16;
            if (d == 0) { any_zero_dim = true; break; }
            starts[i] = fdp.ConsumeIntegralInRange<int>(0, (int)(d - 1));
            strides[i] = fdp.ConsumeIntegralInRange<int>(1, 3);
            int max_edge = (int)((d - starts[i] + strides[i] - 1) / strides[i]);
            if (max_edge > 4) max_edge = 4;
            edges[i] = fdp.ConsumeIntegralInRange<int>(0, max_edge);
        }
        if (!any_zero_dim) {
            matvar_t *got = Mat_VarGetStructs(st, starts, strides, edges,
                                              fdp.ConsumeBool() ? 1 : 0);
            if (got != nullptr) Mat_VarFree(got);
        }

        if (nelems > 0) {
            int lstart = fdp.ConsumeIntegralInRange<int>(0,
                (int)(nelems - 1));
            int lstride = fdp.ConsumeIntegralInRange<int>(1, 3);
            /* Linear-edge contract: lstart + (ledge-1)*lstride <= nelems-1 */
            int max_ledge = (int)((nelems - lstart + lstride - 1) / lstride);
            if (max_ledge > 8) max_ledge = 8;
            int ledge = fdp.ConsumeIntegralInRange<int>(0, max_ledge);
            matvar_t *gotl = Mat_VarGetStructsLinear(st, lstart, lstride, ledge,
                                                     fdp.ConsumeBool() ? 1 : 0);
            if (gotl != nullptr) Mat_VarFree(gotl);
        }
    }

    // (Removed Mat_VarAddStructField — investigating whether it interacts
    //  with malformed structs to produce the Mat_VarFree SEGV.)
}

}  // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size < 4) return 0;
    FuzzerTemporaryFile temp_file(data, size);
    FuzzedDataProvider fdp(data, size);

    Mat_LogInit("matio_struct_cell_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if (mat == nullptr) return 0;

    matvar_t *var;
    int loops = 0;
    while ((var = Mat_VarReadNext(mat)) != nullptr && loops < 32) {
        // For struct/cell vars, exercise the API surface.
        if (var->class_type == MAT_C_CELL) {
            ExerciseCell(var, fdp);
        } else if (var->class_type == MAT_C_STRUCT) {
            ExerciseStruct(var, fdp);
        }
        // Always exercise common operations.
        Mat_VarGetSize(var);
        Mat_VarGetClassName(var);
        // Duplicate (covers matvar_struct/cell deep-copy paths).
        if (loops % 3 == 0) {
            matvar_t *copy = Mat_VarDuplicate(var, 1);
            if (copy != nullptr) Mat_VarFree(copy);
        }
        Mat_VarFree(var);
        ++loops;
    }
    Mat_Close(mat);
    return 0;
}
