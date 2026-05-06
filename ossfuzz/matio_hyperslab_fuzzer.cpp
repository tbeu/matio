// Copyright 2019 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Hyperslab read fuzzer: exercises Mat_VarReadData / Mat_VarReadDataLinear
// with attacker-derived start/stride/edge tuples. The other read fuzzers
// only exercise Mat_VarReadDataAll which goes through one specific code
// path; partial-region reads land in the type-specific decimation /
// byte-swap functions in read_data.c (~80% uncovered today). This harness
// drives those.

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

constexpr int kMaxRank = 4;

void TryHyperslab(mat_t *mat, matvar_t *var, FuzzedDataProvider &fdp) {
    if (var == nullptr || var->rank <= 0 || var->rank > kMaxRank) return;
    int rank = var->rank;
    int starts[kMaxRank] = {0};
    int strides[kMaxRank] = {0};
    int edges[kMaxRank] = {0};
    size_t buf_elems = 1;
    for (int i = 0; i < rank; ++i) {
        size_t dim = var->dims ? var->dims[i] : 1;
        if (dim == 0) dim = 1;
        if (dim > 1024) dim = 1024;  // bound output
        starts[i] = fdp.ConsumeIntegralInRange<int>(0,
            static_cast<int>(dim > 0 ? dim - 1 : 0));
        strides[i] = fdp.ConsumeIntegralInRange<int>(1, 8);
        size_t maxedge = (dim > (size_t)starts[i])
            ? ((dim - starts[i]) + strides[i] - 1) / strides[i] : 0;
        if (maxedge > 16) maxedge = 16;
        edges[i] = fdp.ConsumeIntegralInRange<int>(0,
            static_cast<int>(maxedge));
        buf_elems *= (edges[i] > 0) ? edges[i] : 1;
        if (buf_elems > 4096) return;  // bound allocation
    }

    size_t elem = (var->data_size > 0) ? (size_t)var->data_size : 8;
    std::vector<uint8_t> buf(buf_elems * elem * 2 + 16, 0);
    Mat_VarReadData(mat, var, buf.data(), starts, strides, edges);

    // Linear variant: pack start/stride/edge into single ints across the
    // total flattened space.
    size_t total = 1;
    for (int i = 0; i < rank; ++i) {
        size_t d = var->dims ? var->dims[i] : 1;
        if (d == 0) d = 1;
        if (d > 1024) d = 1024;
        total *= d;
    }
    if (total == 0) return;
    int lstart = fdp.ConsumeIntegralInRange<int>(0,
        static_cast<int>(total > 0 ? total - 1 : 0));
    int lstride = fdp.ConsumeIntegralInRange<int>(1, 8);
    int ledge = fdp.ConsumeIntegralInRange<int>(0,
        static_cast<int>(std::min<size_t>(64,
            (total > (size_t)lstart) ? (total - lstart) : 0)));
    if (ledge > 0) {
        std::vector<uint8_t> lbuf(((size_t)ledge) * elem * 2 + 16, 0);
        Mat_VarReadDataLinear(mat, var, lbuf.data(), lstart, lstride, ledge);
    }
}

}  // namespace

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size < 4) return 0;
    FuzzerTemporaryFile temp_file(data, size);
    FuzzedDataProvider fdp(data, size);

    Mat_LogInit("matio_hyperslab_fuzzer");
    mat_t *mat = Mat_Open(temp_file.filename(), MAT_ACC_RDONLY);
    if (mat == nullptr) return 0;

    size_t n = 0;
    char *const *dir = Mat_GetDir(mat, &n);
    if (n > 16) n = 16;

    // Iterate, reading each variable's info and then exercising hyperslab
    // and linear-region reads against it.
    matvar_t *var;
    int loops = 0;
    while ((var = Mat_VarReadNextInfo(mat)) != nullptr && loops < 16) {
        // First the metadata-driven hyperslab (no full data load).
        TryHyperslab(mat, var, fdp);
        // Then exercise the size / subscript helpers — these wrap arithmetic
        // on rank/dims and have their own coverage gaps.
        if (var->rank > 0 && var->rank <= kMaxRank) {
            int subs[kMaxRank] = {0};
            for (int i = 0; i < var->rank; ++i) {
                subs[i] = fdp.ConsumeIntegralInRange<int>(0, 8);
            }
            int idx = 0;
            int *dims_int = nullptr;
            // Mat_CalcSingleSubscript expects int dims; convert from size_t.
            std::vector<int> idims(var->rank);
            for (int i = 0; i < var->rank; ++i) {
                size_t d = var->dims ? var->dims[i] : 1;
                idims[i] = (int)(d > 1024 ? 1024 : d);
            }
            dims_int = idims.data();
            idx = Mat_CalcSingleSubscript(var->rank, dims_int, subs);
            (void)idx;
            int *back = Mat_CalcSubscripts(var->rank, dims_int,
                fdp.ConsumeIntegralInRange<int>(0, 1024));
            if (back != nullptr) free(back);

            size_t idx2 = 0;
            std::vector<size_t> sdims(var->rank);
            std::vector<size_t> ssubs(var->rank);
            for (int i = 0; i < var->rank; ++i) {
                sdims[i] = idims[i];
                ssubs[i] = (size_t)subs[i];
            }
            Mat_CalcSingleSubscript2(var->rank, sdims.data(),
                                     ssubs.data(), &idx2);
            size_t *back2 = Mat_CalcSubscripts2(var->rank, sdims.data(),
                fdp.ConsumeIntegralInRange<size_t>(0, 1024));
            if (back2 != nullptr) free(back2);
        }
        Mat_VarFree(var);
        ++loops;
    }

    // Try named reads (Mat_VarRead by name) over the directory and call
    // hyperslab on those (with full data already loaded).
    Mat_Rewind(mat);
    if (dir != nullptr && n > 0) {
        for (size_t i = 0; i < n && i < 8; ++i) {
            if (dir[i] == nullptr) continue;
            matvar_t *v = Mat_VarRead(mat, dir[i]);
            if (v != nullptr) {
                TryHyperslab(mat, v, fdp);
                Mat_VarFree(v);
            }
        }
    }

    Mat_Close(mat);
    return 0;
}
