/** @file mcos.c
 * Matlab Class Object System (MCOS) reading support
 * @ingroup MAT
 */
/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include "mcos.h"
#if defined(MCOS) && MCOS
#include "mat5.h"
#if defined(MAT73) && MAT73
#include "mat73.h"
#include <hdf5.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if defined(_MSC_VER) || defined(__MINGW32__)
#define strdup _strdup
#endif

/** @brief MCOS reference value identifying object metadata */
#if !defined(MCOS_REF_VALUE)
#define MCOS_REF_VALUE 0xDD000000U
#endif

/** @brief Parse MCOS opaque metadata from a uint32 array
 *
 * Extracts rank, dimensions, object IDs, and class ID from the metadata
 * array found in MCOS opaque variables. Used by both v5 and v7.3 readers.
 *
 * @param meta       The uint32 metadata array (already byteswapped)
 * @param meta_nvals Number of uint32 values in the array
 * @param matvar     The matvar to populate (rank, dims, internal fields)
 * @return MATIO_E_NO_ERROR on success, or MATIO_E_FILE_FORMAT_VIOLATION
 *         if the type name is "MCOS" but the reference value is invalid
 */
int
ParseOpaqueMetadata(const mat_uint32_t *meta, size_t meta_nvals, matvar_t *matvar)
{
    mat_uint32_t ndims, dim_start, obj_start;
    size_t total_objs = 1;

    if ( meta == NULL || meta_nvals < 4 || matvar == NULL )
        return MATIO_E_NO_ERROR;

    if ( matvar->internal->type_name != NULL && strcmp(matvar->internal->type_name, "MCOS") == 0 &&
         meta[0] != MCOS_REF_VALUE ) {
        return MATIO_E_FILE_FORMAT_VIOLATION;
    }

    ndims = meta[1];
    dim_start = 2;

    if ( dim_start + ndims + 1 > meta_nvals )
        return MATIO_E_NO_ERROR;

    /* Set rank and dims */
    free(matvar->dims);
    matvar->rank = (int)ndims;
    matvar->dims = (size_t *)malloc(ndims * sizeof(size_t));
    if ( matvar->dims != NULL ) {
        mat_uint32_t d;
        for ( d = 0; d < ndims; d++ ) {
            matvar->dims[d] = meta[dim_start + d];
            total_objs *= meta[dim_start + d];
        }
    }

    obj_start = dim_start + ndims;
    if ( obj_start + total_objs < meta_nvals ) {
        matvar->internal->num_objects = total_objs;
        matvar->internal->object_ids = (mat_uint32_t *)malloc(total_objs * sizeof(mat_uint32_t));
        if ( matvar->internal->object_ids != NULL ) {
            size_t k;
            for ( k = 0; k < total_objs; k++ )
                matvar->internal->object_ids[k] = meta[obj_start + k];
        }
        matvar->internal->class_id = meta[obj_start + total_objs];
    }

    return MATIO_E_NO_ERROR;
}

/** @brief Free a property set */
static void
FreePropertySet(mcos_property_set_t *ps)
{
    if ( ps != NULL && ps->props != NULL ) {
        size_t i;
        for ( i = 0; i < ps->nprops; i++ ) {
            free(ps->props[i].name);
        }
        free(ps->props);
        ps->props = NULL;
        ps->nprops = 0;
    }
}

/** @brief Free a dynamic property set */
static void
FreeDynamicPropertySet(mcos_dynamic_property_set_t *ps)
{
    if ( ps != NULL ) {
        free(ps->prop_ids);
        ps->prop_ids = NULL;
        ps->nprop_ids = 0;
    }
}

/** @brief Free the MCOS subsystem data
 *
 * @param mcos Pointer to the MCOS subsystem (cast from void*)
 */
void
Mat_MCOS_Free(void *mcos)
{
    mcos_subsystem_t *ss = (mcos_subsystem_t *)mcos;
    if ( ss == NULL )
        return;

    if ( ss->strings != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_strings; i++ )
            free(ss->strings[i]);
        free(ss->strings);
    }
    if ( ss->class_info != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_classes; i++ ) {
            free(ss->class_info[i].name);
            free(ss->class_info[i].namespace_);
        }
        free(ss->class_info);
    }
    free(ss->object_info);

    if ( ss->saveobj_props != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_saveobj_props; i++ )
            FreePropertySet(&ss->saveobj_props[i]);
        free(ss->saveobj_props);
    }
    if ( ss->normal_props != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_normal_props; i++ )
            FreePropertySet(&ss->normal_props[i]);
        free(ss->normal_props);
    }
    if ( ss->dynamic_props != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_dynamic_props; i++ )
            FreeDynamicPropertySet(&ss->dynamic_props[i]);
        free(ss->dynamic_props);
    }

    if ( ss->prop_cells != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_prop_cells; i++ )
            Mat_VarFree(ss->prop_cells[i]);
        free(ss->prop_cells);
    }

    if ( ss->class_aliases != NULL ) {
        size_t i;
        for ( i = 0; i < ss->num_class_aliases; i++ )
            free(ss->class_aliases[i]);
        free(ss->class_aliases);
    }

    Mat_VarFree(ss->defaults);

    free(ss);
}

/** @brief Parse the linking metadata from Cell 1 of the FileWrapper__
 *
 * Cell 1 contains: version(uint32), num_strings(uint32), 8 offsets(uint32),
 * 2 reserved(uint32), then null-terminated strings, then regions 1-5+.
 *
 * @param data     Pointer to the raw uint8 data from Cell 1
 * @param len      Length in bytes
 * @param ss       Subsystem structure to populate
 * @param byteswap Whether to byte-swap uint32 values
 * @return MATIO_E_NO_ERROR on success, MATIO_E_FILE_FORMAT_VIOLATION on
 *         malformed data, or MATIO_E_OUT_OF_MEMORY on allocation failure
 */
static int
ParseLinkingMetadata(const mat_uint8_t *data, size_t len, mcos_subsystem_t *ss, int byteswap)
{
    mat_uint32_t version, num_strings;
    mat_uint32_t offsets[8];
    size_t pos = 0;
    size_t i;

    if ( len < 40 ) /* minimum: version + num_strings + 8 offsets */
        return MATIO_E_FILE_FORMAT_VIOLATION;

    /* Read version indicator */
    memcpy(&version, data + pos, 4);
    pos += 4;
    if ( byteswap )
        Mat_uint32Swap(&version);
    if ( version < 2 || version > 4 ) {
        Mat_Warning("MCOS: Unknown FileWrapper__ version %u", (mat_uint32_t)version);
    }
    ss->file_wrapper_version = version;

    /* Read number of strings */
    memcpy(&num_strings, data + pos, 4);
    pos += 4;
    if ( byteswap )
        Mat_uint32Swap(&num_strings);

    /* Read 6 segment offsets + 2 reserved (total 8 uint32 fields) */
    for ( i = 0; i < 8; i++ ) {
        memcpy(&offsets[i], data + pos, 4);
        pos += 4;
        if ( byteswap )
            Mat_uint32Swap(&offsets[i]);
    }

    /* Parse null-terminated strings */
    ss->num_strings = num_strings;
    if ( num_strings > 0 ) {
        ss->strings = (char **)calloc(num_strings, sizeof(char *));
        if ( ss->strings == NULL )
            return MATIO_E_OUT_OF_MEMORY;
        for ( i = 0; i < num_strings; i++ ) {
            size_t start = pos;
            while ( pos < len && data[pos] != '\0' )
                pos++;
            if ( pos >= len ) {
                Mat_Critical("MCOS: String table truncated");
                return MATIO_E_FILE_FORMAT_VIOLATION;
            }
            ss->strings[i] = (char *)malloc(pos - start + 1);
            if ( ss->strings[i] == NULL )
                return MATIO_E_OUT_OF_MEMORY;
            memcpy(ss->strings[i], data + start, pos - start);
            ss->strings[i][pos - start] = '\0';
            pos++; /* skip null terminator */
        }
    }

    /* Region 1: Class identifiers
     * Each block: (namespace_idx, class_name_idx, 0, 0)
     * First block is all zeros
     */
    if ( offsets[0] < len ) {
        size_t region_end = (offsets[1] < len) ? offsets[1] : len;
        size_t region_pos = offsets[0];
        size_t nclasses = 0;
        size_t block_size = 4 * sizeof(mat_uint32_t);

        /* Skip the first all-zero block */
        if ( region_pos + block_size <= region_end )
            region_pos += block_size;

        /* Count classes */
        {
            size_t p = region_pos;
            while ( p + block_size <= region_end ) {
                nclasses++;
                p += block_size;
            }
        }

        ss->num_classes = nclasses;
        if ( nclasses > 0 ) {
            ss->class_info = (mcos_class_info_t *)calloc(nclasses, sizeof(mcos_class_info_t));
            if ( ss->class_info == NULL )
                return MATIO_E_OUT_OF_MEMORY;
            for ( i = 0; i < nclasses; i++ ) {
                mat_uint32_t ns_idx, name_idx;
                memcpy(&ns_idx, data + region_pos, 4);
                memcpy(&name_idx, data + region_pos + 4, 4);
                if ( byteswap ) {
                    Mat_uint32Swap(&ns_idx);
                    Mat_uint32Swap(&name_idx);
                }
                if ( ns_idx > 0 && ns_idx <= ss->num_strings )
                    ss->class_info[i].namespace_ = strdup(ss->strings[ns_idx - 1]);
                else
                    ss->class_info[i].namespace_ = strdup("");
                if ( name_idx > 0 && name_idx <= ss->num_strings )
                    ss->class_info[i].name = strdup(ss->strings[name_idx - 1]);
                else
                    ss->class_info[i].name = strdup("");
                if ( ss->class_info[i].namespace_ == NULL || ss->class_info[i].name == NULL )
                    return MATIO_E_OUT_OF_MEMORY;
                region_pos += block_size;
            }
        }
    }

    /* Region 3: Object identifiers (offset index 2)
     * Each block: (class_id, 0, 0, saveobj_id, normalobj_id, dependency_id)
     * First block is all zeros
     */
    if ( offsets[2] < len ) {
        size_t region_end = (offsets[3] < len) ? offsets[3] : len;
        size_t region_pos = offsets[2];
        size_t nobjects = 0;
        size_t block_size = 6 * sizeof(mat_uint32_t);

        /* Skip the first all-zero block */
        if ( region_pos + block_size <= region_end )
            region_pos += block_size;

        /* Count objects */
        {
            size_t p = region_pos;
            while ( p + block_size <= region_end ) {
                nobjects++;
                p += block_size;
            }
        }

        ss->num_objects = nobjects;
        if ( nobjects > 0 ) {
            ss->object_info = (mcos_object_info_t *)calloc(nobjects, sizeof(mcos_object_info_t));
            if ( ss->object_info == NULL )
                return MATIO_E_OUT_OF_MEMORY;
            for ( i = 0; i < nobjects; i++ ) {
                mat_uint32_t vals[6];
                memcpy(vals, data + region_pos, 6 * 4);
                if ( byteswap ) {
                    size_t j;
                    for ( j = 0; j < 6; j++ )
                        Mat_uint32Swap(&vals[j]);
                }
                ss->object_info[i].class_id = vals[0];
                ss->object_info[i].saveobj_id = vals[3];
                ss->object_info[i].normalobj_id = vals[4];
                ss->object_info[i].dependency_id = vals[5];
                region_pos += block_size;
            }
        }
    }

    /* Region 2: saveobj properties (offset index 1)
     * Region 4: normal properties (offset index 3)
     * Both have same format: blocks padded to 8-byte boundary
     * Each block: nprops(int32), then nprops sub-blocks of (field_name_idx, field_type, field_value)
     */
    {
        int region_idx;
        for ( region_idx = 0; region_idx < 2; region_idx++ ) {
            size_t off_start_idx = (region_idx == 0) ? 1 : 3;
            size_t off_end_idx = (region_idx == 0) ? 2 : 4;
            size_t region_start = offsets[off_start_idx];
            size_t region_end_val =
                (off_end_idx < 8 && offsets[off_end_idx] <= len) ? offsets[off_end_idx] : len;
            size_t region_pos2;
            size_t count = 0;
            size_t capacity = 0;
            mcos_property_set_t *prop_sets = NULL;

            if ( region_start >= len || region_start >= region_end_val )
                continue;

            region_pos2 = region_start;

            /* Skip initial zero block (2 int32s = 8 bytes) */
            if ( region_pos2 + 8 <= region_end_val )
                region_pos2 += 8;

            while ( region_pos2 + 4 <= region_end_val ) {
                mat_uint32_t nprops;
                size_t j;

                memcpy(&nprops, data + region_pos2, 4);
                if ( byteswap )
                    Mat_uint32Swap(&nprops);
                region_pos2 += 4;

                /* Grow array */
                if ( count >= capacity ) {
                    size_t new_cap = (capacity == 0) ? 8 : capacity * 2;
                    mcos_property_set_t *tmp =
                        (mcos_property_set_t *)realloc(prop_sets, new_cap * sizeof(*prop_sets));
                    if ( tmp == NULL ) {
                        size_t k;
                        for ( k = 0; k < count; k++ )
                            FreePropertySet(&prop_sets[k]);
                        free(prop_sets);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                    prop_sets = tmp;
                    capacity = new_cap;
                }

                prop_sets[count].nprops = nprops;
                if ( nprops > 0 ) {
                    prop_sets[count].props =
                        (mcos_property_t *)calloc(nprops, sizeof(mcos_property_t));
                    if ( prop_sets[count].props == NULL ) {
                        size_t k;
                        for ( k = 0; k < count; k++ )
                            FreePropertySet(&prop_sets[k]);
                        free(prop_sets);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                } else {
                    prop_sets[count].props = NULL;
                }

                for ( j = 0; j < nprops; j++ ) {
                    mat_uint32_t name_idx, ftype, fvalue;
                    if ( region_pos2 + 12 > region_end_val )
                        break;
                    memcpy(&name_idx, data + region_pos2, 4);
                    memcpy(&ftype, data + region_pos2 + 4, 4);
                    memcpy(&fvalue, data + region_pos2 + 8, 4);
                    if ( byteswap ) {
                        Mat_uint32Swap(&name_idx);
                        Mat_uint32Swap(&ftype);
                        Mat_uint32Swap(&fvalue);
                    }
                    region_pos2 += 12;

                    if ( name_idx > 0 && name_idx <= ss->num_strings )
                        prop_sets[count].props[j].name = strdup(ss->strings[name_idx - 1]);
                    else
                        prop_sets[count].props[j].name = strdup("");
                    if ( prop_sets[count].props[j].name == NULL ) {
                        size_t k;
                        for ( k = 0; k <= count; k++ )
                            FreePropertySet(&prop_sets[k]);
                        free(prop_sets);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                    prop_sets[count].props[j].field_type = (int)ftype;
                    prop_sets[count].props[j].value = fvalue;
                }

                count++;

                /* Align to 8-byte boundary */
                if ( region_pos2 % 8 != 0 )
                    region_pos2 += 8 - (region_pos2 % 8);
            }

            if ( region_idx == 0 ) {
                ss->saveobj_props = prop_sets;
                ss->num_saveobj_props = count;
            } else {
                ss->normal_props = prop_sets;
                ss->num_normal_props = count;
            }
        }
    }

    /* Region 5: dynamic property metadata (offset index 4)
     * Each block: nprops, prop_id_1, ..., prop_id_n, padded to 8-byte boundary.
     * First block is all zeros.
     */
    if ( offsets[4] < len ) {
        size_t region_end = (offsets[5] <= len) ? offsets[5] : len;
        size_t region_pos = offsets[4];
        size_t count = 0;
        mcos_dynamic_property_set_t *dynamic_sets = NULL;

        if ( region_pos < region_end ) {
            size_t capacity = 0;
            if ( region_pos + 8 <= region_end )
                region_pos += 8;

            while ( region_pos + 4 <= region_end ) {
                mat_uint32_t nprops;
                size_t j;

                memcpy(&nprops, data + region_pos, 4);
                if ( byteswap )
                    Mat_uint32Swap(&nprops);
                region_pos += 4;

                if ( count >= capacity ) {
                    size_t new_cap = (capacity == 0) ? 8 : capacity * 2;
                    mcos_dynamic_property_set_t *tmp = (mcos_dynamic_property_set_t *)realloc(
                        dynamic_sets, new_cap * sizeof(*dynamic_sets));
                    if ( tmp == NULL ) {
                        size_t k;
                        for ( k = 0; k < count; k++ )
                            FreeDynamicPropertySet(&dynamic_sets[k]);
                        free(dynamic_sets);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                    dynamic_sets = tmp;
                    capacity = new_cap;
                }

                dynamic_sets[count].nprop_ids = nprops;
                dynamic_sets[count].prop_ids = NULL;
                if ( nprops > 0 ) {
                    dynamic_sets[count].prop_ids =
                        (mat_uint32_t *)calloc(nprops, sizeof(mat_uint32_t));
                    if ( dynamic_sets[count].prop_ids == NULL ) {
                        size_t k;
                        for ( k = 0; k < count; k++ )
                            FreeDynamicPropertySet(&dynamic_sets[k]);
                        free(dynamic_sets);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                }

                for ( j = 0; j < nprops; j++ ) {
                    if ( region_pos + 4 > region_end )
                        break;
                    memcpy(&dynamic_sets[count].prop_ids[j], data + region_pos, 4);
                    if ( byteswap )
                        Mat_uint32Swap(&dynamic_sets[count].prop_ids[j]);
                    region_pos += 4;
                }

                count++;

                if ( region_pos % 8 != 0 )
                    region_pos += 8 - (region_pos % 8);
            }
        }

        ss->dynamic_props = dynamic_sets;
        ss->num_dynamic_props = count;
    }

    return MATIO_E_NO_ERROR;
}

static int
ParseClassAliases(const matvar_t *alias_cell, mcos_subsystem_t *ss)
{
    size_t naliases = 0;
    size_t i;

    if ( alias_cell == NULL || ss == NULL || ss->num_classes == 0 || alias_cell->data == NULL )
        return MATIO_E_NO_ERROR;

    if ( alias_cell->data_type != MAT_T_INT32 && alias_cell->data_type != MAT_T_UINT32 )
        return MATIO_E_NO_ERROR;

    if ( Mat_MulDims(alias_cell, &naliases) )
        return MATIO_E_FILE_FORMAT_VIOLATION;

    ss->class_aliases = (char **)calloc(ss->num_classes, sizeof(char *));
    if ( ss->class_aliases == NULL )
        return MATIO_E_OUT_OF_MEMORY;
    ss->num_class_aliases = ss->num_classes;

    for ( i = 1; i < naliases && i <= ss->num_classes; i++ ) {
        mat_uint32_t alias_idx;
        if ( alias_cell->data_type == MAT_T_INT32 )
            alias_idx = (mat_uint32_t)((const mat_int32_t *)alias_cell->data)[i];
        else
            alias_idx = ((const mat_uint32_t *)alias_cell->data)[i];

        if ( alias_idx > 0 && alias_idx <= ss->num_strings )
            ss->class_aliases[i - 1] = strdup(ss->strings[alias_idx - 1]);
        if ( alias_idx > 0 && ss->class_aliases[i - 1] == NULL )
            return MATIO_E_OUT_OF_MEMORY;
    }

    return MATIO_E_NO_ERROR;
}

static const mcos_property_set_t *
GetPropertySetForObject(const mcos_subsystem_t *ss, const mcos_object_info_t *obj_info)
{
    if ( ss == NULL || obj_info == NULL )
        return NULL;

    if ( obj_info->saveobj_id > 0 && obj_info->saveobj_id <= ss->num_saveobj_props )
        return &ss->saveobj_props[obj_info->saveobj_id - 1];

    if ( obj_info->normalobj_id > 0 && obj_info->normalobj_id <= ss->num_normal_props )
        return &ss->normal_props[obj_info->normalobj_id - 1];

    return NULL;
}

static matvar_t *
GetDefaultPropsForClass(const mcos_subsystem_t *ss, mat_uint32_t class_id)
{
    matvar_t **default_cells = NULL;
    size_t ndefaults = 0;

    if ( ss == NULL || ss->defaults == NULL || ss->defaults->class_type != MAT_C_CELL )
        return NULL;

    if ( Mat_MulDims(ss->defaults, &ndefaults) )
        return NULL;

    default_cells = (matvar_t **)ss->defaults->data;
    if ( default_cells == NULL || class_id >= ndefaults )
        return NULL;

    return default_cells[class_id];
}

static int
AddUniqueFieldName(char ***all_fieldnames, size_t *count, size_t *capacity, const char *fname)
{
    size_t i;

    if ( all_fieldnames == NULL || count == NULL || capacity == NULL || fname == NULL )
        return MATIO_E_BAD_ARGUMENT;

    for ( i = 0; i < *count; i++ ) {
        if ( strcmp((*all_fieldnames)[i], fname) == 0 )
            return MATIO_E_NO_ERROR;
    }

    if ( *count >= *capacity ) {
        size_t new_cap = (*capacity == 0) ? 8 : (*capacity * 2);
        char **tmp = (char **)realloc(*all_fieldnames, new_cap * sizeof(char *));
        if ( tmp == NULL )
            return MATIO_E_OUT_OF_MEMORY;
        *all_fieldnames = tmp;
        *capacity = new_cap;
    }

    (*all_fieldnames)[*count] = strdup(fname);
    if ( (*all_fieldnames)[*count] == NULL )
        return MATIO_E_OUT_OF_MEMORY;
    (*count)++;

    return MATIO_E_NO_ERROR;
}

static int
CollectFieldNamesFromStruct(const matvar_t *matvar, char ***all_fieldnames, size_t *count,
                            size_t *capacity)
{
    size_t fi;

    if ( matvar == NULL || matvar->class_type != MAT_C_STRUCT || matvar->internal == NULL )
        return MATIO_E_NO_ERROR;

    for ( fi = 0; fi < matvar->internal->num_fields; fi++ ) {
        int err =
            AddUniqueFieldName(all_fieldnames, count, capacity, matvar->internal->fieldnames[fi]);
        if ( err )
            return err;
    }

    return MATIO_E_NO_ERROR;
}

static int
CollectFieldNamesFromObject(const mcos_subsystem_t *ss, mat_uint32_t obj_id, char ***all_fieldnames,
                            size_t *count, size_t *capacity, unsigned char *visited)
{
    const mcos_object_info_t *obj_info;
    const mcos_property_set_t *prop_set;
    const matvar_t *default_props;
    size_t pi;
    int err;

    if ( ss == NULL || obj_id < 1 || obj_id > ss->num_objects || visited == NULL )
        return MATIO_E_NO_ERROR;
    if ( visited[obj_id - 1] )
        return MATIO_E_NO_ERROR;
    visited[obj_id - 1] = 1;

    obj_info = &ss->object_info[obj_id - 1];
    prop_set = GetPropertySetForObject(ss, obj_info);
    if ( prop_set != NULL ) {
        for ( pi = 0; pi < prop_set->nprops; pi++ ) {
            err = AddUniqueFieldName(all_fieldnames, count, capacity, prop_set->props[pi].name);
            if ( err )
                return err;
        }
    }

    default_props = GetDefaultPropsForClass(ss, obj_info->class_id);
    err = CollectFieldNamesFromStruct(default_props, all_fieldnames, count, capacity);
    if ( err )
        return err;

    if ( obj_info->dependency_id > 0 && obj_info->dependency_id <= ss->num_dynamic_props ) {
        const mcos_dynamic_property_set_t *dyn_props =
            &ss->dynamic_props[obj_info->dependency_id - 1];
        for ( pi = 0; pi < dyn_props->nprop_ids; pi++ ) {
            err = CollectFieldNamesFromObject(ss, dyn_props->prop_ids[pi], all_fieldnames, count,
                                              capacity, visited);
            if ( err )
                return err;
        }
    }

    return MATIO_E_NO_ERROR;
}

static matvar_t *
CreateFieldValueFromProperty(const mcos_subsystem_t *ss, const char *fname,
                             const mcos_property_t *prop)
{
    matvar_t *field_val = NULL;

    if ( ss == NULL || fname == NULL || prop == NULL )
        return NULL;

    if ( prop->field_type == 1 ) {
        mat_uint32_t cell_idx = prop->value;
        if ( cell_idx < ss->num_prop_cells && ss->prop_cells[cell_idx] != NULL )
            field_val = Mat_VarDuplicate(ss->prop_cells[cell_idx], 1);
    } else if ( prop->field_type == 0 ) {
        mat_uint32_t str_idx = prop->value;
        if ( str_idx > 0 && str_idx <= ss->num_strings ) {
            const char *str_val = ss->strings[str_idx - 1];
            size_t slen = strlen(str_val);
            const size_t str_dims[2] = {1, slen};
            field_val = Mat_VarCreate(fname, MAT_C_CHAR, MAT_T_UINT8, 2, str_dims,
                                      (const void *)str_val, 0);
        }
    } else if ( prop->field_type == 2 ) {
        const size_t scalar_dims[2] = {1, 1};
        if ( prop->value <= 1 ) {
            mat_uint8_t bool_val = (prop->value != 0) ? 1 : 0;
            field_val =
                Mat_VarCreate(fname, MAT_C_UINT8, MAT_T_UINT8, 2, scalar_dims, &bool_val, 0);
            if ( field_val != NULL )
                field_val->isLogical = 1;
        } else {
            mat_uint32_t attr_val = prop->value;
            field_val =
                Mat_VarCreate(fname, MAT_C_UINT32, MAT_T_UINT32, 2, scalar_dims, &attr_val, 0);
        }
    }

    return field_val;
}

static matvar_t *
GetFieldValueFromObject(const mcos_subsystem_t *ss, mat_uint32_t obj_id, const char *fname,
                        unsigned char *visited)
{
    const mcos_object_info_t *obj_info;
    const mcos_property_set_t *prop_set;
    const matvar_t *default_props;
    size_t pi;

    if ( ss == NULL || fname == NULL || visited == NULL || obj_id < 1 || obj_id > ss->num_objects )
        return NULL;
    if ( visited[obj_id - 1] )
        return NULL;
    visited[obj_id - 1] = 1;

    obj_info = &ss->object_info[obj_id - 1];
    prop_set = GetPropertySetForObject(ss, obj_info);
    if ( prop_set != NULL ) {
        for ( pi = 0; pi < prop_set->nprops; pi++ ) {
            if ( strcmp(prop_set->props[pi].name, fname) == 0 )
                return CreateFieldValueFromProperty(ss, fname, &prop_set->props[pi]);
        }
    }

    if ( obj_info->dependency_id > 0 && obj_info->dependency_id <= ss->num_dynamic_props ) {
        const mcos_dynamic_property_set_t *dyn_props =
            &ss->dynamic_props[obj_info->dependency_id - 1];
        for ( pi = 0; pi < dyn_props->nprop_ids; pi++ ) {
            matvar_t *field_val =
                GetFieldValueFromObject(ss, dyn_props->prop_ids[pi], fname, visited);
            if ( field_val != NULL )
                return field_val;
        }
    }

    default_props = GetDefaultPropsForClass(ss, obj_info->class_id);
    if ( default_props != NULL && default_props->class_type == MAT_C_STRUCT ) {
        const matvar_t *def_field = Mat_VarGetStructFieldByName(default_props, fname, 0);
        if ( def_field != NULL )
            return Mat_VarDuplicate(def_field, 1);
    }

    return NULL;
}

/** @brief Parse the subsystem data from a v5 MAT file
 *
 * Reads the subsystem at the offset pointed to by the MAT file header,
 * parses it as a mini-MAT file, extracts the FileWrapper__ cell array,
 * and populates a mcos_subsystem_t structure.
 *
 * @param mat MAT file pointer
 * @return Pointer to newly allocated mcos_subsystem_t, or NULL on failure
 */
static mcos_subsystem_t *
ParseSubsystem5(mat_t *mat)
{
    mat_off_t subsys_pos;
    mat_off_t save_pos;
    mcos_subsystem_t *ss = NULL;
    matvar_t *subsys_var = NULL;
    matvar_t *struct_var = NULL;
    matvar_t *mcos_var = NULL;
    const mat_uint8_t *subsys_data = NULL;
    size_t subsys_len = 0;
    mat_t subsys_mat;
    int err;

    if ( mat == NULL || mat->fp == NULL || mat->subsys_offset == NULL )
        return NULL;

    /* Decode the 8-byte subsystem offset from the MAT header.
     * The offset is stored as an 8-byte value at header bytes 117-124.
     * If all spaces or all zeros, there is no subsystem.
     */
    {
        int all_spaces = 1, all_zeros = 1;
        int k;
        for ( k = 0; k < 8; k++ ) {
            if ( mat->subsys_offset[k] != ' ' )
                all_spaces = 0;
            if ( mat->subsys_offset[k] != 0 )
                all_zeros = 0;
        }
        if ( all_spaces || all_zeros )
            return NULL;
    }

    /* The offset is stored as a uint64 (or two uint32s) */
    {
        mat_uint32_t lo, hi;
        memcpy(&lo, mat->subsys_offset, 4);
        memcpy(&hi, mat->subsys_offset + 4, 4);
        if ( mat->byteswap ) {
            Mat_uint32Swap(&lo);
            Mat_uint32Swap(&hi);
        }
        subsys_pos = (mat_off_t)lo | ((mat_off_t)hi << 32);
    }

    if ( subsys_pos <= 0 )
        return NULL;

    save_pos = ftello((FILE *)mat->fp);
    if ( save_pos == -1L )
        return NULL;

    /* Seek to the subsystem data element */
    if ( fseeko((FILE *)mat->fp, subsys_pos, SEEK_SET) != 0 ) {
        (void)fseeko((FILE *)mat->fp, save_pos, SEEK_SET);
        return NULL;
    }

    /* Read the subsystem data element as a regular variable.
     * It appears as an unnamed mxUINT8 data element.
     */
    subsys_var = Mat_VarReadNextInfo5(mat);
    if ( subsys_var == NULL ) {
        (void)fseeko((FILE *)mat->fp, save_pos, SEEK_SET);
        return NULL;
    }

    err = Mat_VarRead5(mat, subsys_var);
    if ( err || subsys_var->data == NULL ) {
        Mat_VarFree(subsys_var);
        (void)fseeko((FILE *)mat->fp, save_pos, SEEK_SET);
        return NULL;
    }

    /* Restore position */
    (void)fseeko((FILE *)mat->fp, save_pos, SEEK_SET);

    /* The subsystem data is the raw bytes inside subsys_var->data */
    subsys_data = (mat_uint8_t *)subsys_var->data;
    subsys_len = subsys_var->nbytes;

    if ( subsys_len < 8 ) {
        Mat_VarFree(subsys_var);
        return NULL;
    }

    /* The subsystem data is formatted like a mini-MAT file.
     * It has an 8-byte header: version(2) + endian(2) + 4 bytes padding.
     * Then data elements follow.
     * We need to parse these data elements to find the struct with MCOS field.
     *
     * We create a temporary mat_t that reads from a memory buffer.
     * Since our existing API reads from FILE*, we write the subsystem data
     * to a temporary file and re-read it.
     */
    {
        FILE *tmp_fp;
#if defined(_WIN32)
        char tmp_name[L_tmpnam + 1];
        if ( tmpnam(tmp_name) == NULL ) {
            Mat_VarFree(subsys_var);
            return NULL;
        }
        tmp_fp = fopen(tmp_name, "w+b");
#else
        tmp_fp = tmpfile();
#endif
        if ( tmp_fp == NULL ) {
            Mat_VarFree(subsys_var);
#if defined(_WIN32)
            (void)remove(tmp_name);
#endif
            return NULL;
        }

        /* Write subsystem data after the 8-byte mini header */
        if ( subsys_len > 8 ) {
            /* Write a full 128-byte MAT5 header so Mat_VarReadNextInfo5 works */
            {
                char hdr[128];
                memset(hdr, ' ', 128);
                /* Copy version and endian from the mini header */
                /* The mini header is: version(2 bytes) + endian(2 bytes) + 4 padding */
                /* Map to a full header: 116 text + 8 subsys_offset + 2 version + 2 endian */
                memcpy(hdr + 124, subsys_data, 2);     /* version */
                memcpy(hdr + 126, subsys_data + 2, 2); /* endian */
                fwrite(hdr, 1, 128, tmp_fp);
            }
            /* Write the data elements (everything after the 8-byte mini header) */
            fwrite(subsys_data + 8, 1, subsys_len - 8, tmp_fp);
            fflush(tmp_fp);
            rewind(tmp_fp);
        } else {
            fclose(tmp_fp);
#if defined(_WIN32)
            (void)remove(tmp_name);
#endif
            Mat_VarFree(subsys_var);
            return NULL;
        }

        /* Set up a temporary mat_t to read from the temp file */
        memset(&subsys_mat, 0, sizeof(subsys_mat));
        subsys_mat.fp = tmp_fp;
        subsys_mat.version = MAT_FT_MAT5;
        subsys_mat.byteswap = mat->byteswap;
        subsys_mat.bof = 128;
        subsys_mat.next_index = 0;
        subsys_mat.header = NULL;
        subsys_mat.subsys_offset = NULL;
        subsys_mat.filename = NULL;
        subsys_mat.dir = NULL;
        subsys_mat.num_datasets = 0;
#if defined(MAT73) && MAT73
        subsys_mat.refs_id = -1;
#endif
        subsys_mat.mcos = NULL;

        /* Seek past the 128-byte header */
        (void)fseeko(tmp_fp, 128, SEEK_SET);

        /* Read the first data element: should be a struct */
        struct_var = Mat_VarReadNextInfo5(&subsys_mat);
        if ( struct_var != NULL ) {
            err = Mat_VarRead5(&subsys_mat, struct_var);
            if ( err ) {
                Mat_VarFree(struct_var);
                struct_var = NULL;
            }
        }

        fclose(tmp_fp);
#if defined(_WIN32)
        (void)remove(tmp_name);
#endif
    }

    Mat_VarFree(subsys_var);

    if ( struct_var == NULL || struct_var->class_type != MAT_C_STRUCT )
        goto cleanup;

    /* Find the "MCOS" field in the struct */
    mcos_var = Mat_VarGetStructFieldByName(struct_var, "MCOS", 0);
    if ( mcos_var == NULL )
        goto cleanup;

    /* The MCOS field should be of class MAT_C_CELL (the FileWrapper__ cell array).
     * Note: In the MAT file the field is stored as MAT_C_OPAQUE, but our
     * ReadNextStructField converts it to MAT_C_CELL when the opaque payload
     * is a cell array.
     */
    if ( mcos_var->class_type != MAT_C_CELL )
        goto cleanup;

    /* Allocate the subsystem structure */
    ss = (mcos_subsystem_t *)calloc(1, sizeof(mcos_subsystem_t));
    if ( ss == NULL )
        goto cleanup;

    /* Parse the FileWrapper__ cell array */
    {
        matvar_t **cells = (matvar_t **)mcos_var->data;
        size_t ncells = 1;
        size_t trailing_shared_cells;
        size_t total_prop_cells;
        const matvar_t *cell1;

        if ( cells == NULL )
            goto cleanup_ss;

        {
            int merr = Mat_MulDims(mcos_var, &ncells);
            if ( merr )
                goto cleanup_ss;
        }

        /* Cell 1 (index 0): Linking metadata as uint8 array */
        cell1 = cells[0];
        if ( cell1 == NULL || cell1->data == NULL )
            goto cleanup_ss;

        err = ParseLinkingMetadata((const mat_uint8_t *)cell1->data, cell1->nbytes, ss,
                                   mat->byteswap);
        if ( err )
            goto cleanup_ss;

        if ( ss->file_wrapper_version >= 4 ) {
            if ( ncells < 5 )
                goto cleanup_ss;
            trailing_shared_cells = 3;
        } else {
            if ( ncells < 3 )
                goto cleanup_ss;
            trailing_shared_cells = 1;
        }

        /* Cell 2 (index 1): Empty/reserved — skip */

        /* Cells 3.. contain property content cells. FileWrapper__ v4 adds
         * three trailing shared cells: unknown, class alias metadata, defaults.
         */
        if ( ncells > 2 + trailing_shared_cells ) {
            total_prop_cells = ncells - 2 - trailing_shared_cells;
        } else {
            total_prop_cells = 0;
        }

        if ( total_prop_cells > 0 ) {
            size_t ci;
            ss->prop_cells = (matvar_t **)calloc(total_prop_cells, sizeof(matvar_t *));
            if ( ss->prop_cells == NULL )
                goto cleanup_ss;
            ss->num_prop_cells = total_prop_cells;
            for ( ci = 0; ci < total_prop_cells; ci++ ) {
                /* Duplicate cells so they persist after struct_var is freed */
                if ( cells[2 + ci] != NULL )
                    ss->prop_cells[ci] = Mat_VarDuplicate(cells[2 + ci], 1);
            }
        }

        if ( trailing_shared_cells == 3 ) {
            err = ParseClassAliases(cells[ncells - 2], ss);
            if ( err )
                goto cleanup_ss;
        }

        /* Last cell (index ncells-1): Default class properties */
        if ( cells[ncells - 1] != NULL ) {
            ss->defaults = Mat_VarDuplicate(cells[ncells - 1], 1);
        }
    }

    Mat_VarFree(struct_var);
    return ss;

cleanup_ss:
    Mat_MCOS_Free(ss);
    ss = NULL;
cleanup:
    Mat_VarFree(struct_var);
    return NULL;
}

/** @brief Get or lazily parse the MCOS subsystem for a v5 MAT file
 *
 * @param mat MAT file pointer
 * @return Pointer to the MCOS subsystem, or NULL if unavailable
 */
static mcos_subsystem_t *
GetSubsystem5(mat_t *mat)
{
    if ( mat == NULL )
        return NULL;
    if ( mat->mcos != NULL )
        return (mcos_subsystem_t *)mat->mcos;

    mat->mcos = ParseSubsystem5(mat);
    return (mcos_subsystem_t *)mat->mcos;
}

#if defined(MAT73) && MAT73
/** @brief Parse the subsystem data from a v7.3 (HDF5) MAT file
 *
 * Reads the #subsystem# group from the HDF5 file.  In v7.3 files,
 * the subsystem is stored as a struct group containing a "MCOS" dataset
 * of HDF5 object references.  Each reference points to one cell of the
 * FileWrapper__ cell array.  Mat_VarReadFromH5Ref is used to read each
 * cell and then process them the same way as ParseSubsystem5.
 *
 * @param mat MAT file pointer (version MAT_FT_MAT73)
 * @return Pointer to newly allocated mcos_subsystem_t, or NULL on failure
 */
static mcos_subsystem_t *
ParseSubsystem73(mat_t *mat)
{
    hid_t file_id;
    hid_t subsys_group_id = -1;
    hid_t mcos_dset_id = -1;
    mcos_subsystem_t *ss = NULL;
    matvar_t **cells = NULL;
    hsize_t ncells = 0;
    int err;

    if ( mat == NULL || mat->fp == NULL )
        return NULL;

    file_id = *(hid_t *)mat->fp;

    /* The subsystem lives in /#subsystem# as a struct group */
    if ( H5Lexists(file_id, "#subsystem#", H5P_DEFAULT) <= 0 )
        return NULL;

    H5E_BEGIN_TRY
    {
        subsys_group_id = H5Gopen(file_id, "#subsystem#", H5P_DEFAULT);
    }
    H5E_END_TRY

    if ( subsys_group_id < 0 )
        return NULL;

    /* Open the "MCOS" dataset inside the struct group */
    if ( H5Lexists(subsys_group_id, "MCOS", H5P_DEFAULT) <= 0 ) {
        H5Gclose(subsys_group_id);
        return NULL;
    }

    mcos_dset_id = H5Dopen(subsys_group_id, "MCOS", H5P_DEFAULT);
    if ( mcos_dset_id < 0 ) {
        H5Gclose(subsys_group_id);
        return NULL;
    }

    /* Read the object references from the MCOS dataset */
    {
        hid_t space_id = H5Dget_space(mcos_dset_id);
        if ( space_id >= 0 ) {
            ncells = (hsize_t)H5Sget_simple_extent_npoints(space_id);
            H5Sclose(space_id);
        }
    }

    if ( ncells < 3 ) {
        H5Dclose(mcos_dset_id);
        H5Gclose(subsys_group_id);
        return NULL;
    }

    {
        hobj_ref_t *refs = NULL;
        hsize_t ci;

        refs = (hobj_ref_t *)calloc((size_t)ncells, sizeof(hobj_ref_t));
        if ( refs == NULL ) {
            H5Dclose(mcos_dset_id);
            H5Gclose(subsys_group_id);
            return NULL;
        }

        if ( H5Dread(mcos_dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, refs) < 0 ) {
            free(refs);
            H5Dclose(mcos_dset_id);
            H5Gclose(subsys_group_id);
            return NULL;
        }

        cells = (matvar_t **)calloc((size_t)ncells, sizeof(matvar_t *));
        if ( cells == NULL ) {
            free(refs);
            H5Dclose(mcos_dset_id);
            H5Gclose(subsys_group_id);
            return NULL;
        }

        /* Read each referenced dataset as a matvar_t */
        for ( ci = 0; ci < ncells; ci++ ) {
            cells[ci] = Mat_VarReadFromH5Ref(mat, mcos_dset_id, refs[ci]);
        }

        free(refs);
    }

    H5Dclose(mcos_dset_id);
    H5Gclose(subsys_group_id);

    /* Process the cells the same way as ParseSubsystem5 */
    {
        size_t trailing_shared_cells;
        size_t total_prop_cells;
        const matvar_t *cell1;

        /* Cell 0: Linking metadata as uint8 array */
        cell1 = cells[0];
        if ( cell1 == NULL || cell1->data == NULL )
            goto cleanup;

        ss = (mcos_subsystem_t *)calloc(1, sizeof(mcos_subsystem_t));
        if ( ss == NULL )
            goto cleanup;

        err = ParseLinkingMetadata((const mat_uint8_t *)cell1->data, cell1->nbytes, ss,
                                   mat->byteswap);
        if ( err )
            goto cleanup_ss;

        if ( ss->file_wrapper_version >= 4 ) {
            if ( ncells < 5 )
                goto cleanup_ss;
            trailing_shared_cells = 3;
        } else {
            if ( ncells < 3 )
                goto cleanup_ss;
            trailing_shared_cells = 1;
        }

        /* Cell 1: Empty/reserved — skip */

        /* Cells 2..N: property content */
        if ( ncells > 2 + trailing_shared_cells )
            total_prop_cells = (size_t)ncells - 2 - trailing_shared_cells;
        else
            total_prop_cells = 0;

        if ( total_prop_cells > 0 ) {
            size_t ci;
            ss->prop_cells = (matvar_t **)calloc(total_prop_cells, sizeof(matvar_t *));
            if ( ss->prop_cells == NULL )
                goto cleanup_ss;
            ss->num_prop_cells = total_prop_cells;
            for ( ci = 0; ci < total_prop_cells; ci++ ) {
                if ( cells[2 + ci] != NULL )
                    ss->prop_cells[ci] = Mat_VarDuplicate(cells[2 + ci], 1);
            }
        }

        if ( trailing_shared_cells == 3 ) {
            err = ParseClassAliases(cells[ncells - 2], ss);
            if ( err )
                goto cleanup_ss;
        }

        if ( cells[ncells - 1] != NULL )
            ss->defaults = Mat_VarDuplicate(cells[ncells - 1], 1);
    }

    /* Free the cell matvar_ts */
    {
        hsize_t ci;
        for ( ci = 0; ci < ncells; ci++ )
            Mat_VarFree(cells[ci]);
        free(cells);
    }
    return ss;

cleanup_ss:
    Mat_MCOS_Free(ss);
    ss = NULL;
cleanup: {
    hsize_t ci;
    for ( ci = 0; ci < ncells; ci++ )
        Mat_VarFree(cells[ci]);
    free(cells);
}
    return NULL;
}

/** @brief Get or lazily parse the MCOS subsystem for a v7.3 MAT file */
static mcos_subsystem_t *
GetSubsystem73(mat_t *mat)
{
    if ( mat == NULL )
        return NULL;
    if ( mat->mcos != NULL )
        return (mcos_subsystem_t *)mat->mcos;

    mat->mcos = ParseSubsystem73(mat);
    return (mcos_subsystem_t *)mat->mcos;
}
#endif /* MAT73 */

/** @brief Resolve an MCOS opaque variable into a struct-like matvar_t (common logic)
 *
 * @param ss      Parsed subsystem
 * @param matvar  The opaque matvar_t to resolve
 * @return 0 on success
 */
static int
ResolveMCOS(mcos_subsystem_t *ss, matvar_t *matvar)
{
    const mcos_class_info_t *cls;
    size_t nfields = 0;
    size_t num_objs;
    size_t oi;
    char **all_fieldnames = NULL;
    size_t all_fieldnames_count = 0;
    size_t all_fieldnames_cap = 0;

    if ( ss == NULL || matvar == NULL || matvar->internal == NULL )
        return MATIO_E_BAD_ARGUMENT;

    num_objs = matvar->internal->num_objects;
    if ( num_objs == 0 || matvar->internal->object_ids == NULL ) {
        /* No object IDs — leave as opaque */
        return MATIO_E_NO_ERROR;
    }

    /* Validate class_id */
    if ( matvar->internal->class_id < 1 || matvar->internal->class_id > ss->num_classes ) {
        Mat_Warning("MCOS: Invalid class_id %u", matvar->internal->class_id);
        return MATIO_E_FILE_FORMAT_VIOLATION;
    }

    cls = &ss->class_info[matvar->internal->class_id - 1];

    /* Prefer the active class alias when it is present in FileWrapper__ v4+. */
    {
        const char *resolved_name = cls->name;
        if ( matvar->internal->class_id > 0 &&
             matvar->internal->class_id <= ss->num_class_aliases &&
             ss->class_aliases[matvar->internal->class_id - 1] != NULL ) {
            resolved_name = ss->class_aliases[matvar->internal->class_id - 1];
        }
        if ( resolved_name != NULL && (matvar->internal->class_name == NULL ||
                                       strcmp(matvar->internal->class_name, resolved_name) != 0) ) {
            free(matvar->internal->class_name);
            matvar->internal->class_name = strdup(resolved_name);
            if ( matvar->internal->class_name == NULL )
                return MATIO_E_OUT_OF_MEMORY;
        }
    }

    /* Collect all unique field names across the object graph, including
     * class defaults and dynamic-property references.
     */
    for ( oi = 0; oi < num_objs; oi++ ) {
        mat_uint32_t obj_id = matvar->internal->object_ids[oi];
        unsigned char *visited = (unsigned char *)calloc(ss->num_objects, sizeof(unsigned char));
        int err;

        if ( visited == NULL ) {
            size_t kk;
            for ( kk = 0; kk < all_fieldnames_count; kk++ )
                free(all_fieldnames[kk]);
            free(all_fieldnames);
            return MATIO_E_OUT_OF_MEMORY;
        }

        err = CollectFieldNamesFromObject(ss, obj_id, &all_fieldnames, &all_fieldnames_count,
                                          &all_fieldnames_cap, visited);
        free(visited);
        if ( err ) {
            size_t kk;
            for ( kk = 0; kk < all_fieldnames_count; kk++ )
                free(all_fieldnames[kk]);
            free(all_fieldnames);
            return err;
        }
    }

    nfields = all_fieldnames_count;
    if ( nfields == 0 ) {
        free(all_fieldnames);
        /* Object with no properties — set it up as an empty struct-like object */
        matvar->class_type = MAT_C_OBJECT;
        matvar->data_type = MAT_T_STRUCT;
        matvar->data_size = sizeof(matvar_t *);
        matvar->nbytes = 0;
        matvar->data = NULL;
        matvar->internal->num_fields = 0;
        matvar->internal->fieldnames = NULL;
        return MATIO_E_NO_ERROR;
    }

    /* Set up the matvar as a struct-like object */
    matvar->class_type = MAT_C_OBJECT;
    matvar->data_type = MAT_T_STRUCT;
    matvar->data_size = sizeof(matvar_t *);

    matvar->internal->num_fields = (unsigned)nfields;
    matvar->internal->fieldnames = all_fieldnames;

    /* Allocate field data: nfields * num_objs matvar_t pointers */
    {
        size_t total;
        int merr = Mul(&total, nfields, num_objs);
        if ( merr ) {
            return MATIO_E_OUT_OF_MEMORY;
        }
        matvar->data = calloc(total, sizeof(matvar_t *));
        if ( matvar->data == NULL )
            return MATIO_E_OUT_OF_MEMORY;
        matvar->nbytes = (size_t)(total * sizeof(matvar_t *));
    }

    /* Populate each object's fields */
    {
        matvar_t **fields = (matvar_t **)matvar->data;
        for ( oi = 0; oi < num_objs; oi++ ) {
            mat_uint32_t obj_id = matvar->internal->object_ids[oi];
            size_t fi;

            if ( obj_id < 1 || obj_id > ss->num_objects )
                continue;

            for ( fi = 0; fi < nfields; fi++ ) {
                matvar_t *field_val = NULL;
                const char *fname = all_fieldnames[fi];
                unsigned char *visited =
                    (unsigned char *)calloc(ss->num_objects, sizeof(unsigned char));

                if ( visited == NULL )
                    return MATIO_E_OUT_OF_MEMORY;

                field_val = GetFieldValueFromObject(ss, obj_id, fname, visited);
                free(visited);

                /* If still no value, create an empty variable */
                if ( field_val == NULL ) {
                    const size_t empty_dims[2] = {0, 0};
                    field_val =
                        Mat_VarCreate(fname, MAT_C_EMPTY, MAT_T_UNKNOWN, 2, empty_dims, NULL, 0);
                }

                if ( field_val != NULL ) {
                    /* Set field name */
                    free(field_val->name);
                    field_val->name = strdup(fname);
                    if ( field_val->name == NULL ) {
                        Mat_VarFree(field_val);
                        return MATIO_E_OUT_OF_MEMORY;
                    }
                }

                fields[oi * nfields + fi] = field_val;
            }
        }
    }

    return MATIO_E_NO_ERROR;
}

/** @brief Maximum recursion depth for nested MCOS resolution */
#define MCOS_MAX_DEPTH 16

/* Forward declarations for mutual recursion */
static int ResolveNestedMCOS(mcos_subsystem_t *ss, matvar_t *matvar, int depth);

/** @brief Check if a matvar_t is an MCOS-encoded uint32 reference
 *
 * @param matvar  The variable to check
 * @return 1 if MCOS-encoded, 0 otherwise
 */
static int
IsMCOSEncoded(const matvar_t *matvar)
{
    if ( matvar != NULL && matvar->class_type == MAT_C_UINT32 && matvar->data != NULL ) {
        const mat_uint32_t *u32 = (const mat_uint32_t *)matvar->data;
        size_t n = 1;
        int r;
        for ( r = 0; r < matvar->rank; r++ )
            n *= matvar->dims[r];
        if ( n >= 4 && u32[0] == MCOS_REF_VALUE )
            return 1;
    }
    return 0;
}

/** @brief Parse MCOS reference metadata from a uint32 array and resolve in-place
 *
 * Converts a MAT_C_UINT32 variable with the 0xDD000000 magic marker
 * in-place to a resolved MAT_C_OBJECT.
 *
 * @param ss      Parsed subsystem
 * @param matvar  The uint32 variable to resolve
 * @param depth   Current recursion depth (for safety)
 * @return 0 on success
 */
static int
ResolveEncodedMCOS(mcos_subsystem_t *ss, matvar_t *matvar, int depth)
{
    const mat_uint32_t *meta;
    size_t n = 1;
    mat_uint32_t ndims;
    mat_uint32_t dim_start = 2;
    size_t total_objs = 1;
    mat_uint32_t obj_start;
    int r;
    int err;

    if ( ss == NULL || matvar == NULL || matvar->internal == NULL || depth > MCOS_MAX_DEPTH )
        return MATIO_E_NO_ERROR;

    if ( !IsMCOSEncoded(matvar) )
        return MATIO_E_NO_ERROR;

    meta = (const mat_uint32_t *)matvar->data;
    for ( r = 0; r < matvar->rank; r++ )
        n *= matvar->dims[r];

    ndims = meta[1];
    if ( dim_start + ndims + 1 > n )
        return MATIO_E_NO_ERROR; /* Not enough data */

    /* Compute total objects from encoded dimensions */
    total_objs = 1;
    for ( r = 0; r < (int)ndims; r++ )
        total_objs *= meta[dim_start + r];

    obj_start = dim_start + ndims;
    if ( obj_start + total_objs >= n )
        return MATIO_E_NO_ERROR; /* Not enough data for object_ids + class_id */

    /* Set up internal MCOS metadata */
    matvar->internal->num_objects = total_objs;
    matvar->internal->object_ids = (mat_uint32_t *)malloc(total_objs * sizeof(mat_uint32_t));
    if ( matvar->internal->object_ids == NULL )
        return MATIO_E_OUT_OF_MEMORY;

    {
        size_t k;
        for ( k = 0; k < total_objs; k++ )
            matvar->internal->object_ids[k] = meta[obj_start + k];
    }
    matvar->internal->class_id = meta[obj_start + total_objs];

    /* Set type_name to "MCOS" */
    free(matvar->internal->type_name);
    matvar->internal->type_name = strdup("MCOS");
    if ( matvar->internal->type_name == NULL )
        return MATIO_E_OUT_OF_MEMORY;

    /* Update dims to match the MCOS object dimensions */
    free(matvar->dims);
    matvar->dims = (size_t *)malloc(ndims * sizeof(size_t));
    if ( matvar->dims == NULL )
        return MATIO_E_OUT_OF_MEMORY;
    matvar->rank = (int)ndims;
    for ( r = 0; r < (int)ndims; r++ )
        matvar->dims[r] = meta[dim_start + r];

    /* Free the old uint32 data */
    free(matvar->data);
    matvar->data = NULL;
    matvar->nbytes = 0;
    matvar->data_type = MAT_T_UNKNOWN;
    matvar->class_type = MAT_C_OPAQUE;

    /* Resolve the MCOS object */
    err = ResolveMCOS(ss, matvar);
    if ( err )
        return err;

    /* Recursively resolve nested MCOS references in the resolved object */
    return ResolveNestedMCOS(ss, matvar, depth);
}

/** @brief Recursively resolve MCOS references in a variable's children
 *
 * For cell arrays, checks each element. For resolved objects, checks each field.
 *
 * @param ss      Parsed subsystem
 * @param matvar  Variable to scan
 * @param depth   Current recursion depth
 * @return 0 on success
 */
static int
ResolveNestedMCOS(mcos_subsystem_t *ss, matvar_t *matvar, int depth)
{
    if ( ss == NULL || matvar == NULL || depth > MCOS_MAX_DEPTH )
        return MATIO_E_NO_ERROR;

    if ( matvar->class_type == MAT_C_CELL && matvar->data != NULL ) {
        matvar_t **cells = (matvar_t **)matvar->data;
        size_t ncells = 1;
        size_t i;
        int r;

        for ( r = 0; r < matvar->rank; r++ )
            ncells *= matvar->dims[r];

        for ( i = 0; i < ncells; i++ ) {
            if ( cells[i] == NULL )
                continue;
            if ( IsMCOSEncoded(cells[i]) ) {
                ResolveEncodedMCOS(ss, cells[i], depth + 1);
            } else {
                ResolveNestedMCOS(ss, cells[i], depth + 1);
            }
        }
    } else if ( (matvar->class_type == MAT_C_OBJECT || matvar->class_type == MAT_C_STRUCT) &&
                matvar->data != NULL && matvar->internal != NULL ) {
        size_t nfields = matvar->internal->num_fields;
        size_t nelems = 1;
        size_t total;
        size_t i;
        int r;
        matvar_t **fields = (matvar_t **)matvar->data;

        for ( r = 0; r < matvar->rank; r++ )
            nelems *= matvar->dims[r];

        total = nfields * nelems;
        for ( i = 0; i < total; i++ ) {
            if ( fields[i] == NULL )
                continue;
            if ( IsMCOSEncoded(fields[i]) ) {
                ResolveEncodedMCOS(ss, fields[i], depth + 1);
            } else {
                ResolveNestedMCOS(ss, fields[i], depth + 1);
            }
        }
    }

    return MATIO_E_NO_ERROR;
}

/** @brief Resolve an MCOS opaque variable from a v5 MAT file */
int
Mat_MCOS_Read5(mat_t *mat, matvar_t *matvar)
{
    mcos_subsystem_t *ss;

    if ( mat == NULL || matvar == NULL || matvar->internal == NULL )
        return MATIO_E_BAD_ARGUMENT;

    /* Only handle MCOS type system */
    if ( matvar->internal->type_name == NULL || strcmp(matvar->internal->type_name, "MCOS") != 0 )
        return MATIO_E_NO_ERROR;

    ss = GetSubsystem5(mat);
    if ( ss == NULL )
        return MATIO_E_NO_ERROR;

    {
        int err = ResolveMCOS(ss, matvar);
        if ( err )
            return err;
    }

    return ResolveNestedMCOS(ss, matvar, 0);
}

#if defined(MAT73) && MAT73
/** @brief Resolve an MCOS opaque variable from a v7.3 (HDF5) MAT file */
int
Mat_MCOS_Read73(mat_t *mat, matvar_t *matvar)
{
    mcos_subsystem_t *ss;

    if ( mat == NULL || matvar == NULL || matvar->internal == NULL )
        return MATIO_E_BAD_ARGUMENT;

    /* Only handle MCOS type system */
    if ( matvar->internal->type_name == NULL || strcmp(matvar->internal->type_name, "MCOS") != 0 )
        return MATIO_E_NO_ERROR;

    ss = GetSubsystem73(mat);
    if ( ss == NULL )
        return MATIO_E_NO_ERROR;

    {
        int err = ResolveMCOS(ss, matvar);
        if ( err )
            return err;
    }

    return ResolveNestedMCOS(ss, matvar, 0);
}
#endif

#endif /* MCOS */
