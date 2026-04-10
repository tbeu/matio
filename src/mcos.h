/*
 * Copyright (c) 2015-2026, The matio contributors
 * Copyright (c) 2005-2014, Christopher C. Hulbert
 * All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef MCOS_H
#define MCOS_H

#include "matio_private.h"

#if defined(MCOS) && MCOS

/** @brief MCOS class information
 *
 * Stores class name and namespace for each class_id
 */
typedef struct mcos_class_info
{
    char *name;       /**< Class name (e.g. "SimpleClass", "table") */
    char *namespace_; /**< Namespace (e.g. "" for no namespace) */
} mcos_class_info_t;

/** @brief MCOS object information
 *
 * Stores the mapping from object_id to class and property locations
 */
typedef struct mcos_object_info
{
    mat_uint32_t class_id;      /**< Index into class_info array */
    mat_uint32_t saveobj_id;    /**< Index into saveobj (Region 2) properties, 0=unused */
    mat_uint32_t normalobj_id;  /**< Index into normal (Region 4) properties, 0=unused */
    mat_uint32_t dependency_id; /**< Object dependency chain ID */
} mcos_object_info_t;

/** @brief MCOS object property entry
 *
 * One property field for an object
 */
typedef struct mcos_property
{
    char *name;         /**< Property name */
    int field_type;     /**< 0=enum member, 1=property, 2=attribute */
    mat_uint32_t value; /**< Index into cells (type 1) or literal value (type 2) */
} mcos_property_t;

/** @brief MCOS property set for one object
 *
 * Collection of properties for a single object
 */
typedef struct mcos_property_set
{
    mcos_property_t *props; /**< Array of properties */
    size_t nprops;          /**< Number of properties */
} mcos_property_set_t;

/** @brief Dynamic property object IDs for one dependency chain
 *
 * Maps a dependency_id to the referenced dynamic property object IDs.
 */
typedef struct mcos_dynamic_property_set
{
    mat_uint32_t *prop_ids; /**< Array of object IDs for dynamic properties */
    size_t nprop_ids;       /**< Number of dynamic property object IDs */
} mcos_dynamic_property_set_t;

/** @brief Parsed MCOS subsystem data
 *
 * Contains all parsed information from the MAT file subsystem
 */
typedef struct mcos_subsystem
{
    mat_uint32_t file_wrapper_version; /**< FileWrapper__ metadata version */

    /* String table */
    char **strings;     /**< Array of null-terminated strings from metadata */
    size_t num_strings; /**< Number of strings */

    /* Class information (Region 1) */
    mcos_class_info_t *class_info; /**< Array of class info, indexed by class_id-1 */
    size_t num_classes;            /**< Number of classes */

    /* Object information (Region 3) */
    mcos_object_info_t *object_info; /**< Array of object info, indexed by object_id-1 */
    size_t num_objects;              /**< Number of objects */

    /* Property sets from Region 2 (saveobj) */
    mcos_property_set_t *saveobj_props; /**< Array indexed by saveobj_id-1 */
    size_t num_saveobj_props;

    /* Property sets from Region 4 (normal) */
    mcos_property_set_t *normal_props; /**< Array indexed by normalobj_id-1 */
    size_t num_normal_props;

    /* Region 5 (dynamic properties) */
    mcos_dynamic_property_set_t *dynamic_props; /**< Array indexed by dependency_id-1 */
    size_t num_dynamic_props;

    /* Property content cells (Cell 3..N+2 of FileWrapper__) */
    matvar_t **prop_cells; /**< Array of property value matvar_t */
    size_t num_prop_cells; /**< Number of property cells */

    /* Class alias metadata (Cell[-2] in FileWrapper__ v4+) */
    char **class_aliases;     /**< Array indexed by class_id-1 */
    size_t num_class_aliases; /**< Number of class alias entries */

    /* Default class properties (last cell of FileWrapper__) */
    matvar_t *defaults; /**< Cell array of structs, one per class */
} mcos_subsystem_t;

#endif /* MCOS */

#endif /* MCOS_H */
