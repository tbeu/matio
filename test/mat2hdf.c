/** @file mat2hdf.c
 * @brief Converts a Matlab MAT file to an HDF file
 *
 * Reads in each variable in a Matlab MAT file and writes them to an
 * HDF file.  By default, uses smart detection of Attributes/Datasets.
 * Any Numeric array with more than 2 dimensions, or with 2 dimensions
 * and more than 20 elements are written as a Dataset.  Strings and Numeric
 * Arrays and scalars not falling under the Dataset conditions are written as
 * Attributes.  MAT Structures are written under an HDF Group with the name of
 * the structure.  Structures can be nested.  Cell Arrays are also written in
 * this way.
 * @synopsis
 *    mat2hdf [OPTIONS] INPUT OUTPUT
 * @options
 * @opt -v         Set verbose @endopt
 * @opt --help     Print help string and exit @endopt
 * @opt --version  Print version number and exit @endopt
 *
 * @examples
 *   This example shows how to run @b mat2hdf from within a Matlab session.  The
 *   path to the executable @b mat2hdf must exist in your systems PATH.  This
 *   can be set by modifying the environment variable PATH.  Also to run the HDF
 *   command @b h5dump, the path to it must be set in the environment variable
 *   as well.
 * @code
 *     >> triangle.name = 'My Triangle';
 *     >> triangle.sides = [3 4 5];
 *     >> triangle.area = 1/2*(3*4);
 *     >> triangle
 *   
 *     triangle = 
 *   
 *          name: 'My Triangle'
 *         sides: [3 4 5]
 *          area: 6
 *   
 *     >> save triangle.mat
 *     >> !mat2hdf triangle.mat triangle.h5
 *     >> !h5dump triangle.h5
 *     HDF5 "triangle.h5" {
 *     GROUP "/" {
 *        DATASET "triangle" {
 *           DATATYPE  H5T_COMPOUND {
 *              H5T_STRING {
 *                 STRSIZE 12;
 *                 STRPAD H5T_STR_NULLTERM;
 *                 CSET H5T_CSET_ASCII;
 *                 CTYPE H5T_C_S1;
 *              } "name";
 *              H5T_ARRAY { [1][3] H5T_IEEE_F64LE } "sides";
 *              H5T_IEEE_F64LE "area";
 *           }
 *           DATASPACE  SIMPLE { ( 1, 1 ) / ( 1, 1 ) }
 *           DATA {
 *           (0,0): {
 *                 "My Triangle",
 *                 [ 3, 4, 5 ],
 *                 6
 *              }
 *           }
 *        }
 *     }
 *     }
 * @endcode
 */
/*
 * $Revision: 1.1.2.3 $ $State: Exp $
 * $Date: 2005/10/11 14:56:04 $
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <scatsio.h>
#include <scatsMAT.h>
#include <scatsHDF.h>

static char *helpstr[] = {
    "",
    "Usage: mat2hdf [OPTIONS] INPUT OUTPUT",
    "",
    "Converts the given MAT file to an HDF file",
    "",
    "OPTIONS",
    "-v          Sets verbose output",
    "--help      This output",
    "--version   version information",
    "",
    "",
    "INPUT is a Matlab Level 5 MAT file and can have compressed or ",
    "       uncompressed variables",
    "OUTPUT is an HDF5 file",
    NULL
};

static int write_mat( hid_t id, SCATS_MATVAR *matvar )
{

    switch ( matvar->class_type ) {
        case mxDOUBLE_CLASS:
        case mxSINGLE_CLASS:
        case mxINT32_CLASS:
        case mxUINT32_CLASS:
        case mxINT16_CLASS:
        case mxUINT16_CLASS:
        case mxINT8_CLASS:
        case mxUINT8_CLASS:
        case mxCELL_CLASS:
        case mxSTRUCT_CLASS:
        case mxCHAR_CLASS:
            Scats_VerbMessage(1,"Writing %s\n",matvar->name);
            Scats_SDSWriteMatVar(id,matvar);
            break;
        default:
            Scats_Critical("Unrecognized class type %d", matvar->class_type);
            return 1;
    }
    return 0;
}

static char *byteswapping[2] = {"No","Yes"};

int main ( int argc, char *argv[] )
{
    char *prog_name = "mat2hdf";
    int c;
    scats_mat_t *mat;
    SCATS_MATVAR *matvar;
    hid_t   hdf_id;

    if ( argc > 1 && !strcmp(argv[1],"--version")) {
        printf("mat2hdf v%d.%d.%d (compiled %s, %s for %s)\n",
               SCATS_MAJOR_VERSION, SCATS_MINOR_VERSION, SCATS_RELEASE_LEVEL,
               __DATE__, __TIME__, SCATS_PLATFORM );
        exit(0);
    } else if ( argc > 1 && !strcmp(argv[1],"--help") ) {
        Scats_Help(helpstr);
        exit(0);
    } else if ( argc < 3 )
        Scats_Help(helpstr);

    Scats_LogInit(prog_name);

    while ((c = getopt(argc, argv, "v")) != EOF) {
        switch (c) {
            case 'v':
                Scats_SetVerbose(1,0);
                break;
            default:
                Scats_Warning("%c not a valid option\n", c);
                break;
        }
    }

    mat = Scats_MatOpen( argv[optind],SCATS_ACC_RDONLY );
    if ( !mat )
        Scats_Error("Error opening %s\n", argv[1]);

    H5open();

    hdf_id = Scats_HDFOpen(argv[optind+1], SCATS_ACC_RDWR);
    if ( hdf_id < 0 ) {
        printf("Error opening HDF file %s\n", argv[2]);
        Scats_MatClose(mat);
        return 1;
    }

    while ( (matvar = Scats_MatVarReadNext(mat)) != NULL ) {
        write_mat(hdf_id,matvar);
        Scats_MatVarFree(matvar);
        matvar = NULL;
    }

    Scats_MatClose(mat);
    Scats_HDFClose(hdf_id);
    H5close();

    return 0;
}
