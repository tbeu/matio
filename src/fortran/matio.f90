!
! Copyright (C) 2005-2006   Christopher C. Hulbert
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation; either
!  version 2.1 of the License, or (at your option) any later version.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!

MODULE matio
!   Include the type definitions
    INCLUDE 'matio_t.inc'

!   C Routines
!    INTERFACE
!        INTEGER FUNCTION fmat_open_c(filename,mode,mat)
!            CHARACTER(LEN=*) :: filename
!            INTEGER          :: mode
!            TYPE(MAT_T)      :: mat
!        END FUNCTION fmat_open_c
!
!        INTEGER FUNCTION fmat_close_c(mat)
!            TYPE(MAT_T) :: mat
!        END FUNCTION fmat_close_c
!
!        INTEGER FUNCTION fmat_varcreate_c(rank,dims,name,class_type,      &
!                                          data_type,matvar)
!            INTEGER                 :: rank
!            INTEGER,DIMENSION(rank) :: dims
!            CHARACTER(LEN=*)        :: name
!            INTEGER                 :: class_type
!            INTEGER                 :: data_type
!            TYPE(MATVAR_T)          :: matvar
!        END FUNCTION fmat_varcreate_c
!
!        INTEGER FUNCTION fmat_varreadinfo_c(mat,matvar,dataptr,start,     &
!                                            stride,edge)
!            TYPE(mat_t)     :: mat
!            CHARACTER(*)    :: varname
!            TYPE(matvar_t)  :: matvar
!        END FUNCTION fmat_varreadinfo_c
!
!        INTEGER FUNCTION fmat_varfree_c(matvar)
!            TYPE(MATVAR_T) :: matvar
!        END FUNCTION fmat_varfree_c
!
!    END INTERFACE

    INTERFACE FMat_VarCreate
        MODULE PROCEDURE FMat_VarCreateA
        MODULE PROCEDURE FMat_VarCreateDouble0
        MODULE PROCEDURE FMat_VarCreateDouble1
        MODULE PROCEDURE FMat_VarCreateDouble2
        MODULE PROCEDURE FMat_VarCreateDouble3
        MODULE PROCEDURE FMat_VarCreateComplexDouble0
        MODULE PROCEDURE FMat_VarCreateComplexDouble1
        MODULE PROCEDURE FMat_VarCreateComplexDouble2
        MODULE PROCEDURE FMat_VarCreateComplexDouble3
        MODULE PROCEDURE FMat_VarCreateSingle0
        MODULE PROCEDURE FMat_VarCreateSingle1
        MODULE PROCEDURE FMat_VarCreateSingle2
        MODULE PROCEDURE FMat_VarCreateSingle3
        MODULE PROCEDURE FMat_VarCreateComplexSingle0
        MODULE PROCEDURE FMat_VarCreateComplexSingle1
        MODULE PROCEDURE FMat_VarCreateComplexSingle2
        MODULE PROCEDURE FMat_VarCreateComplexSingle3
        MODULE PROCEDURE FMat_VarCreateInt32_0
        MODULE PROCEDURE FMat_VarCreateInt32_1
        MODULE PROCEDURE FMat_VarCreateInt32_2
        MODULE PROCEDURE FMat_VarCreateInt32_3
        MODULE PROCEDURE FMat_VarCreateInt16_0
        MODULE PROCEDURE FMat_VarCreateInt16_1
        MODULE PROCEDURE FMat_VarCreateInt16_2
        MODULE PROCEDURE FMat_VarCreateInt16_3
        MODULE PROCEDURE FMat_VarCreateInt8_0
        MODULE PROCEDURE FMat_VarCreateInt8_1
        MODULE PROCEDURE FMat_VarCreateInt8_2
        MODULE PROCEDURE FMat_VarCreateInt8_3
        MODULE PROCEDURE FMat_VarCreateChar_0
        MODULE PROCEDURE FMat_VarCreateChar_1
        MODULE PROCEDURE FMat_VarCreateChar_2
        MODULE PROCEDURE FMat_VarCreateChar_3
    END INTERFACE FMat_VarCreate

    ! FMat_VarReadData(mat,matvar,d,start,stride,edge)
    INTERFACE FMat_VarReadData
        MODULE PROCEDURE FMat_VarReadDoubleData_0
        MODULE PROCEDURE FMat_VarReadDoubleData_1
        MODULE PROCEDURE FMat_VarReadDoubleData_2
        MODULE PROCEDURE FMat_VarReadDoubleData_3
        MODULE PROCEDURE FMat_VarReadComplexDoubleData_0
        MODULE PROCEDURE FMat_VarReadComplexDoubleData_1
        MODULE PROCEDURE FMat_VarReadComplexDoubleData_2
        MODULE PROCEDURE FMat_VarReadComplexDoubleData_3
        MODULE PROCEDURE FMat_VarReadSingleData_0
        MODULE PROCEDURE FMat_VarReadSingleData_1
        MODULE PROCEDURE FMat_VarReadSingleData_2
        MODULE PROCEDURE FMat_VarReadSingleData_3
        MODULE PROCEDURE FMat_VarReadComplexSingleData_0
        MODULE PROCEDURE FMat_VarReadComplexSingleData_1
        MODULE PROCEDURE FMat_VarReadComplexSingleData_2
        MODULE PROCEDURE FMat_VarReadComplexSingleData_3
        MODULE PROCEDURE FMat_VarReadInt32Data_0
        MODULE PROCEDURE FMat_VarReadInt32Data_1
        MODULE PROCEDURE FMat_VarReadInt32Data_2
        MODULE PROCEDURE FMat_VarReadInt32Data_3
        MODULE PROCEDURE FMat_VarReadInt16Data_0
        MODULE PROCEDURE FMat_VarReadInt16Data_1
        MODULE PROCEDURE FMat_VarReadInt16Data_2
        MODULE PROCEDURE FMat_VarReadInt16Data_3
        MODULE PROCEDURE FMat_VarReadInt8Data_0
        MODULE PROCEDURE FMat_VarReadInt8Data_1
        MODULE PROCEDURE FMat_VarReadInt8Data_2
        MODULE PROCEDURE FMat_VarReadInt8Data_3
        MODULE PROCEDURE FMat_VarReadCharData_0
        MODULE PROCEDURE FMat_VarReadCharData_1
        MODULE PROCEDURE FMat_VarReadCharData_2
        MODULE PROCEDURE FMat_VarReadCharData_3
    END INTERFACE

    INTERFACE FMat_VarWrite
        MODULE PROCEDURE FMat_VarWriteDouble_0
        MODULE PROCEDURE FMat_VarWriteDouble_1
        MODULE PROCEDURE FMat_VarWriteDouble_2
        MODULE PROCEDURE FMat_VarWriteDouble_3
        MODULE PROCEDURE FMat_VarWriteComplexDouble_0
        MODULE PROCEDURE FMat_VarWriteComplexDouble_1
        MODULE PROCEDURE FMat_VarWriteComplexDouble_2
        MODULE PROCEDURE FMat_VarWriteComplexDouble_3
        MODULE PROCEDURE FMat_VarWriteSingle_0
        MODULE PROCEDURE FMat_VarWriteSingle_1
        MODULE PROCEDURE FMat_VarWriteSingle_2
        MODULE PROCEDURE FMat_VarWriteSingle_3
        MODULE PROCEDURE FMat_VarWriteComplexSingle_0
        MODULE PROCEDURE FMat_VarWriteComplexSingle_1
        MODULE PROCEDURE FMat_VarWriteComplexSingle_2
        MODULE PROCEDURE FMat_VarWriteComplexSingle_3
        MODULE PROCEDURE FMat_VarWriteInt32_0
        MODULE PROCEDURE FMat_VarWriteInt32_1
        MODULE PROCEDURE FMat_VarWriteInt32_2
        MODULE PROCEDURE FMat_VarWriteInt32_3
        MODULE PROCEDURE FMat_VarWriteInt16_0
        MODULE PROCEDURE FMat_VarWriteInt16_1
        MODULE PROCEDURE FMat_VarWriteInt16_2
        MODULE PROCEDURE FMat_VarWriteInt16_3
        MODULE PROCEDURE FMat_VarWriteInt8_0
        MODULE PROCEDURE FMat_VarWriteInt8_1
        MODULE PROCEDURE FMat_VarWriteInt8_2
        MODULE PROCEDURE FMat_VarWriteInt8_3
    END INTERFACE

    INTERFACE FMat_VarWriteData
        MODULE PROCEDURE FMat_VarWriteDoubleData_0
        MODULE PROCEDURE FMat_VarWriteDoubleData_1
        MODULE PROCEDURE FMat_VarWriteDoubleData_2
        MODULE PROCEDURE FMat_VarWriteDoubleData_3
        MODULE PROCEDURE FMat_VarWriteComplexDoubleData_0
        MODULE PROCEDURE FMat_VarWriteComplexDoubleData_1
        MODULE PROCEDURE FMat_VarWriteComplexDoubleData_2
        MODULE PROCEDURE FMat_VarWriteComplexDoubleData_3
        MODULE PROCEDURE FMat_VarWriteSingleData_0
        MODULE PROCEDURE FMat_VarWriteSingleData_1
        MODULE PROCEDURE FMat_VarWriteSingleData_2
        MODULE PROCEDURE FMat_VarWriteSingleData_3
        MODULE PROCEDURE FMat_VarWriteComplexSingleData_0
        MODULE PROCEDURE FMat_VarWriteComplexSingleData_1
        MODULE PROCEDURE FMat_VarWriteComplexSingleData_2
        MODULE PROCEDURE FMat_VarWriteComplexSingleData_3
        MODULE PROCEDURE FMat_VarWriteInt32Data_0
        MODULE PROCEDURE FMat_VarWriteInt32Data_1
        MODULE PROCEDURE FMat_VarWriteInt32Data_2
        MODULE PROCEDURE FMat_VarWriteInt32Data_3
        MODULE PROCEDURE FMat_VarWriteInt16Data_0
        MODULE PROCEDURE FMat_VarWriteInt16Data_1
        MODULE PROCEDURE FMat_VarWriteInt16Data_2
        MODULE PROCEDURE FMat_VarWriteInt16Data_3
        MODULE PROCEDURE FMat_VarWriteInt8Data_0
        MODULE PROCEDURE FMat_VarWriteInt8Data_1
        MODULE PROCEDURE FMat_VarWriteInt8Data_2
        MODULE PROCEDURE FMat_VarWriteInt8Data_3
        MODULE PROCEDURE FMat_VarWriteCharData_0
        MODULE PROCEDURE FMat_VarWriteCharData_1
        MODULE PROCEDURE FMat_VarWriteCharData_2
        MODULE PROCEDURE FMat_VarWriteCharData_3
    END INTERFACE

CONTAINS

!----------------------------------------------------------
!   FMat_LogInit
!
!   Initilize the logging functions
!
!   prog_name: Name of program/function initializing log functions
!----------------------------------------------------------
    SUBROUTINE FMat_LogInit(prog_name)
        CHARACTER(LEN=*) :: prog_name

        CALL fmat_loginit_c(prog_name)
    END SUBROUTINE FMat_LogInit

!----------------------------------------------------------
!   FMat_Open
!
!   Opens a Matlab file
!
!   filename: Name of the matlab file
!   mode:     Mode to open the file. One of MAT_ACC_*
!   mat:      mat_t output structure containing the file information
!----------------------------------------------------------
    FUNCTION FMat_Open(filename,mode,mat) RESULT(err)
    IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(IN)  :: filename
        INTEGER,INTENT(IN)           :: mode
        TYPE(mat_t),INTENT(OUT)      :: mat
        INTEGER                      :: err
        INTEGER,EXTERNAL :: fmat_open_c

        err = fmat_open_c(filename,mode,mat)
    END FUNCTION FMat_Open

!----------------------------------------------------------
!   FMat_Open
!
!   Opens a Matlab file
!
!   filename: Name of the matlab file
!   mode:     Mode to open the file. One of MAT_ACC_*
!   mat:      mat_t output structure containing the file information
!----------------------------------------------------------
    FUNCTION FMat_Create(filename,mat,header) RESULT(err)
    IMPLICIT NONE
        CHARACTER(LEN=*),INTENT(IN)           :: filename
        CHARACTER(LEN=*),INTENT(IN),OPTIONAL  :: header
        TYPE(mat_t),INTENT(OUT)               :: mat
        INTEGER                               :: err
        INTEGER,EXTERNAL :: fmat_create_c

        IF ( .NOT. PRESENT(header) ) THEN
            err = fmat_create_c(filename,mat,nullptr)
        ELSE
            err = fmat_create_c(filename,mat,header)
        END IF
    END FUNCTION FMat_Create

!----------------------------------------------------------
!   FMat_Close
!
!   Closes an open Matlab file
!
!   mat: mat_t structure containing the file information
!----------------------------------------------------------
    FUNCTION FMat_Close(mat) RESULT(err)
        INTEGER  (KIND=4) err
        TYPE(mat_t),INTENT(INOUT)  :: mat

        err = fmat_close_c(mat)
    END FUNCTION FMat_Close

!----------------------------------------------------------
!   FMat_VarPrint
!
!   Prints a Matlab variable's information and optionally the data
!
!   matvar: matvar_t structure containing the variable information
!----------------------------------------------------------
    SUBROUTINE FMat_VarPrint(matvar)
        TYPE(matvar_t),INTENT(IN)  :: matvar

        CALL fmat_varprint_c(matvar)
    END SUBROUTINE FMat_VarPrint

!----------------------------------------------------------
!   FMat_VarReadInfo
!
!   Reads the information for a variable from the file in mat
!
!   mat:     mat_t structure containing the Matlab file information
!   varname: Name of the variable to read
!   matvar:  output matvar_t structure containing the variable information
!
!   err: 0 on success
!----------------------------------------------------------
    FUNCTION FMat_VarReadInfo(mat,varname,matvar) RESULT(err)
        TYPE(mat_t),   INTENT(INOUT)         :: mat
        CHARACTER(*),  INTENT(IN)            :: varname
        TYPE(matvar_t),INTENT(OUT)           :: matvar
        INTEGER                              :: err
        INTEGER,EXTERNAL                     :: fmat_varreadinfo_c

        err = fmat_varreadinfo_c(mat,varname,matvar)
    END FUNCTION FMAT_VARREADINFO

!----------------------------------------------------------
!   FMat_VarReadInfo
!
!   Reads the information for a variable from the file in mat
!
!   mat:     mat_t structure containing the Matlab file information
!   varname: Name of the variable to read
!   matvar:  output matvar_t structure containing the variable information
!
!   err: 0 on success
!----------------------------------------------------------
    FUNCTION FMat_VarReadNextInfo(mat,matvar) RESULT(err)
        TYPE(mat_t),   INTENT(INOUT)         :: mat
        TYPE(matvar_t),INTENT(OUT)           :: matvar
        INTEGER                              :: err
        INTEGER,EXTERNAL                     :: fmat_varreadnextinfo_c

        err = fmat_varreadnextinfo_c(mat,matvar)
    END FUNCTION FMat_VarReadNextInfo

!----------------------------------------------------------
!   FMat_VarRead
!
!   Reads a variable from the file in mat
!
!   mat:     mat_t structure containing the Matlab file information
!   varname: Name of the variable to read
!   matvar:  output matvar_t structure containing the variable
!
!   err: 0 on success
!----------------------------------------------------------
    FUNCTION FMat_VarRead(mat,varname,matvar) RESULT(err)
        TYPE(mat_t),INTENT(INOUT)          :: mat
        CHARACTER(*),INTENT(OUT)           :: varname
        TYPE(matvar_t),INTENT(OUT)         :: matvar
        INTEGER(4)                         :: err

        err = fmat_varread_c(mat,varname,matvar)
    END FUNCTION FMAT_VARREAD

    FUNCTION FMat_VarWriteInfo(mat,matvar) RESULT(err)
        TYPE(mat_t),INTENT(INOUT)           :: mat
        TYPE(matvar_t),INTENT(INOUT)        :: matvar

        err = fmat_varwriteinfo_c(mat,matvar)
    END FUNCTION

    FUNCTION FMat_VarFree(matvar) RESULT(err)
        TYPE(matvar_t)         :: matvar
        INTEGER(4)             :: err

        err = fmat_varfree_c(matvar)
    END FUNCTION FMAT_VARFREE

    FUNCTION FMat_VarGetStuctFieldByName(struct,field_name,struct_index,  &
                                         field) RESULT(err)
        TYPE(MATVAR_T),INTENT(IN)  :: struct
        CHARACTER(LEN=*)           :: field_name
        INTEGER,INTENT(IN)         :: struct_index
        TYPE(MATVAR_T),INTENT(OUT) :: field
        INTEGER,EXTERNAL           :: fmat_vargetstructfield_byname_c

        err = fmat_vargetstructfield_byname_c(struct,field_name,          &
            struct_index,field)
    END FUNCTION FMat_VarGetStuctFieldByName

    FUNCTION FMat_VarGetNumberOfFields(struct) RESULT(nfields)
        TYPE(MATVAR_T),INTENT(IN)  :: struct
        INTEGER,EXTERNAL           :: fmat_vargetnumberoffields_c

        nfields = fmat_vargetnumberoffields_c(struct)
    END FUNCTION FMat_VarGetNumberOfFields

!   Include the fmat_varcreate interface routines
    INCLUDE 'create.f90'

!   Include the fmat_varreaddata interface routines
    INCLUDE 'read_data.f90'

!   Include the fmat_varwrite interface routines
    INCLUDE 'write.f90'

!   Include the fmat_varwritedata interface routines
    INCLUDE 'write_data.f90'

END MODULE matio
