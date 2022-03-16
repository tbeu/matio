!
! Copyright (c) 2015-2022, The matio contributors
! Copyright (c) 2005-2014, Christopher C. Hulbert
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
PROGRAM test
USE MATIO
IMPLICIT NONE
    CHARACTER(len=4)                :: prog_name
    INTEGER                         :: opt
    INTEGER(KIND=4)                 :: mode, err, i,rank, class_type,data_type
    TYPE(mat_t)                     :: mat
    TYPE(matvar_t)                  :: matvar,field

    prog_name = 'test'
    CALL FMAT_LOGINIT(prog_name)

    DO
        WRITE (*,*) '   Version 5 MAT File tests'
        WRITE (*,*) '========================================================='
        WRITE (*,*) ' 1.  copy           - Copies one matlab file to another'
        WRITE (*,*) ' 2.  write          - Writes a matlab file'
        WRITE (*,*) ' 3.  readvar        - Reads a specific variable from a file'
        WRITE (*,*) ' 4.  writestruct    - Writes a structure'
        WRITE (*,*) ' 5.  writecell      - Writes a Cell Array'
        WRITE (*,*) ' 6.  getstructfield - Uses Mat_VarGetStructField to get fields from a structure'
        WRITE (*,*) ' 7.  readvarinfo    - Reads a variables header information only'
        WRITE (*,*) ' 8.  readslab       - Tests reading a part of a dataset'
        WRITE (*,*) ' 9.  writeslab      - Tests writing a part of a dataset'
        WRITE (*,*) ' 10. writesparse    - Tests writing a sparse matrix'
        WRITE (*,*) ' 11. writecompressed - Tests writing a compressed file'
        WRITE (*,*) ''
        WRITE (*,*) ''
        WRITE (*,*) '   Version 4 MAT File tests'
        WRITE (*,*) '========================================================='
        WRITE (*,*) ''
        WRITE (*,*) ''
        WRITE (*,*) '   Other Tests'
        WRITE (*,*) '========================================================='
        WRITE (*,*) '101. ind2sub - Calculates a set of subscripts from a linear index'
        WRITE (*,*) '102. sub2ind - Calculates the linear index from subscript values'
        WRITE (*,*) '999.  Exit'
        WRITE (*,*) ''
        WRITE (*,'(a)',ADVANCE='NO') 'Select a test: '
        READ (*,'(i3)') opt

        SELECT CASE (opt)
            CASE (999)
                EXIT
            CASE (1)
                CALL test_copy
            CASE (2)
                CALL test_write
            CASE (3)
                CALL test_readvar
            CASE (4)
                CALL test_writestruct
            CASE (5)
                CALL test_writecell
            CASE (6)
                CALL test_getstructfield
            CASE (7)
                CALL test_readvarinfo
            CASE (8)
                CALL test_readslab
            CASE (9)
                CALL test_writeslab
            CASE (10)
                CALL test_writesparse
            CASE (11)
                CALL test_write_compressed
            CASE DEFAULT
                WRITE (*,*) ''
                WRITE (*,*) 'Invalid option!'
                WRITE (*,*) ''
        END SELECT
    END DO

CONTAINS
    SUBROUTINE test_copy
    USE MATIO
    IMPLICIT NONE
        DOUBLE PRECISION,ALLOCATABLE :: double_data(:)
        REAL(4),ALLOCATABLE          :: single_data(:)
        INTEGER(4),ALLOCATABLE       :: int32_data(:)
        INTEGER(2),ALLOCATABLE       :: int16_data(:)
        INTEGER(1),ALLOCATABLE       :: int8_data(:)
        INTEGER            :: err,N
        TYPE(matvar_t)     :: matvar
        TYPE(mat_t)        :: mat,mat2
        CHARACTER(LEN=128) :: inputfile
        CHARACTER(LEN=17)  :: outfilename

        outfilename = 'test_mat_copy.mat'
        WRITE (*,'(a)',ADVANCE='NO') 'Enter the input filename: '
        READ (*,'(a128)') inputfile

        err = FMat_Open(inputfile,MAT_ACC_RDWR,mat)
        err = FMat_Create(outfilename,MAT_FT_MAT5,mat2)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',inputfile
            RETURN
        ELSE
            err = FMat_VarReadNextInfo(mat,matvar)
            DO WHILE ( err .EQ. 0 )
                N = PRODUCT(matvar%dims(1:matvar%rank))
                IF ( matvar%isComplex .GT. 0 ) N=N*2
                SELECT CASE (matvar%class_type)
                    CASE (MAT_C_DOUBLE)
                        ALLOCATE(double_data(N))
                        err = FMat_VarReadData(mat,matvar,double_data)
                        err = FMat_VarWrite(mat2,matvar,double_data)
                        DEALLOCATE(double_data)
                    CASE (MAT_C_SINGLE)
                        ALLOCATE(single_data(N))
                        err = FMat_VarReadData(mat,matvar,single_data)
                        err = FMat_VarWrite(mat2,matvar,single_data)
                        DEALLOCATE(single_data)
                    CASE (MAT_C_INT32,MAT_C_UINT32)
                        ALLOCATE(int32_data(N))
                        err = FMat_VarReadData(mat,matvar,int32_data)
                        err = FMat_VarWrite(mat2,matvar,int32_data)
                        DEALLOCATE(int32_data)
                    CASE (MAT_C_INT16,MAT_C_UINT16)
                        ALLOCATE(int16_data(N))
                        err = FMat_VarReadData(mat,matvar,int16_data)
                        err = FMat_VarWrite(mat2,matvar,int16_data)
                        DEALLOCATE(int16_data)
                    CASE (MAT_C_INT8,MAT_C_UINT8)
                        ALLOCATE(int8_data(N))
                        err = FMat_VarReadData(mat,matvar,int8_data)
                        err = FMat_VarWrite(mat2,matvar,int8_data)
                        DEALLOCATE(int8_data)
                END SELECT
                err = FMat_VarReadNextInfo(mat,matvar)
            END DO
            err = FMat_Close(mat)
            err = FMat_Close(mat2)
        END IF
    END SUBROUTINE test_copy

    SUBROUTINE test_write
    USE MATIO
    IMPLICIT NONE
        CHARACTER(LEN=18)  :: filename = 'test_mat_write.mat'
        INTEGER            :: err,k,l,n = 1
        TYPE(MAT_T)        :: mat
        TYPE(MATVAR_T)     :: matvar
        ! Data
        CHARACTER(LEN=3)           :: cdouble_name = 'd_j'
        CHARACTER(LEN=1)           :: double_name = 'd'
        CHARACTER(LEN=1)           :: single_name = 's'
        CHARACTER(LEN=3)           :: int32_name  = 'i32'
        CHARACTER(LEN=3)           :: int16_name  = 'i16'
        CHARACTER(LEN=2)           :: int8_name   = 'i8'
        CHARACTER(LEN=1)           :: char_name   = 'c'
        REAL(8),DIMENSION(5,10)    :: double_data
        COMPLEX(8),DIMENSION(5,10) :: cdouble_data
        REAL(4),DIMENSION(5,10)    :: single_data
        COMPLEX(4),DIMENSION(5,10) :: csingle_data
        INTEGER(4),DIMENSION(5,10) :: int32_data
        INTEGER(2),DIMENSION(5,10) :: int16_data
        INTEGER(1),DIMENSION(5,10) :: int8_data
        CHARACTER(1),DIMENSION(5,10) :: char_data

        double_data  = RESHAPE((/(n,n=1,50)/),(/5,10/))
        cdouble_data = CMPLX(RESHAPE((/(n,n=1,50)/),(/5,10/)),  &
                             RESHAPE((/(n,n=1,50)/),(/5,10/)))
        single_data  = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int32_data   = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int16_data   = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int8_data    = RESHAPE((/(n,n=1,50)/),(/5,10/))

        char_data(1,1)  = 'a'
        char_data(2,1)  = 'b'
        char_data(3,1)  = 'c'
        char_data(4,1)  = 'd'
        char_data(5,1)  = 'e'
        char_data(1,2)  = 'f'
        char_data(2,2)  = 'g'
        char_data(3,2)  = 'h'
        char_data(4,2)  = 'i'
        char_data(5,2) = 'j'
        char_data(1,3) = 'k'
        char_data(2,3) = 'l'
        char_data(3,3) = 'm'
        char_data(4,3) = 'n'
        char_data(5,3) = 'o'
        char_data(1,4) = 'p'
        char_data(2,4) = 'q'
        char_data(3,4) = 'r'
        char_data(4,4) = 's'
        char_data(5,4) = 't'
        char_data(1,5) = 'u'
        char_data(2,5) = 'v'
        char_data(3,5) = 'w'
        char_data(4,5) = 'x'
        char_data(5,5) = 'y'
        char_data(1,6) = 'z'
        char_data(2,6) = '0'
        char_data(3,6) = '1'
        char_data(4,6) = '2'
        char_data(5,6) = '3'
        char_data(1,7) = '4'
        char_data(2,7) = '5'
        char_data(3,7) = '6'
        char_data(4,7) = '7'
        char_data(5,7) = '8'
        char_data(1,8) = '9'
        char_data(2,8) = '!'
        char_data(3,8) = '@'
        char_data(4,8) = '#'
        char_data(5,8) = '$'
        char_data(1,9) = '%'
        char_data(2,9) = '^'
        char_data(3,9) = '&'
        char_data(4,9) = '*'
        char_data(5,9) = '('
        char_data(1,10) = ')'
        char_data(2,10) = '-'
        char_data(3,10) = '+'
        char_data(4,10) = '='
        char_data(5,10) = '~'

        err = FMat_Open(filename,MAT_ACC_RDWR,mat)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',filename
            RETURN
        ELSE
            err = FMat_VarCreate(double_name,double_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,double_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(cdouble_name,cdouble_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,cdouble_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(single_name,single_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,single_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int32_name,int32_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,int32_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int16_name,int16_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,int16_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int8_name,int8_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,int8_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(char_name,char_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,char_data)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_Close(mat)
        END IF
    END SUBROUTINE test_write

    SUBROUTINE test_write_compressed
    USE MATIO
    IMPLICIT NONE
        CHARACTER(LEN=64)  :: filename = 'test_mat_write_compressed.mat'
        INTEGER            :: err,k,l,n = 1
        TYPE(MAT_T)        :: mat
        TYPE(MATVAR_T)     :: matvar
        ! Data
        CHARACTER(LEN=1)           :: double_name = 'd'
        CHARACTER(LEN=1)           :: single_name = 's'
        CHARACTER(LEN=3)           :: int32_name  = 'i32'
        CHARACTER(LEN=3)           :: int16_name  = 'i16'
        CHARACTER(LEN=2)           :: int8_name   = 'i8'
        REAL(8),DIMENSION(5,10)    :: double_data
        REAL(4),DIMENSION(5,10)    :: single_data
        INTEGER(4),DIMENSION(5,10) :: int32_data
        INTEGER(2),DIMENSION(5,10) :: int16_data
        INTEGER(1),DIMENSION(5,10) :: int8_data

        double_data = RESHAPE((/(n,n=1,50)/),(/5,10/))
        single_data = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int32_data  = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int16_data  = RESHAPE((/(n,n=1,50)/),(/5,10/))
        int8_data   = RESHAPE((/(n,n=1,50)/),(/5,10/))

        err = FMat_Open(filename,MAT_ACC_RDWR,mat)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',filename
            RETURN
        ELSE
            err = FMat_VarCreate(double_name,double_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,double_data,COMPRESSION_ZLIB)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(single_name,single_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,single_data,COMPRESSION_ZLIB)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int32_name,int32_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,int32_data,COMPRESSION_ZLIB)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int16_name,int16_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,int16_data,COMPRESSION_ZLIB)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int8_name,int8_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWrite(mat,matvar,int8_data,COMPRESSION_ZLIB)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_Close(mat)
        END IF
    END SUBROUTINE test_write_compressed

    SUBROUTINE test_readvar
    USE MATIO
    IMPLICIT NONE
        CHARACTER(LEN=128) :: inputfile
        CHARACTER(LEN=32)  :: varname
        TYPE(MAT_T)        :: mat
        TYPE(MATVAR_T)     :: matvar
        INTEGER            :: err,N
        ! Data matrices
        REAL(8),DIMENSION(:),ALLOCATABLE      :: double_data
        REAL(4),DIMENSION(:),ALLOCATABLE      :: single_data
        INTEGER(4),DIMENSION(:),ALLOCATABLE   :: int32_data
        INTEGER(2),DIMENSION(:),ALLOCATABLE   :: int16_data
        INTEGER(1),DIMENSION(:),ALLOCATABLE   :: int8_data
        CHARACTER(1),DIMENSION(:),ALLOCATABLE :: char_data

        WRITE (*,'(a)',ADVANCE='NO') 'Enter the input filename: '
        READ (*,'(a128)') inputfile
        WRITE (*,'(a)',ADVANCE='NO') 'Enter the variable name: '
        READ (*,'(a32)') varname

        err = FMat_Open(inputfile,MAT_ACC_RDONLY,mat)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',inputfile
            RETURN
        ELSE
            err = FMat_VarReadInfo(mat,varname,matvar)
            IF ( err .NE. 0 ) THEN
                WRITE (*,*) 'err=',err
                RETURN
            END IF
            N = PRODUCT(matvar%dims(1:matvar%rank))
            SELECT CASE (matvar%class_type)
                CASE (MAT_C_DOUBLE)
                    ALLOCATE(double_data(N))
                    err = FMat_VarReadData(mat,matvar,double_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) double_data
                    DEALLOCATE(double_data)
                CASE (MAT_C_SINGLE)
                    ALLOCATE(single_data(N))
                    err = FMat_VarReadData(mat,matvar,single_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) single_data
                    DEALLOCATE(single_data)
                CASE (MAT_C_INT32,MAT_C_UINT32)
                    ALLOCATE(int32_data(N))
                    err = FMat_VarReadData(mat,matvar,int32_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) int32_data
                    DEALLOCATE(int32_data)
                CASE (MAT_C_INT16,MAT_C_UINT16)
                    ALLOCATE(int16_data(N))
                    err = FMat_VarReadData(mat,matvar,int16_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) int16_data
                    DEALLOCATE(int16_data)
                CASE (MAT_C_INT8,MAT_C_UINT8)
                    ALLOCATE(int8_data(N))
                    err = FMat_VarReadData(mat,matvar,int8_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) int8_data
                    DEALLOCATE(int8_data)
                CASE (MAT_C_CHAR)
                    ALLOCATE(char_data(N))
                    err = FMat_VarReadData(mat,matvar,char_data)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) char_data
                    DEALLOCATE(char_data)
                CASE (MAT_C_STRUCT)
                    err = FMat_VarGetNumberOfFields(matvar)
                    WRITE(*,*) '      Name: ',matvar%name
                    WRITE(*,*) '      Rank: ',matvar%rank
                    WRITE(*,*) '     Class: ',matvar%class_type
                    WRITE(*,*) 'Dimensions: ',matvar%dims(1:matvar%rank)
                    WRITE(*,*) 'Number of Fields: ',err
            END SELECT
            err = FMat_VarFree(matvar)
            err = FMat_Close(mat)
        END IF
    END SUBROUTINE test_readvar

    SUBROUTINE test_writestruct
    END SUBROUTINE test_writestruct
    SUBROUTINE test_writecell
    END SUBROUTINE test_writecell
    SUBROUTINE test_getstructfield
    END SUBROUTINE test_getstructfield
    SUBROUTINE test_readvarinfo
    END SUBROUTINE test_readvarinfo

    SUBROUTINE test_writeslab
    USE MATIO
    IMPLICIT NONE
        CHARACTER(LEN=64)  :: filename = 'test_mat_writeslab.mat'
        INTEGER            :: err,k,l,n = 1
        INTEGER,DIMENSION(2) :: start  = (/0,0/)
        INTEGER,DIMENSION(2) :: stride = (/2,2/)
        INTEGER,DIMENSION(2) :: edge   = (/3,5/)
        TYPE(MAT_T)        :: mat
        TYPE(MATVAR_T)     :: matvar
        ! Data
        CHARACTER(LEN=1)           :: double_name = 'd'
        CHARACTER(LEN=1)           :: single_name = 's'
        CHARACTER(LEN=3)           :: int32_name  = 'i32'
        REAL(8),DIMENSION(6,10)    :: double_data
        REAL(4),DIMENSION(6,10)    :: single_data
        INTEGER(4),DIMENSION(6,10) :: int32_data

        double_data = RESHAPE((/(n,n=1,60)/),(/6,10/))
        single_data = RESHAPE((/(n,n=1,60)/),(/6,10/))
        int32_data  = RESHAPE((/(n,n=1,60)/),(/6,10/))

        err = FMat_Open(filename,MAT_ACC_RDWR,mat)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',filename
            RETURN
        ELSE
            err = FMat_VarCreate(double_name,double_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,double_data,start,stride,edge)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(single_name,single_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,single_data,start,stride,edge)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_VarCreate(int32_name,int32_data,matvar)
            IF (err .EQ. 0) THEN
                err = FMat_VarWriteInfo(mat,matvar)
                err = FMat_VarWriteData(mat,matvar,int32_data,start,stride,edge)
                err = FMat_VarFree(matvar)
            END IF
            err = FMat_Close(mat)
        END IF
    END SUBROUTINE test_writeslab

    SUBROUTINE test_readslab
    USE MATIO
    IMPLICIT NONE
        CHARACTER(LEN=128) :: inputfile
        CHARACTER(LEN=32)  :: varname
        TYPE(MAT_T)        :: mat
        TYPE(MATVAR_T)     :: matvar
        INTEGER            :: err,N
        INTEGER,DIMENSION(2) :: start  = (/0,0/)
        INTEGER,DIMENSION(2) :: stride = (/1,1/)
        INTEGER,DIMENSION(2) :: edge   = (/2,2/)
        ! Data matrices
        REAL(8),DIMENSION(4)      :: double_data
        REAL(4),DIMENSION(4)      :: single_data
        INTEGER(4),DIMENSION(4)   :: int32_data
        INTEGER(2),DIMENSION(4)   :: int16_data
        INTEGER(1),DIMENSION(4)   :: int8_data

        WRITE (*,'(a)',ADVANCE='NO') 'Enter the input filename: '
        READ (*,'(a128)') inputfile
        WRITE (*,'(a)',ADVANCE='NO') 'Enter the variable name: '
        READ (*,'(a32)') varname

        err = FMat_Open(inputfile,MAT_ACC_RDONLY,mat)
        IF ( err .NE. 0 ) THEN
            WRITE (*,*) 'Error opening file ',inputfile
            RETURN
        ELSE
            err = FMat_VarReadInfo(mat,varname,matvar)
            IF ( err .NE. 0 ) THEN
                WRITE (*,*) 'err=',err
                RETURN
            END IF
            SELECT CASE (matvar%class_type)
                CASE (MAT_C_DOUBLE)
                    stride(1) = matvar%dims(1)-1
                    stride(2) = matvar%dims(2)-1
                    err = FMat_VarReadData(mat,matvar,double_data,start,stride,edge)
                    WRITE(*,*) double_data
                CASE (MAT_C_SINGLE)
                    stride(1) = matvar%dims(1)-1
                    stride(2) = matvar%dims(2)-1
                    err = FMat_VarReadData(mat,matvar,single_data,start,stride,edge)
                    WRITE(*,*) single_data
                CASE (MAT_C_INT32,MAT_C_UINT32)
                    stride(1) = matvar%dims(1)-1
                    stride(2) = matvar%dims(2)-1
                    err = FMat_VarReadData(mat,matvar,int32_data,start,stride,edge)
                    WRITE(*,*) int32_data
                CASE (MAT_C_INT16,MAT_C_UINT16)
                    stride(1) = matvar%dims(1)-1
                    stride(2) = matvar%dims(2)-1
                    err = FMat_VarReadData(mat,matvar,int16_data,start,stride,edge)
                    WRITE(*,*) int16_data
                CASE (MAT_C_INT8,MAT_C_UINT8)
                    stride(1) = matvar%dims(1)-1
                    stride(2) = matvar%dims(2)-1
                    err = FMat_VarReadData(mat,matvar,int8_data,start,stride,edge)
                    WRITE(*,*) int8_data
            END SELECT
            err = FMat_VarFree(matvar)
            err = FMat_Close(mat)
        END IF
    END SUBROUTINE test_readslab

    SUBROUTINE test_writesparse
    END SUBROUTINE test_writesparse

END PROGRAM test
