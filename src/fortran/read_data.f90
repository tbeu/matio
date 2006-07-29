!----------------------------------------------------------
!   FMat_VarReadData Interface procedures
!
!   This files shuold be included on the MATIO module
!----------------------------------------------------------
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

FUNCTION FMat_VarReadDoubleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=8),INTENT(OUT)                    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadDoubleData_0

FUNCTION FMat_VarReadDoubleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=8),INTENT(OUT),DIMENSION(:)       :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadDoubleData_1

FUNCTION FMat_VarReadDoubleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=8),INTENT(OUT),DIMENSION(:,:)     :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadDoubleData_2

FUNCTION FMat_VarReadDoubleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=8),INTENT(OUT),DIMENSION(:,:,:)   :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadDoubleData_3

FUNCTION FMat_VarReadComplexDoubleData_0(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    DOUBLE COMPLEX,INTENT(OUT)                  :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadComplexDoubleData_0

FUNCTION FMat_VarReadComplexDoubleData_1(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    DOUBLE COMPLEX,INTENT(OUT),DIMENSION(:)     :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*SIZE(d))       :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(d_split(1:SIZE(d)),d_split(SIZE(d)+1:))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(d_split(1:SIZE(d)),d_split(SIZE(d)+1:))
    ENDIF
END FUNCTION FMat_VarReadComplexDoubleData_1

FUNCTION FMat_VarReadComplexDoubleData_2(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    DOUBLE COMPLEX,INTENT(OUT),DIMENSION(:,:)   :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ENDIF
END FUNCTION FMat_VarReadComplexDoubleData_2

FUNCTION FMat_VarReadComplexDoubleData_3(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    DOUBLE COMPLEX,INTENT(OUT),DIMENSION(:,:,:) :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    DOUBLE PRECISION,DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ENDIF
END FUNCTION FMat_VarReadComplexDoubleData_3

FUNCTION FMat_VarReadSingleData_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=4),INTENT(OUT)                    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadSingleData_0

FUNCTION FMat_VarReadSingleData_1(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=4),INTENT(OUT),DIMENSION(:)       :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadSingleData_1

FUNCTION FMat_VarReadSingleData_2(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=4),INTENT(OUT),DIMENSION(:,:)     :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadSingleData_2

FUNCTION FMat_VarReadSingleData_3(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    REAL(KIND=4),INTENT(OUT),DIMENSION(:,:,:)   :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadSingleData_3

FUNCTION FMat_VarReadComplexSingleData_0(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    COMPLEX(4),INTENT(OUT)                  :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadComplexSingleData_0

FUNCTION FMat_VarReadComplexSingleData_1(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    COMPLEX(4),INTENT(OUT),DIMENSION(:)     :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    REAL(4),DIMENSION(2*SIZE(d))       :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(d_split(1:SIZE(d)),d_split(SIZE(d)+1:))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(d_split(1:SIZE(d)),d_split(SIZE(d)+1:))
    ENDIF
END FUNCTION FMat_VarReadComplexSingleData_1

FUNCTION FMat_VarReadComplexSingleData_2(mat,matvar,d,start,stride,edge) &
                                                             RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    COMPLEX(4),INTENT(OUT),DIMENSION(:,:)   :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ENDIF
END FUNCTION FMat_VarReadComplexSingleData_2

FUNCTION FMat_VarReadComplexSingleData_3(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    COMPLEX(4),INTENT(OUT),DIMENSION(:,:,:) :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    REAL(4),DIMENSION(2*PRODUCT(SHAPE(d))) :: d_split
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d_split,start,stride,edge)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d_split,nullptr,nullptr,nullptr)
        d = CMPLX(RESHAPE(d_split(1:PRODUCT(SHAPE(d))),SHAPE(d)),         &
                  RESHAPE(d_split(PRODUCT(SHAPE(d))+1:),SHAPE(d)))
    ENDIF
END FUNCTION FMat_VarReadComplexSingleData_3

FUNCTION FMat_VarReadInt32Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=4),INTENT(OUT)                 :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadInt32Data_0

FUNCTION FMAT_VARREADINT32DATA_1(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=4),INTENT(OUT),DIMENSION(:)    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT32DATA_1

FUNCTION FMAT_VARREADINT32DATA_2(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=4),INTENT(OUT),DIMENSION(:,:)  :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT32DATA_2

FUNCTION FMAT_VARREADINT32DATA_3(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                   :: err
    TYPE(mat_t)                                  :: mat
    TYPE(matvar_t)                               :: matvar
    INTEGER(KIND=4),INTENT(OUT),DIMENSION(:,:,:) :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: edge
    INTEGER,EXTERNAL                             :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT32DATA_3

FUNCTION FMat_VarReadInt16Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=2),INTENT(OUT)                 :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadInt16Data_0

FUNCTION FMAT_VARREADINT16DATA_1(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=2),INTENT(OUT),DIMENSION(:)    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT16DATA_1

FUNCTION FMAT_VARREADINT16DATA_2(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=2),INTENT(OUT),DIMENSION(:,:)  :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT16DATA_2

FUNCTION FMAT_VARREADINT16DATA_3(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                   :: err
    TYPE(mat_t)                                  :: mat
    TYPE(matvar_t)                               :: matvar
    INTEGER(KIND=2),INTENT(OUT),DIMENSION(:,:,:) :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: edge
    INTEGER,EXTERNAL                             :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT16DATA_3

FUNCTION FMat_VarReadInt8Data_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=1),INTENT(OUT)                 :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadInt8Data_0

FUNCTION FMAT_VARREADINT8DATA_1(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=1),INTENT(OUT),DIMENSION(:)    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT8DATA_1

FUNCTION FMAT_VARREADINT8DATA_2(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    INTEGER(KIND=1),INTENT(OUT),DIMENSION(:,:)  :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT8DATA_2

FUNCTION FMAT_VARREADINT8DATA_3(mat,matvar,d,start,stride,edge) &
                                                       RESULT(err)
    INTEGER(4)                                   :: err
    TYPE(mat_t)                                  :: mat
    TYPE(matvar_t)                               :: matvar
    INTEGER(KIND=1),INTENT(OUT),DIMENSION(:,:,:) :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL  :: edge
    INTEGER,EXTERNAL                             :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMAT_VARREADINT8DATA_3

FUNCTION FMat_VarReadCharData_0(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    CHARACTER(1),INTENT(OUT)                    :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadCharData_0

FUNCTION FMat_VarReadCharData_1(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    CHARACTER(1),INTENT(OUT),DIMENSION(:)       :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadCharData_1

FUNCTION FMat_VarReadCharData_2(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    CHARACTER(1),INTENT(OUT),DIMENSION(:,:)     :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadCharData_2

FUNCTION FMat_VarReadCharData_3(mat,matvar,d,start,stride,edge) RESULT(err)
    INTEGER(4)                                  :: err
    TYPE(mat_t)                                 :: mat
    TYPE(matvar_t)                              :: matvar
    CHARACTER(1),INTENT(OUT),DIMENSION(:,:,:)   :: d
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: start
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: stride
    INTEGER(4),INTENT(IN),DIMENSION(*),OPTIONAL :: edge
    INTEGER,EXTERNAL                            :: fmat_varreaddata_c

    IF ( PRESENT(start) .AND. PRESENT(stride) .AND. PRESENT(edge) ) THEN
        err = fmat_varreaddata_c(mat,matvar,d,start,stride,edge)
    ELSEIF ( PRESENT(start) .OR. PRESENT(stride) .OR. PRESENT(edge) ) THEN
        err = 1    ! Must have all 3 or none
    ELSE
        err = fmat_varreaddata_c(mat,matvar,d,nullptr,nullptr,nullptr)
    ENDIF
END FUNCTION FMat_VarReadCharData_3
