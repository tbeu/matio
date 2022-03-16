!----------------------------------------------------------
!   FMat_VarReadData Interface procedures
!
!   This files should be included on the MATIO module
!----------------------------------------------------------
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
