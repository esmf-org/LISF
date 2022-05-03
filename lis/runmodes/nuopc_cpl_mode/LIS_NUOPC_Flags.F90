!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.3
!
! Copyright (c) 2020 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#define FILENAME "lis_nuopc_flags"
#define MODNAME "lis_nuopc_flags"
#include "LIS_NUOPC_Macros.h"

module LIS_NUOPC_Flags
!BOP
!
! !MODULE: LIS_NUOPC_Flags
!
! !DESCRIPTION:
!   This module contains configuration setting flags
!
! !REVISION HISTORY:
!  2022May02    Dan Rosen  Added
!
! !USES:
  use ESMF, only: ESMF_UtilStringUpperCase, ESMF_SUCCESS
  IMPLICIT NONE

  PRIVATE

!-----------------------------------------------------------------------------
! !FLAG TYPES AND VALUES
!-----------------------------------------------------------------------------

  type missingval_flag
    sequence
    private
      integer :: opt
  end type missingval_flag

  type(missingval_flag), parameter ::        &
    MISSINGVAL_ERROR  = missingval_flag(-1), &
    MISSINGVAL_IGNORE = missingval_flag(0),  &
    MISSINGVAL_FAIL   = missingval_flag(1),  &
    MISSINGVAL_SKPCPY = missingval_flag(2)

!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------

  public missingval_flag
  public MISSINGVAL_ERROR
  public MISSINGVAL_IGNORE
  public MISSINGVAL_FAIL
  public MISSINGVAL_SKPCPY

  public operator(==), assignment(=)

!-----------------------------------------------------------------------------
! !INTERFACE DEFINITIONS:
!-----------------------------------------------------------------------------

  interface operator (==)
    module procedure missingval_eq
  end interface

  interface assignment (=)
    module procedure missingval_toString
    module procedure missingval_frString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "missingval_eq"
  function missingval_eq(val1, val2)
    logical missingval_eq
    type(missingval_flag), intent(in) :: val1, val2
    missingval_eq = (val1%opt == val2%opt)
  end function missingval_eq

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "missingval_toString"
  subroutine missingval_toString(string, val)
    character(len=*), intent(out) :: string
    type(missingval_flag), intent(in) :: val
    if (val == MISSINGVAL_IGNORE) then
      write(string,'(a)') 'MISSINGVAL_IGNORE'
    elseif (val == MISSINGVAL_FAIL) then
      write(string,'(a)') 'MISSINGVAL_FAIL'
    elseif (val == MISSINGVAL_SKPCPY) then
      write(string,'(a)') 'MISSINGVAL_SKPCPY'
    else
      write(string,'(a)') 'MISSINGVAL_ERROR'
    endif
  end subroutine missingval_toString

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "missingval_frString"
  subroutine missingval_frString(val, string)
    type(missingval_flag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=32) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (rc .ne. ESMF_SUCCESS) then
      val = MISSINGVAL_ERROR
    elseif (ustring .eq. 'MISSINGVAL_IGNORE') then
      val = MISSINGVAL_IGNORE
    elseif (ustring .eq. 'MISSINGVAL_FAIL') then
      val = MISSINGVAL_FAIL
    elseif (ustring .eq. 'MISSINGVAL_SKPCPY') then
      val = MISSINGVAL_SKPCPY
    else
      val = MISSINGVAL_ERROR
    endif
  end subroutine missingval_frString

  !-----------------------------------------------------------------------------

end module
