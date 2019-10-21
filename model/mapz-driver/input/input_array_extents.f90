module input_array_extents_mod

  implicit none
  
  private

  public ArrayExtents_T, get_input_array_extents

  type ArrayExtents_T
     integer :: is, ie, isd, ied
     integer :: js, je, jsd, jed
     integer :: km
     integer :: nq
  end type ArrayExtents_T
  
contains

  subroutine get_input_array_extents(filename, arrext)

    ! Arguments
    character(len=*), intent(in) :: filename
    type(ArrayExtents_T), intent(out) :: arrext

    ! Locals
    integer :: file_handle
    
    ! Start
    print *, 'Reading array extents from file: ', trim(filename)

    open(newunit = file_handle, file = filename, form = 'unformatted', status = 'old')

    read(file_handle) arrext%is
    read(file_handle) arrext%ie
    read(file_handle) arrext%js
    read(file_handle) arrext%je
    read(file_handle) arrext%isd
    read(file_handle) arrext%ied
    read(file_handle) arrext%jsd
    read(file_handle) arrext%jed
    read(file_handle) arrext%nq
    read(file_handle) arrext%km

  end subroutine get_input_array_extents

end module input_array_extents_mod
