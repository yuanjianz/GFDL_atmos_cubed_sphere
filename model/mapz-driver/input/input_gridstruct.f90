module input_gridstruct_mod

  use fv_arrays_mod, only: fv_grid_type
  use input_array_extents_mod, only: ArrayExtents_T
  
  implicit none
  
  private

  public get_input_gridstruct
  
contains

  subroutine get_input_gridstruct(filename, arrext, gridstruct)
  
    ! Arguments
    character(len=*), intent(in) :: filename
    type(ArrayExtents_T), intent(in) :: arrext
    type(fv_grid_type), intent(out) :: gridstruct

    ! Locals
    integer :: file_handle
    
    ! Start
    print *, 'Reading gridstruct data from file: ', trim(filename)

    allocate(gridstruct%rsin2(arrext%isd:arrext%ied, arrext%jsd:arrext%jed))
    allocate(gridstruct%cosa_s(arrext%isd:arrext%ied, arrext%jsd:arrext%jed))
    allocate(gridstruct%area_64(arrext%isd:arrext%ied, arrext%jsd:arrext%jed))
    
    open(newunit = file_handle, file = filename, form = 'unformatted', status = 'old')
    
    read(file_handle) gridstruct%rsin2
    read(file_handle) gridstruct%cosa_s
    read(file_handle) gridstruct%area_64

    close(file_handle)
    
  end subroutine get_input_gridstruct

end module input_gridstruct_mod
