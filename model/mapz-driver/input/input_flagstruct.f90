module input_flagstruct_mod
  
  use fv_arrays_mod, only: fv_flags_type
  
  implicit none
  
  private
  
  public get_input_flagstruct
  
contains
  
  subroutine get_input_flagstruct(filename, flagstruct)
    
    ! Arguments
    character(len=*), intent(in) :: filename
    type(fv_flags_type), intent(out) :: flagstruct
    
    ! Locals
    integer :: file_handle
    
    ! Start
    print *, 'Reading flagstruct data from file: ', trim(filename)

    open(newunit = file_handle, file = filename, form = 'unformatted', status = 'old')
    
    read(file_handle) flagstruct%fv_debug

    close(file_handle)
    
  end subroutine get_input_flagstruct

end module input_flagstruct_mod
