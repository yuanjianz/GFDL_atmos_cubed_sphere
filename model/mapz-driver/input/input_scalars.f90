module input_scalars_mod

  implicit none
  
  private

  public Scalars_T, get_input_scalars

  type Scalars_T
     ! All of these are inputs to Lagrangian_to_Eulerian
     logical :: last_step
     real :: consv
     real :: mdt
     real :: pdt
     integer :: nwat
     integer :: sphum
     real :: r_vir
     real :: cp
     real :: akap
     integer :: kord_mt
     integer :: kord_wz
     integer :: kord_tm
     integer :: ng
     logical :: fill
     logical :: reproduce_sum
     logical :: out_dt
     real :: ptop
     logical :: do_sat_adj
     logical :: hydrostatic
     logical :: hybrid_z
     logical :: do_omega
     logical :: adiabatic
     logical :: do_adiabatic_init
     integer :: remap_option
  end type Scalars_T

contains

  subroutine get_input_scalars(filename, scalars)

    ! Arguments
    character(len=*), intent(in) :: filename
    type(Scalars_T), intent(out) :: scalars

    ! Locals
    integer :: file_handle
    
    ! Start
    print *, 'Reading scalar from file: ', trim(filename)

    open(newunit = file_handle, file = filename, form = 'unformatted', status = 'old')

    read(file_handle) scalars%last_step
    read(file_handle) scalars%consv
    read(file_handle) scalars%mdt
    read(file_handle) scalars%pdt
    read(file_handle) scalars%nwat
    read(file_handle) scalars%sphum
    read(file_handle) scalars%r_vir
    read(file_handle) scalars%cp
    read(file_handle) scalars%akap
    read(file_handle) scalars%kord_mt
    read(file_handle) scalars%kord_wz
    read(file_handle) scalars%kord_tm
    read(file_handle) scalars%ng
    read(file_handle) scalars%fill
    read(file_handle) scalars%reproduce_sum
    read(file_handle) scalars%out_dt
    read(file_handle) scalars%ptop
    read(file_handle) scalars%do_sat_adj
    read(file_handle) scalars%hydrostatic
    read(file_handle) scalars%hybrid_z
    read(file_handle) scalars%do_omega
    read(file_handle) scalars%adiabatic
    read(file_handle) scalars%do_adiabatic_init
    read(file_handle) scalars%remap_option

    close(file_handle)

  end subroutine get_input_scalars

end module input_scalars_mod
