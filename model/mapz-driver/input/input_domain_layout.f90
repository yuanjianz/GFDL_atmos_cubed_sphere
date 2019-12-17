module input_domain_layout_mod

  implicit none
  
  private
  
  public get_npx_npy, get_domain_layout
  
contains

  subroutine get_domain_layout(filename, nx, ny)
    
    ! Arguments
    character(len=*), intent(in) :: filename
    integer, intent(out) :: nx
    integer, intent(out) :: ny

    ! Locals
    integer :: file_handle
    namelist /layout_nml/ nx, ny
    
    ! Start
    open(newunit = file_handle, file = filename, status = 'old')
    read(file_handle, nml = layout_nml)
    close(file_handle)
    
  end subroutine get_domain_layout
  
  subroutine get_npx_npy(filename, npx, npy)

    ! Arguments
    character(len=*) filename
    integer :: npx, npy

    ! Locals
    integer :: file_handle
    integer :: npz, n_sponge, fv_sg_adj, k_split, n_split
    integer :: hord_mt, hord_vt, hord_tm, hord_dp, hord_tr
    logical :: adiabatic, hydrostatic, make_nh, fv_debug, do_vort_damp
    real :: vtdm4, d_con
    namelist /fv_core_nml/ &
         npx, npy, npz, n_sponge, fv_sg_adj, adiabatic, hydrostatic, make_nh, &
         fv_debug, k_split, n_split, vtdm4, do_vort_damp, d_con, hord_mt, &
         hord_vt, hord_tm, hord_dp, hord_tr

    ! Start
    open(newunit = file_handle, file = filename, status = 'old')
    read(file_handle, nml = fv_core_nml)
    close(file_handle)
    
  end subroutine get_npx_npy

end module input_domain_layout_mod
