module input_arrays_mod

  ! use fv_arrays_mod, only: fv_flags_type, fv_grid_type
  use input_array_extents_mod, only: ArrayExtents_T, get_input_array_extents

  implicit none
  
  private

  public Arrays_T, get_input_arrays

  type Arrays_T
     real, allocatable :: ps(:, :)
     real, allocatable :: pe(:, :, :)
     real, allocatable :: delp(:, :, :)
     real, allocatable :: pkz(:, :, :)
     real, allocatable :: pk(:, :, :)
     real, allocatable :: q_con(:, :, :)
     real, allocatable :: u(:, :, :) , v(:, :, :), w(:, :, :)
     real, allocatable :: delz(:, :, :)
     real, allocatable :: pt(:, :, :)
     real, allocatable :: q(:, :, :, :) ! this is the assumed shape array
     real, allocatable :: hs(:, :)
     real, allocatable :: cappa(:, :, :)
     integer, allocatable :: kord_tr(:)
     real, allocatable :: peln(:, :, :)
     real, allocatable :: te0_2d(:, :)
     real, allocatable :: ua(:, :, :), va(:, :, :)
     real, allocatable :: omga(:, :, :)
     real, allocatable :: te(:, :, :)
     real, allocatable :: ws(:, :)     
     real, allocatable :: dtdt(:, :, :)
     real, allocatable :: ak(:)
     real, allocatable :: bk(:)
     real, allocatable :: pfull(:)
     real, allocatable :: mfx(:, :, :), mfy(:, :, :)
     real, allocatable :: cx(:, :, :), cy(:, :, :)
  end type Arrays_T

  
contains

  subroutine allocate_memory_for_arrays_(arrext, arrays)

    ! Arguments
    type(ArrayExtents_T), intent(in) :: arrext
    type(Arrays_T), intent(inout) :: arrays
    integer :: is, ie, isd, ied, js, je, jsd, jed, km, nq

    ! Start

    ! Shorthands
    is = arrext%is; ie = arrext%ie
    isd = arrext%isd; ied = arrext%ied
    js = arrext%js; je = arrext%je
    jsd = arrext%jsd; jed = arrext%jed
    km = arrext%km
    nq = arrext%nq
    
    ! Now allocate memory
    allocate(arrays%ps(isd:ied, jsd:jed))
    allocate(arrays%pe(is-1:ie+1, km+1, js-1:je+1))
    allocate(arrays%delp(isd:ied, jsd:jed, km))
    allocate(arrays%pkz(is:ie, js:je, km))
    allocate(arrays%pk(is:ie, js:je, km+1))
    allocate(arrays%q_con(isd:isd, jsd:jsd, 1:1))
    allocate(arrays%u(isd:ied, jsd:jed+1, km))
    allocate(arrays%v(isd:ied+1, jsd:jed, km))
    allocate(arrays%w(isd:isd, jsd:jsd, 1:1))
    allocate(arrays%delz, mold = arrays%q_con)
    allocate(arrays%pt(isd:ied, jsd:jed, km))
    allocate(arrays%q(isd:ied, jsd:jed, km, nq))
    allocate(arrays%hs(isd:ied, jsd:jed))
    allocate(arrays%cappa, mold = arrays%q_con)
    allocate(arrays%kord_tr(nq))
    allocate(arrays%peln(is:ie, km+1, js:je))
    allocate(arrays%te0_2d(is:ie, js:je))
    allocate(arrays%ua(isd:ied, jsd:jed, km))
    allocate(arrays%va, mold = arrays%ua)
    allocate(arrays%omga, mold = arrays%ua)
    allocate(arrays%te(isd:ied, jsd:jed, km))
    allocate(arrays%ws(is:ie, js:je))
    allocate(arrays%dtdt(is:ie, js:je, km))
    allocate(arrays%ak(km+1))
    allocate(arrays%bk, mold = arrays%ak)
    allocate(arrays%pfull(km))
    allocate(arrays%mfx(is:ie+1, js:je, km))
    allocate(arrays%mfy(is:ie, js:je+1, km))
    allocate(arrays%cx(is:ie+1, jsd:jed, km))
    allocate(arrays%cy(isd:ied, js:je+1, km))
    
  end subroutine allocate_memory_for_arrays_
  
  subroutine get_input_arrays(filename, arrext, arrays)
  
    ! Arguments
    character(len=*), intent(in) :: filename
    type(ArrayExtents_T), intent(in) :: arrext
    type(Arrays_T), intent(out) :: arrays

    ! Locals
    integer :: is, ie, isd, ied, js, je, jsd, jed, km, nq
    integer :: file_handle
    
    ! Start
    print *, 'Reading array data from file: ', trim(filename)

    ! Allocate memory for all arrays
    call allocate_memory_for_arrays_(arrext, arrays)
    
    open(newunit = file_handle, file = filename, form = 'unformatted', status = 'old')

    read(file_handle) arrays%ps
    read(file_handle) arrays%pe
    read(file_handle) arrays%delp
    read(file_handle) arrays%pkz
    read(file_handle) arrays%pk
    read(file_handle) arrays%q_con
    read(file_handle) arrays%u
    read(file_handle) arrays%v
    read(file_handle) arrays%w
    read(file_handle) arrays%delz
    read(file_handle) arrays%pt
    read(file_handle) arrays%q(:,:,:,1)
    read(file_handle) arrays%hs
    read(file_handle) arrays%cappa
    read(file_handle) arrays%kord_tr
    read(file_handle) arrays%peln
    read(file_handle) arrays%te0_2d
    read(file_handle) arrays%ua
    read(file_handle) arrays%va
    read(file_handle) arrays%omga
    read(file_handle) arrays%te
    read(file_handle) arrays%ws
    read(file_handle) arrays%dtdt
    read(file_handle) arrays%ak
    read(file_handle) arrays%bk
    read(file_handle) arrays%pfull
    read(file_handle) arrays%mfx
    read(file_handle) arrays%mfy
    read(file_handle) arrays%cx
    read(file_handle) arrays%cy
    
    close(file_handle)

  end subroutine get_input_arrays

end module input_arrays_mod
