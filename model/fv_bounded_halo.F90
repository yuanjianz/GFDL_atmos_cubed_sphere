!> GCHP regional: halo fills on a bounded (regional) domain.
!!
!! On a single tile without contacts, no PE lies beyond the edge of the
!! domain. FMS halo updates leave the halo there untouched, and
!! mpp_get_boundary, which the global code uses for the faces each PE shares
!! with its east/north neighbor, exchanges nothing at all. These routines fill
!! such halos instead. Everything outside the domain gets the value of the
!! nearest point inside it (copy-nearest, i.e. zero gradient); with a buffer
!! zone of boundary conditions inside the domain, errors from this stay in
!! the buffer.
!!
!! Arrays are on the data domain of bd, in FV3's precision (default real),
!! with global indices: cells 1..npx-1 by 1..npy-1 inside the domain.
module fv_bounded_halo_mod

  use fv_arrays_mod,     only: fv_grid_bounds_type
  use mpp_domains_mod,   only: domain2d, mpp_update_domains
  use mpp_parameter_mod, only: EAST, NORTH

  implicit none
  private

  public :: bounded_copy_nearest, bounded_fill_faces

contains

  !> Copy-nearest outside the domain, for a field at the given position
  !! (CENTER: cells, EAST: x faces, NORTH: y faces, as in FMS). Fills along x
  !! first, then along y over all columns, which fills the domain corners.
  !! Needs a completed halo update, so that the parts of the edge row or
  !! column owned by neighboring PEs are in place. For an array of another
  !! rank, pass its first element and the product of the trailing dimensions
  !! as nk.
  subroutine bounded_copy_nearest(a, bd, npx, npy, nk, position)
    type(fv_grid_bounds_type), intent(IN) :: bd
    integer, intent(IN) :: npx, npy, nk, position
    real, intent(INOUT) :: a(bd%isd:bd%ied+merge(1,0,position==EAST), &
                             bd%jsd:bd%jed+merge(1,0,position==NORTH), nk)
    integer :: i, j, ilast, jlast

    ilast = npx - 1
    jlast = npy - 1
    if (position == EAST)  ilast = npx
    if (position == NORTH) jlast = npy

    do i = lbound(a,1), 0
       a(i,:,:) = a(1,:,:)
    enddo
    do i = ilast+1, ubound(a,1)
       a(i,:,:) = a(ilast,:,:)
    enddo
    do j = lbound(a,2), 0
       a(:,j,:) = a(:,1,:)
    enddo
    do j = jlast+1, ubound(a,2)
       a(:,j,:) = a(:,jlast,:)
    enddo

  end subroutine bounded_copy_nearest

  !> Halo of a field on the x faces (x) and one on the y faces (y): C-grid
  !! winds or fluxes (uc, vc), or D-grid winds (v, u). x(i,j) is on the west
  !! face and y(i,j) on the south face of cell (i,j). On entry the compute
  !! domain of both is set; this fills the rest:
  !!   1. the faces shared with the east/north neighbor, from a cell-centered
  !!      halo update (each cell holds its west/south face, so the neighbor's
  !!      first face lands at ie+1/je+1);
  !!   2. the faces on the east/north edge of the domain, by copy-nearest,
  !!      unless edge_set says the caller has set them;
  !!   3. the rest of the halo between PEs, from staggered halo updates (the
  !!      domain is symmetric);
  !!   4. everything outside the domain, by copy-nearest.
  subroutine bounded_fill_faces(x, y, bd, domain, npx, npy, nk, edge_set)
    type(fv_grid_bounds_type), intent(IN) :: bd
    type(domain2d), intent(INOUT) :: domain
    integer, intent(IN) :: npx, npy, nk
    real, intent(INOUT) :: x(bd%isd:bd%ied+1,bd%jsd:bd%jed  ,nk)
    real, intent(INOUT) :: y(bd%isd:bd%ied  ,bd%jsd:bd%jed+1,nk)
    logical, intent(IN), optional :: edge_set
    real :: xc(bd%isd:bd%ied,bd%jsd:bd%jed,nk)
    real :: yc(bd%isd:bd%ied,bd%jsd:bd%jed,nk)
    logical :: keep_edge
    integer :: is, ie, js, je

    is = bd%is ; ie = bd%ie
    js = bd%js ; je = bd%je
    keep_edge = .false.
    if (present(edge_set)) keep_edge = edge_set

    xc = 0.
    yc = 0.
    xc(is:ie,js:je,:) = x(is:ie,js:je,:)
    yc(is:ie,js:je,:) = y(is:ie,js:je,:)
    call mpp_update_domains(xc, domain, complete=.false.)
    call mpp_update_domains(yc, domain, complete=.true.)
    if (ie+1 < npx) then
       x(ie+1,js:je,:) = xc(ie+1,js:je,:)
    else if (.not. keep_edge) then
       x(npx,js:je,:) = x(npx-1,js:je,:)
    endif
    if (je+1 < npy) then
       y(is:ie,je+1,:) = yc(is:ie,je+1,:)
    else if (.not. keep_edge) then
       y(is:ie,npy,:) = y(is:ie,npy-1,:)
    endif

    call mpp_update_domains(x, domain, position=EAST)
    call mpp_update_domains(y, domain, position=NORTH)

    call bounded_copy_nearest(x, bd, npx, npy, nk, EAST)
    call bounded_copy_nearest(y, bd, npx, npy, nk, NORTH)

  end subroutine bounded_fill_faces

end module fv_bounded_halo_mod
