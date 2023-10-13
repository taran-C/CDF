module mod_io
	
	use netcdf
	use configuration

	type :: t_nc_file
		integer ::ncid
		integer, allocatable :: varid(:)
		integer, allocatable :: dimids(:)
		integer :: step
		integer :: NDIMS
	end type

contains

	function create_nc(config) result(nc_file)

		type(t_config) :: config
  		type(t_nc_file) nc_file

  		integer :: x_dimid, y_dimid, z_dimid, t_dimid

  		nc_file%NDIMS = 4
  		allocate(nc_file%varid(config%model%var_num))
  		allocate(nc_file%dimids(nc_file%NDIMS))

  		!create the file
  		call check( nf90_create(config%file_name, NF90_CLOBBER, nc_file%ncid) )

  		!define dims
  		call check( nf90_def_dim(nc_file%ncid, "x", config%init%sizes(1), x_dimid) )
 		call check( nf90_def_dim(nc_file%ncid, "y", config%init%sizes(2), y_dimid) )
 		call check( nf90_def_dim(nc_file%ncid, "z", config%init%sizes(3), z_dimid) )
 		call check( nf90_def_dim(nc_file%ncid, "t", config%steps, t_dimid) )

 		nc_file%dimids =  (/ y_dimid, x_dimid, z_dimid, t_dimid /)

 		do i = 1, config%model%var_num, 1
 			call check( nf90_def_var(nc_file%ncid, config%model%var_names(i), NF90_FLOAT, nc_file%dimids, nc_file%varid(i)) )
 		end do

 		call check( nf90_enddef(nc_file%ncid) )

 		nc_file%step = 1

	end function create_nc

	subroutine write_nc(config, nc_file, grid)

		class(t_grid), intent(in) :: grid
		type(t_nc_file) :: nc_file
		type(t_config) :: config
		integer :: start(nc_file%NDIMS), count(nc_file%NDIMS)

		start = (/1,1,1,nc_file%step/)
		count = (/grid%sizes(1),grid%sizes(2),grid%sizes(3),1/)
		do i = 1, config%model%var_num, 1
			call check( nf90_put_var(nc_file%ncid, nc_file%varid(i), grid%data(i,:,:,:), start = start, count = count) )
		end do
		nc_file%step = nc_file%step+1
	end subroutine write_nc

	subroutine close_nc(nc_file)
		type(t_nc_file) :: nc_file
  		call check( nf90_close(nc_file%ncid) )
	end subroutine close_nc

	subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

end module mod_IO