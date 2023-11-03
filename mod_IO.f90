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

	function parse_arguments() result(conf)

		type(t_config) :: conf
		character(len = 32) :: arg
		integer :: i
		integer :: stat
		integer :: int_val

		conf = t_config([3,3,3])

		i = 1
	    do while (i <= command_argument_count())
	        call get_command_argument(i, arg)

	        select case (arg)
	            case ('-s', '--steps')
	            	call get_command_argument(i+1, arg)
	            	read(arg,*,iostat=stat) int_val
	                if (stat /= 0) then
	                	print *, "arg error"
	                	error stop
	                end if
	                i = i+1
	                conf%steps = int_val

	            case ('-o', '--out')
	            	call get_command_argument(i+1, arg)
	            	conf%file_name = arg
	            	i=i+1

	            case ('-h', '--help')
	                call print_help()
	                stop

	            case default
	                print '(2a, /)', 'unrecognised command-line option: ', arg
	                call print_help()
	                stop
	        end select
	        i=i+1
	    end do

	end function parse_arguments

	subroutine print_help()

		print '(a, /)', 'command-line options:'
        print '(a)',    '  -s, --steps       number of time steps to integrate'
        print '(a)',    '  -o, --out         file name for the output file (*.nc)'
        print '(a, /)', '  -h, --help        print usage information and exit'

	end subroutine print_help

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