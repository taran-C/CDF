program main

	use main_loop
	use configuration
	use mod_io

	implicit none

	type(t_config) :: conf
	type(t_nc_file) :: nc_file

	conf = parse_arguments()

	nc_file = create_nc(conf)

	call time_loop(conf, nc_file)

	call close_nc(nc_file)

end program