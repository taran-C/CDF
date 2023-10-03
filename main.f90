program main

	use main_loop
	use configuration

	implicit none

	type(t_config) :: conf
	conf = t_config([3,3,3])

	call time_loop(conf)

end program