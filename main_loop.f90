module main_loop

	use configuration
	use mod_grid
	use mod_model

	implicit none

contains

	subroutine time_loop(config)
	!Main time loop, at each step we update the grid, then save it to a file.

		type(t_config), intent(in) :: config

		type(t_grid) :: grid

		integer :: n

		grid = config%init

		open(1, file = "res.txt")
		call grid%print_to_file(1)
		do n = 1, config%steps, 1

			call grid%grid_step(config%model)

			call grid%print_to_file(1)
		end do

		close(1)

	end subroutine time_loop

end module main_loop