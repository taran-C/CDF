module main_loop

	use configuration
	use mod_grid
	use mod_model
	use mod_IO

	implicit none

contains

	subroutine time_loop(config, nc_file)
	!Main time loop, at each step we update the grid, then save it to a file.

		type(t_config), intent(in) :: config
		type(t_nc_file) :: nc_file
		type(t_grid) :: grid

		integer :: n

		grid = config%init

		call write_nc(config, nc_file, grid)
		do n = 1, config%steps-1, 1

			call grid%grid_step(config%model)

			call write_nc(config, nc_file, grid)
		end do

	end subroutine time_loop

end module main_loop