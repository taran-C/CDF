module configuration

	use mod_grid
	use mod_model

	implicit none

	type :: t_config
	!Type containing the different parts of our data (model, initial conditions grid...) as well as run specific data (number of steps...)
		type(t_model):: model
		type(t_grid):: init
		integer :: steps
	end type

	interface t_config
		module procedure :: config_constructor
	end interface t_config

contains

	function config_constructor(grid_sizes) result(config)

		type(t_config) :: config
		type(t_model) :: model

		real, parameter :: sigma = 10., rho = 28., beta = 8./3.
		!stable starting point
		!real, parameter :: x0 = (-sqrt(beta*(rho-1.))), y0 = (-sqrt(beta*(rho-1.))), z0 = (rho-1.)
		integer :: i,j,k
		integer :: grid_sizes(3)

		real, parameter :: max_coord = 10., min_coord = -10.
		type(t_grid) :: init

		init = t_grid(grid_sizes)

		config%steps = 10000

		model = t_model([sigma,rho,beta], 0.01)

		!grid_init
		do i=1, grid_sizes(1)
			do j=1, grid_sizes(2)
				do k=1, grid_sizes(3)

					init%points(i,j,k) = t_grid_point([min_coord+(max_coord-min_coord)*(real(i)/real(grid_sizes(1))), &
							min_coord+(max_coord-min_coord)*(real(j)/real(grid_sizes(2))), &
							min_coord+(max_coord-min_coord)*(real(k)/real(grid_sizes(3)))])

				end do
			end do
		end do

		config%model = model
		config%init = init

	end function config_constructor

end module configuration