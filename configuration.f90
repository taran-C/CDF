module configuration

	use mod_grid
	use mod_model

	implicit none

	integer :: DEFAULT_STEPS = 1000

	type :: t_config
	!Type containing the different parts of our data (model, initial conditions grid...) as well as run specific data (number of steps...)
		type(t_model):: model
		type(t_grid):: init
		integer :: steps
		character (len = 100):: file_name
		integer :: ncid
		integer, allocatable :: ncvars
	end type

	interface t_config
		module procedure :: config_constructor
	end interface t_config

contains

	function config_constructor(grid_sizes) result(config)

		type(t_config) :: config
		type(t_model) :: model

		!stable starting point
		!real, parameter :: x0 = (-sqrt(beta*(rho-1.))), y0 = (-sqrt(beta*(rho-1.))), z0 = (rho-1.)
		integer :: i,j,k
		integer :: grid_sizes(3)

		real, parameter :: max_coord = 10., min_coord = -10.
		type(t_grid) :: init

		init = t_grid(grid_sizes)

		config%steps = DEFAULT_STEPS

		model = get_model_lorenz()

		!grid_init
		do i=1, grid_sizes(1)
			do j=1, grid_sizes(2)
				do k=1, grid_sizes(3)

					init%data(1,i,j,k) = min_coord+(max_coord-min_coord)*(real(i)/real(grid_sizes(1)))
					init%data(2,i,j,k) = min_coord+(max_coord-min_coord)*(real(j)/real(grid_sizes(2)))
					init%data(3,i,j,k) = min_coord+(max_coord-min_coord)*(real(k)/real(grid_sizes(3)))

				end do
			end do
		end do

		config%model = model
		config%init = init
		config%file_name = "a.nc"

	end function config_constructor

end module configuration