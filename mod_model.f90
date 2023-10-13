module mod_model

	type t_model
	!Contains the information about our model (parameters...) as well as the update function used to go from one time step to the next
		real :: params(3)
		integer :: var_num
		character(len = 10), allocatable :: var_names(:)
		real :: delta_t
		procedure(update_lorenz), pointer, nopass :: update_func

	end type

contains

	!Lorenz model
	function get_model_lorenz() result(model)
		type(t_model) :: model
		real, parameter :: sigma = 10., rho = 28., beta = 8./3., delta_t = 0.01

		model%params = [sigma,rho,beta]
		model%var_num = 3
		allocate(model%var_names(model%var_num))
		model%var_names = ["x(t)","y(t)","z(t)"]
		model%delta_t = delta_t
		model%update_func=>update_lorenz

	end function get_model_lorenz

	pure function update_lorenz(self, data, i, j, k, grid_varnum, grid_sizes) result(res)
		integer, intent(in) :: grid_sizes(3)
		integer, intent(in) :: grid_varnum
		real, intent(in) :: data(grid_varnum,grid_sizes(1),grid_sizes(2),grid_sizes(3))
		integer, intent(in) :: i,j,k
		real :: res(grid_varnum)
		class(t_model), intent(in) :: self

		res(1) = self%delta_t * self%params(1) * (data(2,i,j,k) - data(1,i,j,k)) + data(1,i,j,k)
		res(2) = self%params(2) * self%delta_t * data(1,i,j,k) + data(2,i,j,k)*(1.-self%delta_t) &
					- self%delta_t * data(1,i,j,k) * data(3,i,j,k)
		res(3) = self%delta_t * data(1,i,j,k) * data(2,i,j,k) + data(3,i,j,k)*(1. - self%delta_t*self%params(3))

	end function update_lorenz

end module mod_model