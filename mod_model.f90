module mod_model

	type t_model
	!Contains the information about our model (parameters...) as well as the update function used to go from one time step to the next
		real :: params(3)
		real :: delta_t
	contains

		procedure, pass(self) :: update

	end type

contains

	pure function update(self, data) result(res)
		real, intent(in) :: data(3)
		real :: res(3)
		class(t_model), intent(in) :: self

		res(1) = self%delta_t * self%params(1) * (data(2) - data(1)) + data(1)
		res(2) = self%params(2) * self%delta_t * data(1) &
				+ data(2)*(1.-self%delta_t) - self%delta_t * data(1) * data(3)
		res(3) = self%delta_t * data(1) * data(2) + data(3)*(1. - self%delta_t*self%params(3))

	end function update

end module mod_model