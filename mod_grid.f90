module mod_grid

	use mod_model

	type :: t_grid
	!The complete grid that we iterate on
		real, allocatable :: data(:,:,:,:)
		integer :: varnum
		integer :: sizes(3)
	contains
		procedure, pass(self) :: grid_step
	end type

	interface t_grid
		module procedure :: grid_constructor
	end interface t_grid

contains

	function grid_constructor(grid_sizes) result(grid)
		integer :: grid_sizes(3)
		type(t_grid) :: grid

		grid%varnum = 3
		allocate(grid%data(grid%varnum,grid_sizes(1),grid_sizes(2),grid_sizes(3)))
		grid%sizes = grid_sizes
	end function grid_constructor

	subroutine grid_step(self, model)
	!Update each point with the model's update function

		class(t_grid) :: self
		type(t_model), intent(in) :: model
		integer :: i,j,k
		real :: res(self%varnum)


		do concurrent (i=1:self%sizes(1))
			do concurrent (j=1:self%sizes(2))
				do concurrent(k=1:self%sizes(3))
					self%data(:,i,j,k) = model%update_func(model,self%data,i,j,k,self%varnum,self%sizes)
				end do
			end do
		end do
	
	end subroutine grid_step

end module mod_grid