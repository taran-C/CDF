module mod_grid

	use mod_model

	type :: t_grid_point
	!Data contained in each grid points, e.g velocity components, tracers...
		real :: data(3)
	end type


	type :: t_grid
	!The complete grid that we iterate on
		type(t_grid_point), allocatable :: points(:,:,:)
		integer :: sizes(3)
	contains
		procedure, pass(self) :: print_to_file
		procedure, pass(self) :: grid_step
	end type

	interface t_grid
		module procedure :: grid_constructor
	end interface t_grid

contains

	function grid_constructor(grid_sizes) result(grid)
		integer :: grid_sizes(3)
		type(t_grid) :: grid

		allocate(grid%points(grid_sizes(1),grid_sizes(2),grid_sizes(3)))
		grid%sizes = grid_sizes
	end function grid_constructor

	subroutine grid_step(self, model)
	!Update each point with the model's update function

		class(t_grid) :: self
		type(t_model), intent(in) :: model
		integer :: i,j,k

		do concurrent (i=1:self%sizes(1))
			do concurrent (j=1:self%sizes(2))
				do concurrent(k=1:self%sizes(3))
					self%points(i,j,k)%data = model%update_func(model,self%points(i,j,k)%data)
				end do
			end do
		end do
	
	end subroutine grid_step

	subroutine print_to_file(self, file)

		class(t_grid), intent(in) :: self
		integer, intent(in):: file

		integer :: i,j,k

		write(file, '(A)', advance="no") '['
		do i=1, self%sizes(1)

			write(file, '(A)', advance="no") '['
			do j=1, self%sizes(2)

				write(file, '(A)', advance="no") '['
				do k=1, self%sizes(3)

					write(file, '(A)', advance="no") '['
					write(file, '(F10.5 , A, F10.5 , A, F10.5)', advance='no') self%points(i,j,k)%data(1), ',', &
							self%points(i,j,k)%data(2), ',', self%points(i,j,k)%data(3)
					if (k<self%sizes(3)) then
						write(file, '(A)', advance="no") '],'
					else
						write(file, '(A)', advance="no") ']'
					end if
				end do
				if (j<self%sizes(2)) then
					write(file, '(A)', advance="no") '],'
				else
					write(file, '(A)', advance="no") ']'
				end if

			end do
			if (i<self%sizes(1)) then
				write(file, '(A)', advance="no") '],'
			else
				write(file, '(A)', advance="no") ']'
			end if

		end do
		write(file, *) ']' !newline
	end subroutine print_to_file

end module mod_grid