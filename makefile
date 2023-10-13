FC = gfortran

OBJS = mod_model.o mod_grid.o configuration.o mod_io.o main_loop.o main.o
PROG = lorenz

all: $(PROG)

$(PROG): $(OBJS)
	gfortran -o $@ $^ `pkg-config --cflags --libs netcdf-fortran`

$(OBJS): %.o: %.f90
	gfortran -c -o $@ $< `pkg-config --cflags --libs netcdf-fortran`

clean:
	rm -f *.o *.mod