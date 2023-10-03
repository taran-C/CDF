FC = gfortran

OBJS = mod_model.o mod_grid.o configuration.o main_loop.o main.o
PROG = lorenz

all: $(PROG)

$(PROG): $(OBJS)
	gfortran -o $@ $^

$(OBJS): %.o: %.f90
	gfortran -c -o $@ $<

clean:
	rm -f *.o *.mod