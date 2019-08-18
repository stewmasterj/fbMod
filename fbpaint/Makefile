# GNU Makefile
#
.SUFFIXES: .o .f90
FC=gfortran
CC=gcc
#FCFLAGS= -Ofast -march=native -funsafe-math-optimizations
#FCFLAGS= -O3
FCFLAGS= -O -g -fbounds-check -Wuninitialized -Wsurprising -Wall -Wextra
FCFLAGS= -fcheck=all -ffpe-trap=invalid,zero,overflow -g -Wall -Wextra -Werror #-pedantic
#FCFLAGS += -I/usr/include
FOMP= #-fopenmp

PROGRAM = fbpaint

## "make" builds all
#all: $(PROGRAM)

# list all source files
OBJ = kbhit.o fcurses.o fbMod2.o ArgsMod.o fbpaint.o

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
${PROGRAM}:${OBJ}
	$(FC) -o ${PROGRAM} ${FOMP} ${OBJ} ${FCFLAGS}

# to clean up stuff
clean:
	rm -f *.o *.mod *.MOD
veryclean: clean
	rm -f *~ $(PROGRAMS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
kbhit.o:kbhit.c
	$(CC) -c $<
fbpaint.o:fbpaint.f90
	$(FC) $(FCFLAGS) $(FOMP) -c fbpaint.f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<


