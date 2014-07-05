COMP=gnu

ifeq ($(COMP),cray)
CC=cc 
FC=ftn
CCFLAGS=-g 
FCFLAGS=-g
RUN=
endif

ifeq ($(COMP),gnu)
CC=gcc
FC=gfortran
CCFLAGS=-g
FCFLAGS=-g -dynamic -fopenmp
RUN=
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
CCFLAGS=-g
FCFLAGS=-g
RUN=
endif

EXE6=ex6
OBJS6=pngwriter.o heat.o ex6.o 

EXES=$(EXE6)

all: $(EXES)

# Dependencies
ex6.o: pngwriter.o heat.o ex6.f90

heat.o: heat.f90
pngwriter.o: pngwriter.c


$(EXE6): $(OBJS6) .ALWAYSEXEC
	$(FC) $(FCLAGS) -fopenmp $(OBJS6) -o $@ -lpng

%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXES)
	-/bin/rm -f a.out *.o *.mod *~ *.png

.ALWAYSEXEC:
