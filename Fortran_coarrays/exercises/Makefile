COMP=cray

ifeq ($(COMP),cray)
CC=cc
FC=ftn 
CCFLAGS=-O3
FCFLAGS=-efmw -dynamic
endif

ifeq ($(COMP),gnu)
CC=gcc
FC=gfortran
CCFLAGS=-g -O3 -ftree-vectorize
FCFLAGS=-g -O3 -ftree-vectorize
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
CCFLAGS=-g
FCFLAGS=-g
endif

EXE1=heat_serial
OBJS1=pngwriter.o pngwriter_mod.o heat.o ex4_heat_equation.o
LIBS=-lpng

all: heat_serial

pngwriter.o:	pngwriter.c
pngwriter_mod.o:	pngwriter_mod.f90
heat.o: heat.f90
ex4_heat_equation.o: ex4_heat_equation.f90

.PHONY: $(EXE1)

$(EXE1): $(OBJS1) .ALWAYSEXEC
	$(FC) $(FCFLAGS) $(OBJS1) -o $@ $(LIBS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXE1) a.out *.o *.mod *~

.ALWAYSEXEC:
