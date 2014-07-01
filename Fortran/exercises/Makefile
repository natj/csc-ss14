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
FCFLAGS=-g
RUN=
endif

ifeq ($(COMP),intel)
CC=icc
FC=ifort
CCFLAGS=-g
FCFLAGS=-g
RUN=
endif

EXE1=ex1
OBJS1=ex1.o

EXE2a=ex2a
OBJS2a=ex2a.o

EXE2b=ex2b
OBJS2b=ex2b.o

EXE3a=ex3a
OBJS3a=ex3a.o

EXE3b=ex3b
OBJS3b=ex3b.o

EXE4=ex4
OBJS4=pngwriter.o ex4.o

EXE5a=ex5a
OBJS5a=ex5a.o

EXE5b=ex5b
OBJS5b=ex5b.o

EXE6=ex6
OBJS6=ex6.o

EXE7=ex7
OBJS7=pngwriter.o ex7.o

EXE8=ex8
OBJS8=pngwriter.o ex8.o 

EXE9=ex9
OBJS9=pngwriter.o heat.o ex9.o 

EXE10a=bonus_ex10a
OBJS10a=bonus_ex10a.o

EXE10b=bonus_ex10b
OBJS10b=bonus_ex10b.o

EXE10c=bonus_ex10c
OBJS10c=bonus_ex10c.o

EXE11a=bonus_ex11a
OBJS11a=ex11_linkedlistmod.o ex11_linkedlist.o 

EXE11b=bonus_ex11b
OBJS11b=ex11_linkedlistmod_priv.o ex11_linkedlist_priv.o 

EXES=$(EXE1) $(EXE2a) $(EXE2b) $(EXE3a) $(EXE3b) $(EXE4) \
     $(EXE5a) $(EXE5b) $(EXE6) $(EXE7) $(EXE8) $(EXE9) \
     $(EXE10a) $(EXE10b) $(EXE10c) $(EXE11a) $(EXE11b)

all: $(EXES)

# Dependencies
ex1.o: ex1.f90
ex2a.o: ex2a.f90
ex2b.o: ex2b.f90
ex3a.o: ex3a.f90
ex3b.o: ex3b.f90
ex4.o: pngwriter.o ex4.f90
ex5a.o: ex5a.f90
ex5b.o: ex5b.f90
ex6.o: ex6.f90
ex7.o: pngwriter.o ex7.f90
ex8.o: pngwriter.o ex8.f90
ex9.o: pngwriter.o heat.o ex9.f90
bonus_ex10a.o: bonus_ex10a.f90
bonus_ex10b.o: bonus_ex10b.f90
bonus_ex10c.o: bonus_ex10c.f90

heat.o: heat.f90
pngwriter.o: pngwriter.c

ex11_linkedlist.o: ex11_linkedlistmod_priv.o ex11_linkedlist.f90
ex11_linkedlistmod.o: ex11_linkedlistmod.f90

ex11_linkedlist_priv.o: ex11_linkedlistmod_priv.o ex11_linkedlist_priv.f90
ex11_linkedlistmod_priv.o: ex11_linkedlistmod_priv.f90

$(EXE1): $(OBJS1) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS1) -o $@

$(EXE2a): $(OBJS2a) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS2a) -o $@

$(EXE2b): $(OBJS2b) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS2b) -o $@

$(EXE3a): $(OBJS3a) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS3a) -o $@

$(EXE3b): $(OBJS3b) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS3b) -o $@

$(EXE4): $(OBJS4) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS4) -o $@ -lpng

$(EXE5a): $(OBJS5a) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS5a) -o $@

$(EXE5b): $(OBJS5b) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS5b) -o $@

$(EXE6): $(OBJS6) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS6) -o $@

$(EXE7): $(OBJS7) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS7) -o $@ -lpng

$(EXE8): $(OBJS8) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS8) -o $@ -lpng

$(EXE9): $(OBJS9) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS9) -o $@ -lpng

$(EXE10a): $(OBJS10a) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS10a) -o $@ -lpng

$(EXE10b): $(OBJS10b) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS10b) -o $@

$(EXE10c): $(OBJS10c) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS10c) -o $@

$(EXE11a): $(OBJS11a) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS11a) -o $@

$(EXE11b): $(OBJS11b) .ALWAYSEXEC
	$(FC) $(FCLAGS) $(OBJS11b) -o $@

%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXES)
	-/bin/rm -f a.out *.o *.mod *~ *.png

.ALWAYSEXEC: