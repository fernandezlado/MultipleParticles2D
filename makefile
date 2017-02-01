#2D ACCELERATOR MAKEFILE

## VARIABLES DECLARATION ##

#COMPILER
FC=ifort

#DIRECTORIES
CURDIR=$(shell pwd)
SRCDIR=src
OBJDIR=obj
MODDIR=mod
BINDIR=bin

#OBJECTS FILES TO COMPILE
OBJ = $(CURDIR)/$(OBJDIR)/main.o $(CURDIR)/$(OBJDIR)/modObstacle.o $(CURDIR)/$(OBJDIR)/modOperators.o $(CURDIR)/$(OBJDIR)/modSpecialFunctions.o $(CURDIR)/$(OBJDIR)/modMathConstants.o $(CURDIR)/$(OBJDIR)/modFarInteractions.o

#OUTPUT FILENAME
fileOut=MultipleParticles


output: $(OBJ)
	$(FC) -o $(CURDIR)/$(BINDIR)/$(fileOut) $(OBJ)

$(CURDIR)/$(OBJDIR)/main.o: $(CURDIR)/$(SRCDIR)/main.f90 $(CURDIR)/$(OBJDIR)/modObstacle.o $(CURDIR)/$(OBJDIR)/modOperators.o 
	$(FC) -c $(CURDIR)/$(SRCDIR)/main.f90 -o $(CURDIR)/$(OBJDIR)/main.o -module $(CURDIR)/$(MODDIR)

$(CURDIR)/$(OBJDIR)/modObstacle.o: $(CURDIR)/$(SRCDIR)/modObstacle.f90
	$(FC) -c $(CURDIR)/$(SRCDIR)/modObstacle.f90 -o $(CURDIR)/$(OBJDIR)/modObstacle.o -module $(CURDIR)/$(MODDIR)

$(CURDIR)/$(OBJDIR)/modOperators.o: $(CURDIR)/$(SRCDIR)/modOperators.f90 $(CURDIR)/$(OBJDIR)/modObstacle.o $(CURDIR)/$(OBJDIR)/modSpecialFunctions.o $(CURDIR)/$(OBJDIR)/modMathConstants.o
	$(FC) -c $(CURDIR)/$(SRCDIR)/modOperators.f90 -o $(CURDIR)/$(OBJDIR)/modOperators.o -module $(CURDIR)/$(MODDIR)

$(CURDIR)/$(OBJDIR)/modSpecialFunctions.o: $(CURDIR)/$(SRCDIR)/modSpecialFunctions.f90
	$(FC) -c $(CURDIR)/$(SRCDIR)/modSpecialFunctions.f90 -o $(CURDIR)/$(OBJDIR)/modSpecialFunctions.o -module $(CURDIR)/$(MODDIR)

$(CURDIR)/$(OBJDIR)/modMathConstants.o: $(CURDIR)/$(SRCDIR)/modMathConstants.f90
	$(FC) -c $(CURDIR)/$(SRCDIR)/modMathConstants.f90 -o $(CURDIR)/$(OBJDIR)/modMathConstants.o -module $(CURDIR)/$(MODDIR)

$(CURDIR)/$(OBJDIR)/modFarInteractions.o: $(CURDIR)/$(SRCDIR)/modFarInteractions.f90 $(CURDIR)/$(OBJDIR)/modMathConstants.o $(CURDIR)/$(OBJDIR)/modObstacle.o
	$(FC) -c $(CURDIR)/$(SRCDIR)/modFarInteractions.f90 -o $(CURDIR)/$(OBJDIR)/modFarInteractions.o -module $(CURDIR)/$(MODDIR)


clean:
	rm *.mod
	rm *.o

run:
	./bin/$(fileOut)
