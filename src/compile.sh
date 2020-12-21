#!/bin/bash/

gfortran -c ingredients.f90 velverlet.f90 calcforces.f90 calcenergy.f90 start.f90 main.f90
gfortran -o solar ingredients.o velverlet.o calcforces.o calcenergy.o start.o main.o

rm *.o
rm *.mod

mv solar ../run/

