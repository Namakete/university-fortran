FC=gfortran
FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

debug:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/gen_array.f90 -J obj/ -o obj/gen_array.o
	$(FC) $(FFLAGS) $(FOPT)  -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/gen_array.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt