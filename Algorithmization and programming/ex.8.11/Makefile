FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

clean:
	rm -rf obj/*
	rm -rf bin/*

build:
	mkdir -p bin obj
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/findMaxMinElements.f90 -J obj/ -o obj/findMaxMinElements.o
	$(FC) $(FFLAGS) -c src/matrix_IO.f90 -J obj/ -o obj/matrix_IO.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/matrix_IO.o ./obj/findMaxMinElements.o ./obj/environment.o obj/main.o

run:
	cd ./bin; rm output.txt 2>/dev/null; ./app; cat output.txt 2>/dev/null
