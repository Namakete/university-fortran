FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

debug:
	$(FC) $(FFLAGS) -c src/sub.f90 -J obj/ -o obj/sub.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/sub.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
