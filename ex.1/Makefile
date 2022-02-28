FC=gfortran

debug:
	$(FC)  -c src/main.f90 -I obj/ -o obj/main.o
	$(FC)  -o bin/app ./obj/methodCramer.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
