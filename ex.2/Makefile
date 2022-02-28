FC=gfortran

clean:
	rm -rf obj/*
	rm -rf bin/*

debug:
	$(FC)  -o bin/app ./src/main.f90

run:
	$(FC)  -o bin/app ./src/main.f90
	cd ./bin; ./app;
