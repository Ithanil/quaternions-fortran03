tests:	quatexample testquats
	./testquats

quaternions.o:
	gfortran -g -fbounds-check -c quaternions.F03

quatexample:	quaternions.o
	gfortran -g -fbounds-check quatexample.F03 quaternions.o -o quatexample

testquats:	quaternions.o
	gfortran -g -fbounds-check testquats.F03 quaternions.o -o testquats

clean:
	rm quatexample
	rm testquats
	rm *.mod
	rm *.o
	rm -r *.dSYM

python:
	f2py -m quaternions quaternions.F03
