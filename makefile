all:
	R CMD INSTALL elves

clean:
	rm elves/src/*.o elves/src/*.so
