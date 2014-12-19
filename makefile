all: clean
	Rscript -e "library(methods); Rcpp::Rcpp.package.skeleton(name='elves_temp', code_files='elves/R/helpers.R', cpp_files='elves/src/book_elf.cpp', example_code=FALSE, force=T);"
	cp elves_temp/src/RcppExports.cpp elves/src/
	cp elves_temp/R/RcppExports.R elves/R/
	sed -i.bak s/elves_temp/elves/g elves/R/RcppExports.R
	sed -i.bak s/elves_temp/elves/g elves/src/RcppExports.cpp
	R CMD INSTALL elves
	rm -r elves_temp
	rm elves/R/*.bak
	rm elves/src/*.bak

clean:
	-rm elves/src/*.o elves/src/*.so

run:
	Rscript build_schedule_test.R
