all: clean
	Rscript -e "library(methods); Rcpp::Rcpp.package.skeleton(name='elves_temp', code_files='elves/R/helpers.R', cpp_files='elves/src/book_elf.cpp', example_code=FALSE, force=T);"
	cp elves_temp/src/RcppExports.cpp elves/src/
	cp elves_temp/R/RcppExports.R elves/R/
	R CMD INSTALL elves
	rm -r elves_temp

clean:
	rm elves/src/*.o elves/src/*.so
