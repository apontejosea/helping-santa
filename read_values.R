# Input
n_elves <- 2
toys    <- read.csv('test_toys.csv')

# This will generate a data.frame with the following columns
#   toy_id
#   elf_id
#   start_time
#   duration
allocate_elves <- function(n_elves, toys) {
	return(data.frame(toy_id=1, elf_id=2, start_time=0, duration=1))
}

print(allocate_elves(n_elves, toys))
