# Can you parallelise building environments? - NOT by my testing
library(future)
library(furrr)
library(purrr)
library(tictoc)

plan(multisession, workers = 12)
# multicore - may have better luck with multicore but doubtful
# multicore does not work interactively

message("making keys...")

maxn <- 1e5
values <- 1:maxn

keys <- purrr::map_chr(1:maxn, ~paste0(
	sample(c(letters, LETTERS, 1:100),sample(10:15,1)),
	collapse = ""
))
# Ensure that all n keys are unique
keys <- unique(keys)
#length(keys)
while(length(keys) < maxn) {
	nn <- maxn - length(keys)
	new_keys <- purrr::map_chr(1:nn, ~paste0(
		sample(c(letters, LETTERS, 1:100),sample(10:15,1)),
		collapse = ""
	))
	keys <- unique(c(keys, new_keys))
}

message("keys made")

message("serial begins")
my_env_s <- new.env()
tic()
purrr::walk2(keys, values, ~{my_env_s[[.x]] <- .y})
toc()
head(ls(my_env_s))
message("serial over")

message("parallel begins")
my_env_p <- new.env()
tic()
furrr::future_walk2(keys, values, ~{my_env_p[[.x]] <- .y})
toc()
head(ls(my_env_p))
message("parallel over")

# Result is not an error but not values are in the resulting environment
