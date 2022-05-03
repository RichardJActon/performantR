# Packages ----
library(targets)
library(future)
library(future.callr)
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c(
	"dplyr",
	"tibble",
	"readr",
	"purrr",
	"tidyr",
	"qs",
	"ggplot2",
	"data.table",
	"microbenchmark"
))
plan(list(callr, multisession))
# Parallelism ----
# involve the pipeline with `tar_make_future(N)``, where N is the number of
# parallel processes you'd like to use to evaluate the pipeline.
# Indpendent targets will automatically be computed in parallel and 
# dependencies will begin evaluation once their parents are done.
list(
	# Hash table lookup performance bechmarks ----
	tar_target(
		effect_of_vec_len_parallel, {
			n_bench <- 13
			lengths <- round(seq(1, 1e6, length.out = 20))
			lst <- vector(mode = "list", length = length(lengths))
			lstr <- vector(mode = "list", length = length(lengths))
			maxn <- max(lengths)
			values <- 1:maxn
			
			# Key value pairs generated outside the main benchmarking loop 
			# in the max length required adn subset within the loop to 
			# save on the expensive operation of random key generation
			# and uniqueness checking
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
			# The {furrr} package parallelises the {purr} map functions using {future}
			furrr::future_walk(seq_along(lengths), ~{
				i <- .x
				n <- lengths[i]
				# random subset of key value pairs of the appropriate length
				values <- sample(values, n) 
				keys <- keys[values]
				named_vec <- values
				names(named_vec) <- keys
				
				lst[[i]] <- microbenchmark::microbenchmark(
					"named vector" = {
						named_vec <- values
						names(named_vec) <- keys
					},
					"named list" = {
						named_lst <- as.list(named_vec)
					},
					"hash" = {
						my_env <- new.env(hash = TRUE) 
						purrr::walk2(keys, values, ~{my_env[[.x]] <- .y})
					},
					"data.frame" = {
						df <- data.frame(keys = keys, values = values)
					},
					"indexed data.table" = {
						dt <- data.table(keys = keys, values = values)
						setkey(dt, keys) 
					},
					times = n_bench
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n, type = "Construction")
				
				example_key <- sample(keys, 1)
				
				named_lst <- as.list(named_vec)
				
				my_env <- new.env(hash = TRUE) 
				purrr::walk2(keys, values, ~{my_env[[.x]] <- .y})
				
				# doesnot work multisession - maybe multicore? unlikely
				# see mc_env_assign.R
				#furrr::future_walk2(keys, values, ~{my_env[[.x]] <- .y})
				
				dt <- data.table(keys = keys, values = values)
				setkey(dt, keys) 
				
				df <- data.frame(keys = keys, values = values)
				
				lstr[[i]] <- microbenchmark::microbenchmark(
					"named vector" = {
						named_vec[example_key]
					},
					"named list" = {
						named_lst[example_key]
					},
					"hash" = {
						my_env[[example_key]]
					},
					"data.frame" = {
						df[df$keys == example_key,]
					},
					"indexed data.table" = {
						dt[keys == example_key]
						#dt[keys == example_key][[2]]
					},
					times = n_bench
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n, type = "Retreival")
				
			}, seed = TRUE)
			effect_of_vec_len <- dplyr::bind_rows(lst,lstr)
			effect_of_vec_len
		},
		format = "qs"
	),
	tar_target(
		effect_of_vec_len_plot,
		effect_of_vec_len_parallel %>%
			dplyr::mutate(time = time / 1e6) %>%
			ggplot(., aes(length, time)) +
			scale_y_log10(labels = scales::comma) +
			scale_x_continuous(labels = scales::comma) +
			geom_smooth(
				aes(color = expr),
				method = "gam", formula = y ~ s(x, bs = "cs")
			) + 
			theme_bw() + 
			labs(
				title = "Change in runtime with number of elements",
				y = "time /ms (log10 scale)", color = "Condition",
				x = "Number of elements"
			) +
			facet_wrap(~type, ncol = 2)
	),
	
	# Simple vector creation ----
	tar_target(
		object_creation, {
			n <- 100
			n_bench <- 100
			microbenchmark::microbenchmark(
				grow = {
					vec <- numeric(0)
					for(i in 1:n) vec <- c(vec, i)
				},
				"pre-allocate" = {
					vec <- numeric(n)
					for(i in 1:n) vec[i] <- i
				},
				direct = {
					vec <- 1:n
				},
				times = n_bench
			)
		}
	),
	tar_target(
		object_creation_plot,
		object_creation %>%
			as_tibble() %>%
			ggplot(aes(expr, time)) + 
			ggbeeswarm::geom_quasirandom(size = 0.2) +
			geom_boxplot(outlier.shape = NA, fill = NA, color = "red") +
			scale_y_log10() + 
			labs(title = "Comparative Runtimes", y = "time /ns", x = "Condition") + 
			theme_bw()
	),
	
	# vector growth benchmark ----
	tar_target(
		effect_of_vec_len_vec, {
			n_bench <- 50
			lengths <- seq(1, 1000, length.out = 10)
			lst <- vector(mode = "list", length = length(lengths))
			for (i in seq_along(lengths)) {
				n <- lengths[i]
				lst[[i]] <- microbenchmark::microbenchmark(
					grow = {
						vec <- numeric(0)
						for(i in 1:n) vec <- c(vec, i)
					},
					"pre-allocate" = {
						vec <- numeric(n)
						for(i in 1:n) vec[i] <- i
					},
					direct = {
						vec <- 1:n
					},
					times = 50
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n)
			}
			effect_of_vec_len_vec <- dplyr::bind_rows(lst)
		}
		
	),
	
	# data frame growth benchmark ----
	tar_target(
		effect_of_vec_len_df, {
			n_bench <- 50
			lengths <- seq(1, 1000, length.out = 10)
			lst <- vector(mode = "list", length = length(lengths))
			for (i in seq_along(lengths)) {
				n <- lengths[i]
				lst[[i]] <- microbenchmark::microbenchmark(
					grow = {
						df <- data.frame(A = integer())
						for(i in 1:n) df <- rbind(df, data.frame(A = i))
					},
					"pre-allocate" = {
						df <- data.frame(A = integer(length = n))
						for(i in 1:n) df[i, 1] <- i
					},
					direct = {
						df <- data.frame(A = 1:n)
					},
					times = n_bench
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n)
			}
			effect_of_vec_len_df <- dplyr::bind_rows(lst)
		}
	),
	
	# Combined plot of vector and data frame growth benchmarks ----
	tar_target(
		effect_length_vec_vs_df_plot,
		dplyr::bind_rows(
			effect_of_vec_len_vec %>% dplyr::mutate(type = "vector"),
			effect_of_vec_len_df %>% dplyr::mutate(type = "data.frame")
		) %>%
			ggplot(., aes(length, time)) +
			scale_y_log10(labels = scales::comma) +
			geom_smooth(
				aes(color = expr),
				method = "gam", formula = y ~ s(x, bs = "cs")
			) + 
			theme_bw() + 
			labs(
				title = "Vector growth faster now, df pre-allocation still faster",
				subtitle = "Runtime by length",
				y = "time /ns", color = "Condition",
				x = "Length of vector"
			) +
			facet_wrap(~type)
	)
)
