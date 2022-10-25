# Packages ----
library(targets)
library(future)
library(future.callr)
library(furrr)
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
	"furrr",
	"microbenchmark"
))
plan(list(tweak(callr, workers = 1), tweak(multisession, workers = 12)))
# Parallelism ----
# with`plan(callr)`
# invoke the pipeline with `tar_make_future(N)`, where N is the number of
# parallel processes you'd like to use to evaluate the pipeline.
# Independent targets will automatically be computed in parallel and 
# dependencies will begin evaluation once their parents are done.
# A custom plan can be specified when you have nested futures within 
# individual targets
list(
	# Hash table lookup performance bechmarks ----
	tar_target(
		effect_of_vec_len_parallel, {
			n_bench <- 20
			lengths <- round(seq(1, 1e6, length.out = 20))
			#lst <- vector(mode = "list", length = length(lengths))
			#lstr <- vector(mode = "list", length = length(lengths))
			maxn <- max(lengths)
			values <- 1:maxn
			
			# Key value pairs generated outside the main benchmarking loop 
			# in the max length required and subset within the loop to 
			# save on the expensive operation of random key generation
			# and uniqueness checking
			keys <- purrr::map_chr(1:maxn, ~paste0(
				sample(c(letters, LETTERS, 1:100), sample(10:15, 1)),
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
			# furrr::future_walk(seq_along(lengths), ~{
			furrr::future_map_dfr(
				seq_along(lengths),
				#.options = furrr_options(seed = 42),
				~{
					i <- .x
					n <- lengths[i]
					# random subset of key value pairs of the appropriate length
					values <- sample(values, n) 
					keys <- keys[values]
					named_vec <- values
					names(named_vec) <- keys
					
					#lst[[i]]
					lst <- microbenchmark::microbenchmark(
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
					
					#lstr[[i]]
					lstr <- microbenchmark::microbenchmark(
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
					
					dplyr::bind_rows(lst,lstr)
					
				}, seed = TRUE)
			#effect_of_vec_len <- dplyr::bind_rows(lst,lstr)
			#effect_of_vec_len
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
	),
	
	# if else methods comparisons ----
	
	tar_target(
		if_else_comparisons, {
			lengths <- seq(1, 1000, length.out = 50)
			lst <- vector(mode = "list", length = length(lengths))
			marks <- sample(1:100, 1000, replace = TRUE)
			for (i in seq_along(lengths)) {
				n <- lengths[i]
				lst[[i]] <- microbenchmark::microbenchmark(
					"base::ifelse" = {
						ifelse(marks[1:n] >= 40, "pass", "fail")
					},
					"dplyr::if_else" = {
						dplyr::if_else(marks[1:n] >= 40, "pass", "fail")
					},
					"vector" = {
						results <- rep("fail", length(marks[1:n]))
						results[marks[1:n] >= 40] <- "pass"
					},
					"long_if_else" = {
						purrr::map_chr(marks[1:n], ~{
							if(.x >= 40) {
								"pass"
							} else {
								"fail"
							}
						})
					},
					"long_switch" = {
						purrr::map_chr(marks[1:n], ~{
							switch(
								as.character(.x >= 40), "TRUE" = "pass", "FALSE" = "fail"
							)
						})
					},
					times = 20
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n)
			}
			if_else_comparisons <- dplyr::bind_rows(lst)
		}
	),
	tar_target(
		if_else_comparisons_plot,
		if_else_comparisons %>%
			ggplot(., aes(length, time)) +
			scale_y_log10() +
			geom_smooth(
				aes(color = expr),
				method = "gam", formula = y ~ s(x, bs = "cs")
			) + 
			theme_bw() + 
			labs(
				title = "Comparison of 'ifelse' methods",
				y = "time /ns", color = "Condition",
				x = "Length of vector"
			) 
	),
	
	tar_target(
		sort_comparisons, {
			max_len <- 1e5
			n_bench <- 15
			lengths <- round(seq(10, max_len, length.out = 12))
			lst <- vector(mode = "list", length = length(lengths))
			marks <- sample(1:100, max_len, replace = TRUE)
			strings <- purrr::map_chr(1:max_len, ~paste0(
				sample(c(letters, LETTERS, 1:10),sample(10:15,1)),
				collapse = ""
			))
			furrr::future_map_dfr(seq_along(lengths), ~{
				n <- lengths[.x]
				microbenchmark::microbenchmark(
					"numeric_auto_fwd_complete" = {
						sort(marks[1:n])
					},
					"numeric_auto_decreasing_complete" = {
						sort(marks[1:n], decreasing = TRUE)
					},
					"numeric_auto_rev_complete" = {
						rev(sort(marks[1:n]))
					},
					"numeric_auto_fwd_partial10" = {
						sort(marks[1:n], partial = 1:10)
					},
					
					"strings_auto_fwd_complete" = {
						sort(strings[1:n])
					},
					"strings_auto_decreasing_complete" = {
						sort(strings[1:n], decreasing = TRUE)
					},
					"strings_auto_rev_complete" = {
						rev(sort(strings[1:n]))
					},
					"strings_auto_fwd_partial10" = {
						sort(strings[1:n], partial = 1:10)
					},
					
					"numeric_shell_fwd_complete" = {
						sort(marks[1:n], method = "shell",)
					},
					"numeric_quick_fwd_complete" = {
						sort(marks[1:n], method = "quick")
					},
					"numeric_radix_fwd_complete" = {
						sort(marks[1:n], method = "radix")
					},
					
					"strings_shell_fwd_complete" = {
						sort(strings[1:n], method = "shell",)
					},
					"strings_quick_fwd_complete" = {
						sort(strings[1:n], method = "quick")
					},
					"strings_radix_fwd_complete" = {
						sort(strings[1:n], method = "radix")
					},
					times = n_bench
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n) %>%
					#tidyr::extract(
					tidyr::separate(
						expr, sep = "_",
						into = c("type", "method", "direction", "complete"),
						#regex = "(\\w+)_method_(\\w+)_(\\w+)",
						remove = FALSE
					)
			})
		}
	),
	tar_target(
		partial_sort_plot,
		sort_comparisons %>%
			dplyr::filter(
				direction == "fwd",
				method == "auto"#,
				#complete != "complete"
			) %>%
			ggplot(., aes(length, time)) +
			#geom_point(aes(color = expr), size = 0.2) + 
			scale_y_log10() +
			#geom_line(aes(color = expr)) + 
			geom_smooth(
				aes(color = complete),
				#method = "lm", formula = "y ~ x"
				method = "gam", formula = y ~ s(x, bs = "cs")
			) + 
			theme_bw() + 
			facet_grid(~type) +
			labs(
				title = "Partial sorting (e.g. just top 10)",
				y = "time /ns", color = "Condition",
				x = "Length of vector"
			) 
	),
	tar_target(parallel_overhead, {
		move_square <- function(current){
			rolls <- matrix(sample(seq(1, 6), 6, replace = TRUE), ncol = 2)
			Total <- rowSums(rolls)
			IsDouble <- rolls[,1] == rolls[,2]
			if(IsDouble[1] && IsDouble[2] && IsDouble[3]) {
				current <- 11L # integer! #<<
			} else if (IsDouble[1] && IsDouble[2]) {
				current <- current + sum(Total[1:3])
			} else if (IsDouble[1]) {
				current <- current + sum(Total[1:2])
			} else {
				current <- current + sum(Total[1])
			}
			if(current %% 40L == 0L) {
				current <- 40L
			} else {
				current <- current %% 40L
			}
			as.integer(current)
		}
		initial <- sample(1:40, 1e5, replace = TRUE)
		lengths <- c(1, 1e2, 1e3, 1e4, 5e4, 1e5)
		#furrr::future_map_dfr(seq_along(lengths), ~{
		sessions <- c(2,4,6,8,12)
		prll <- vector(mode = "list", length = length(sessions))
		for (i in seq_along(sessions)) {
			plan(multisession, workers = sessions[i])
			prll[[i]] <- purrr::map_dfr(seq_along(lengths), ~{
				n <- lengths[.x]
				microbenchmark::microbenchmark(
					"Parallel" = {
						move <- furrr::future_map_int(
							initial[1:n], move_square,
							.options = furrr_options(seed = TRUE)
						)
					},
					times = 5
				) %>% 
					tibble::as_tibble() %>%
					dplyr::mutate(length = n, sessions = sessions[i])
			}#, .options = furrr_options(seed = TRUE)
			)
		}
		# plan(multisession, workers = 4)
		# prl <- purrr::map_dfr(seq_along(lengths), ~{
		# 	n <- lengths[.x]
		# 	microbenchmark::microbenchmark(
		# 		"Parallel" = {
		# 			move <- furrr::future_map_int(
		# 				initial[1:n], move_square,
		# 				.options = furrr_options(seed = TRUE)
		# 			)
		# 		},
		# 		times = 5
		# 	) %>% 
		# 		tibble::as_tibble() %>%
		# 		dplyr::mutate(length = n, sessions = 1)
		# }#, .options = furrr_options(seed = TRUE)
		# )
		
		plan(sequential)
		sql <- purrr::map_dfr(seq_along(lengths), ~{
			n <- lengths[.x]
			microbenchmark::microbenchmark(
				"Sequential" = {
					move <- purrr::map_int(initial[1:n], move_square)
				},
				times = 5
			) %>% 
				tibble::as_tibble() %>%
				dplyr::mutate(length = n, sessions = 1)
		}#, .options = furrr_options(seed = TRUE)
		)
		
		dplyr::bind_rows(prll,sql)
	}),
	tar_target(parallel_overhead_plot,
		parallel_overhead %>% 
			dplyr::mutate(time = time / 1e6) %>%
			ggplot(aes(length, time)) + 
			geom_smooth(
				aes(color = expr, linetype = as.factor(sessions)),
				method = "lm", formula = 'y~x'
			) +
			scale_y_log10(labels = scales::comma) +
			scale_x_continuous(labels = scales::comma) +
			theme_bw() +
			labs(
				title = "Parallel Overhead",
				y = "time /ns", color = "Evaluation",
				linetype = "cores",
				x = "Length of vector"
			)
	)
)

