library(targets)
list(tar_target(data, read_tsv("data.csv"), format = "file"), 
    tar_target(params, c(0.01, 0.1, 1)), tar_target(model, fit_model(data, 
        params), pattern = map(params)), tar_target(plot, plot_results(model), 
        pattern = map(params)))
