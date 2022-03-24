library(MIBench)
library(Amelia)

if (dgp == "khjs-mar") {
  true_values <- c(0, -0.11, -0.089)
}
if (dgp == "mixed-mcar") {
  true_values <- c(0, 1, 0, -2)
}

if (dgp == "cg-mar") {
  true_values <- c(-1.92, 1.92, 1.92)

}

if (dgp == "mo-mcar") {
  true_values <- c(0, 1, 1, 1, 1)
}

# Wrap imputation function so that it outputs a list of m completed data sets

mi_amelia <- function(data, m = 10) {
  mi_obj <- Amelia::amelia(data, m = m, noms = NULL)

  return(mi_obj$imputations)

}

test <-
  MIBench(
    dgp = "khjs-mar",
    n_iter = 500,
    MIalgorithm = mi_amelia,
    n_data = 500,
    seed = 220324,
    store_runs = T,
    compare = F,
    start_i = 1,
    algorithm_prefix = "amelia",
    congenial = T
  )

MIBench(load_runs = T, algorithm_prefix = "amelia", dgp = "khjs-mar", n_iter = 500, start_i = 1, compare = F)


path <- paste0("experiments/",dgp,"/", algorithm_prefix, "/")

res_list <- lapply(1:500, function(i) readRDS(paste0(path, "analysis_",i,".RDS")))

imp_list <- lapply(1:500, function(i) readRDS(paste0(path, "imputations_",i,".RDS")))

analyze_mi(imputations, analysis_model, congenial = congenial)

analysis_model <- function(x) {
  lm(x[, 1] ~ x[, 2] + x[, 3])
}
res_nc <- lapply(imp_list, function(x) MIBench:::analyze_mi(x, analysis_model, congenial = F))


res <-
  MIBench:::summarize_mi_analysis(res_nc, true_values)


MIBench:::summarize_mi_analysis(res_nc, true_values)
MIBench:::summarize_mi_analysis(res_list, true_values)
