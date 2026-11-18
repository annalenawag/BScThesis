# libraries
library(bgms)
library(easybgm)

# check package version for reproducability 
packageVersion("bgms")

# read in data 
data <- readRDS("/Users/annalenawag/Documents/BSc_thesis/BSc_git/decoy_data_3.RDS")

# -----------------------
# 1. edge selection and parameter estimation 
# -----------------------

fit1 <- bgm(
  x = data, 
  variable_type = "ordinal",
  edge_selection = TRUE, 
  edge_prior = bernoulli_prior(0.5),
  interaction_prior = normal_prior(scale=1),
  iter = 25000,
  warmup = 5000,
  na_action ="listwise",
  seed = 1
)

# largest R-hat per parameter class (should be below 1.01)
sapply(extract_rhat(fit1), max, na.rm = TRUE)

# posterior summaries with MCSE, ESS and R-hat
summary(fit1)

# inclusion Bayes factors and edge verdicts
verdicts(fit1, evidence_threshold = 10)
extract_inclusion_bf(fit1)

# posterior means (p x p matrices)
coef(fit1)$pairwise # partial associations
coef(fit1)$indicator # inclusion probabilities 
extract_log_odds(fit1) # lod agjacent-category ORs

# plots 
plot(fit1) # edge evidence plot
plot_edge_posterior(fit1, "CEIII1E", "CEIII3E") # one edge

