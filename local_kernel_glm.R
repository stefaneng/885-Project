# # Simulate a dataset
# set.seed(123)
# n <- 200
# data <- data.frame(
#   age = runif(n, 20, 80),
#   TRT_0 = rbinom(n, 1, 0.5),
#   TRT_1 = rbinom(n, 1, 0.5),
#   y = rbinom(n, 1, 0.5)
# )

#library(survival)
#data(heart)
#heart$TRT <- heart$transplant
heart$y <- as.factor(1 - heart$event)
#heart$age <- heart$age - 48
data <- heart

gaussian_kernel <- function(x, center, bandwidth) {
  exp(-0.5 * ((x - center) / bandwidth)^2) / (sqrt(2 * pi) * bandwidth)
}

# Local linear logistic regression function
local_logistic <- function(data, target_age, bandwidth) {
  # Compute kernel weights
  weights <- gaussian_kernel(data$age, target_age, bandwidth)

  # Center age around target_age for local linear terms
  data$age_centered <- data$age - target_age

  #subset_data <- data[abs(data$age_centered) < bandwidth, ]

  # Fit logistic regression with weights
  fit <- glm(y ~ 1 + transplant * age_centered + surgery * age_centered,
             family = quasibinomial(link = "logit"),
             data = data,
             weights = weights)
  # Extract coefficients
  coef(fit)
}

# Define target ages and bandwidth
target_ages <- seq(20, 80, length.out = 100)
bandwidth <- 5

# Compute coefficients for each target age
results <- sapply(target_ages, function(age) {
  local_logistic(data, target_age = age, bandwidth = bandwidth)
})

# Transpose results for easier handling
results <- t(results)
#colnames(results) <- c("Intercept", "TRT_0", "Age_Centered_TR0", "TRT_1", "Age_Centered_TR1")

ilogit <- function(x) exp(x) / (1 + exp(x))
plot(target_ages, ilogit(rowSums(results[, 1:3])), col = "black", type = "l", ylim = c(0, 1))
lines(target_ages, ilogit(rowSums(results[, 1:2])), col = "blue", type = "l")
lines(target_ages, ilogit(rowSums(results[, c(1, 3)])), col = "red", type = "l")

#legend("bottomleft", legend = c("Transplant", "No transplant"), col = c("black", "red"), lty = 1)
