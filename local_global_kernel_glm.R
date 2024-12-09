gaussian_kernel <- function(x, center, bandwidth) {
  exp(-0.5 * ((x - center) / bandwidth)^2) / (sqrt(2 * pi) * bandwidth)
}

data <- heart
data$surgery <- as.numeric(as.character(data$surgery))
data$transplant <- as.numeric(as.character(data$transplant))
target_ages <- seq(min(heart$age), max(heart$age), length.out = 100)

# Step 1: Fit the global logistic model
global_fit <- glm(y ~ transplant + age, data = data, family = binomial())

data$residuals <- residuals(global_fit, type = "pearson")#, type = "response")  # Response residuals

# Step 2: Fit the local linear model for residuals
fit_local_linear <- function(data, target_ages, bandwidth) {
  n <- nrow(data)
#  theta_est <- numeric(length(target_ages))
  t(sapply(target_ages, function(age) {
    weights <- gaussian_kernel(data$age, age, bandwidth)
    data$age_centered <- data$age - age

    # Fit logistic regression with weights
    fit <- lm(residuals ~ surgery*age, data = data, weights = weights)
    # Extract coefficients
    coef(fit)[c("(Intercept)", "surgery")]
  }))
}

predict_partial_local_logistic <- function(globalfit, localfit, newdata, type = c("link", "response")) {
  type <- match.arg(type)
  a <- newdata[, "age"]

  #intercept_func <- approxfun(target_ages, localfit[,"(Intercept)"])
  #surgery_func <- approxfun(target_ages, localfit[,"surgery"])

  transplant_val <- newdata$transplant
  if (is.factor(transplant_val)) {
    transplant_val <- as.numeric(as.character(transplant_val))
  }
  surgery_val <- newdata$surgery
  if (is.factor(surgery_val)) {
    surgery_val <- as.numeric(as.character(surgery_val))
  }

  global_pred <- predict(globalfit, newdata = newdata, type = "link")
  local_res <- localfit[,"(Intercept)"] + surgery_val * localfit[,"surgery"]

  final_pred <- predict(globalfit, newdata = newdata, type = "link") + local_res * sqrt(var(globalfit$data$event))

  if (type == "link") {
    return(final_pred)
  } else if (type == "response") {
    return(plogis(final_pred))
  } else {
    stop("Invalid type")
  }
}


### Cross validation
if (FALSE) {
  bandwidth <- seq(5, 30, by = 0.5)
  cv_results_partial_local <- lapply(bandwidth, function(b) {
    cat("Start bandwidth ", b, "\n")
    mean(sapply(seq_len(nrow(heart)), function(i) {
      yi <- data$age[i]
      theta_df <- fit_local_linear(data[-i, ], yi, b)

      p_hat <- predict_partial_local_logistic(
        global_fit, theta_df, newdata = data[i, c("age", "transplant", "surgery")], type = "response")
      - 2 * sum(yi * log(p_hat) + (1 - yi) * log(1 - p_hat))
    }))
  })
  cv_bandwidth <- cv_results_partial_local_kernel$bandwidth[which.min(cv_results_partial_local_kernel$deviance)]
  saveRDS(data.frame(bandwidth = bandwidth, deviance = unlist(cv_results_partial_local)), "cv_results_partial_local_kernel.rds")
}

