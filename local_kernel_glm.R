heart$y <- as.factor(1 - heart$event)
data <- heart
bandwidth <- seq(5, max(heart$age), by = 1)
target_ages <- seq(min(heart$age), max(heart$age), length.out = 100)

ilogit <- function(x) {
  exp(x) / (1 + exp(x))
}

gaussian_kernel <- function(x, center, bandwidth) {
  exp(-0.5 * ((x - center) / bandwidth)^2) / (sqrt(2 * pi) * bandwidth)
}

predict_local_logistic <- function(localfit, newdata, type = c("link", "response")) {
  type <- match.arg(type)
  a <- newdata[, "age"]
  X <- newdata[, colnames(newdata) != "age"]

  intercept_func <- approxfun(target_ages, localfit[,1])
  transplant_func <- approxfun(target_ages, localfit[,2])
  surgery_func <- approxfun(target_ages, localfit[,3])

  transplant_val <- X[, 1]
  if (is.factor(transplant_val)) {
    transplant_val <- as.numeric(as.character(X[, 1]))
  }
  surgery_val <- X[, 2]
  if (is.factor(surgery_val)) {
    surgery_val <- as.numeric(as.character(X[, 2]))
  }

  res <- intercept_func(a) + transplant_val * transplant_func(a) + surgery_val * surgery_func(a)
  if (type == "link") {
    return(res)
  } else if (type == "response") {
    return(exp(res) / (1 + exp(res)))
  } else {
    stop("Invalid type")
  }
}

# Local linear logistic regression function
fit_local_logistic <- function(data, target_age, bandwidth) {
  t(sapply(target_ages, function(age) {
    weights <- gaussian_kernel(data$age, age, bandwidth)
    data$age_centered <- data$age - age

    # Fit logistic regression with weights
    fit <- glm(y ~ 1 + transplant * age_centered + surgery * age_centered,
               family = quasibinomial(link = "logit"),
               data = data,
               weights = weights)
    # Extract coefficients
    coef(fit)[c("(Intercept)", "transplant1", "surgery1")]
  }))
}

if (FALSE) {
  cv_results <- lapply(bandwidth, function(b) {
    cat("Start bandwidth ", b, "\n")
    mean(sapply(seq_len(nrow(heart)), function(i) {
      # Leave-one-out cross-validation
      yi <- heart$age[i]
      local_fit <- fit_local_logistic(heart[-i,], yi, b)
      p_hat <- predict_local_logistic(local_fit, heart[i, c("age", "transplant", "surgery")], type = "response")
      - 2 * sum(yi * log(p_hat) + (1 - yi) * log(1 - p_hat))
    }))
  })

  saveRDS(data.frame(bandwidth = bandwidth, deviance = unlist(cv_results)), "cv_results.rds")
}
