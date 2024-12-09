# TODO:
# Confidence intervals (bootstrap?)
# CV

heart$y <- as.factor(1 - heart$event)
data <- heart

gaussian_kernel <- function(x, center, bandwidth) {
  exp(-0.5 * ((x - center) / bandwidth)^2) / (sqrt(2 * pi) * bandwidth)
}

# Local linear logistic regression function
fit_local_logistic <- function(data, target_age, bandwidth) {
  weights <- gaussian_kernel(data$age, target_age, bandwidth)
  data$age_centered <- data$age - target_age

    # Fit logistic regression with weights
  fit <- glm(y ~ 1 + transplant * age_centered + surgery * age_centered,
             family = quasibinomial(link = "logit"),
             data = data,
             weights = weights)
  # Extract coefficients
  coef(fit)[c("(Intercept)", "transplant1", "surgery1")]
}

# Define target ages and bandwidth
target_ages <- seq(min(heart$age), max(heart$age), length.out = 100)
bandwidth <- 5

# Compute coefficients for each target age
results <- t(sapply(target_ages, function(age) {
  fit_local_logistic(data, target_age = age, bandwidth = bandwidth)
}))

predict_local_logistic <- function(newdata, type = c("link", "response")) {
  type <- match.arg(type)
  a <- newdata[, "age"]
  X <- newdata[, colnames(newdata) != "age"]

  intercept_func <- approxfun(target_ages, results[,1])
  transplant_func <- approxfun(target_ages, results[,2])
  surgery_func <- approxfun(target_ages, results[,3])

  res <- intercept_func(a) + X[,1] * transplant_func(a) + X[,2] * surgery_func(a)
  if (type == "link") {
    return(res)
  } else if (type == "response") {
    return(exp(res) / (1 + exp(res)))
  } else {
    stop("Invalid type")
  }
}

pred_newdata <- expand.grid(
  age = target_ages,
  transplant = c(0, 1),
  surgery = c(0, 1)
)

pred_newdata$pred <- predict_local_logistic(pred_newdata, type = "response")

ggplot(pred_newdata) +
  geom_line(aes(x = age, y = pred, color = factor(transplant), linetype = factor(surgery)))
