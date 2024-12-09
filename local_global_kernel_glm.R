library(np)

# Simulated data
# set.seed(123)
# n <- 200
# age <- runif(n, 20, 80)
# surgery <- rbinom(n, 1, 0.5)
# transplant <- rbinom(n, 1, 0.5)
# theta <- 0.05 * (age - 50)  # Smooth age-dependent effect
# logit_mu <- -2 + 1.5 * transplant + theta * surgery
# mu <- plogis(logit_mu)
# y <- rbinom(n, 1, mu)
# data <- data.frame(age, surgery, transplant, y)

gaussian_kernel <- function(x, center, bandwidth) {
  exp(-0.5 * ((x - center) / bandwidth)^2) / (sqrt(2 * pi) * bandwidth)
}

data <- heart
data$y <- as.factor(1 - data$event)
data$surgery <- as.numeric(as.character(data$surgery))
data$transplant <- as.numeric(as.character(data$transplant))

# Step 1: Fit the global logistic model
global_fit <- glm(y ~ transplant + age, data = data, family = binomial())
data$residuals <- residuals(global_fit)#, type = "response")  # Response residuals

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

  intercept_func <- approxfun(target_ages, localfit[,"(Intercept)"])
  surgery_func <- approxfun(target_ages, localfit[,"surgery"])

  transplant_val <- newdata$transplant
  if (is.factor(transplant_val)) {
    transplant_val <- as.numeric(as.character(transplant_val))
  }
  surgery_val <- newdata$surgery
  if (is.factor(surgery_val)) {
    surgery_val <- as.numeric(as.character(surgery_val))
  }

  global_pred <- predict(globalfit, newdata = newdata, type = "link")
  local_res <- intercept_func(a) + surgery_val * surgery_func(a)

  final_pred <- predict(globalfit, newdata = newdata, type = "link") + local_res

  if (type == "link") {
    return(final_pred)
  } else if (type == "response") {
    return(plogis(final_pred))
  } else {
    stop("Invalid type")
  }
}

# Parameters
bandwidth <- 10  # Bandwidth for local smoothing
target_ages <- seq(min(data$age), max(data$age), length.out = 100)
pred_newdata <- expand.grid(
  age = target_ages,
  transplant = c(0, 1),
  surgery = c(0, 1)
)

# theta(age) and theta(age) * surgery estimate

theta_df <- fit_local_linear(data, target_ages, bandwidth)
# results <- fit_local_logistic(heart, target_ages, b)

pred_newdata$pred <- predict_partial_local_logistic(
  global_fit, theta_df, newdata = pred_newdata, type = "response")
# pred_newdata$pred <-  predict_local_logistic(results, pred_newdata, type = "response")

# Visualization of theta(age)
library(ggplot2)
#theta_df <- data.frame(age = target_ages, theta = theta_est)
ggplot(
  pred_newdata,
  aes(x = age, y = pred,
      color = factor(transplant),
      linetype = factor(surgery))) +
  geom_line() +
  labs(title = "Estimated Local Effect of Age (Theta)", y = "Theta (Local Effect)", x = "Age") +
  scale_linetype_manual(
    name = "",
    labels = c("No Surgery", "Surgery"),
    values = c("solid", "dashed")
  ) +
  scale_color_manual(
    name = "",
    labels = transplant_factor_levels,
    values = colortheme2
  ) +
  scale_fill_manual(
    name = "",
    labels = transplant_factor_levels,
    values = colortheme2
  ) +
  theme_classic()

