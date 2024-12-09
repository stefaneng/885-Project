gam_glm_formulas <- list(
  "event ~ surgery * transplant * age",
  'event ~ surgery + transplant + s(age, by = surgery, bs = "bs")',
  "event ~ surgery + transplant + s(age, by = transplant, bs = 'bs')",
  "event ~ surgery + transplant + s(age, by = transplant, bs = 'bs') + s(age, by = surgery, bs = 'bs')"
)

gam_mods_glm <- lapply(gam_glm_formulas, function(fc) {
  f <- formula(fc)
  nice_formula <- reformulate(labels(terms(f)), f[[2]])[[3]]
  reml_mod <- gam(
    formula = formula(f),
    data = heart,
    family = binomial,
    method = "REML"
  )
  ml_mod <- gam(
    formula = formula(f),
    data = heart,
    family = binomial,
    method = "ML"
  )

  list( reml = reml_mod,
    ml = ml_mod,
    aic = AIC(ml_mod),
    deviance = ml_mod$deviance,
    formula = fc
  )
})

gam_table_aic_logit <- bind_rows(lapply(gam_mods_glm, function(m) {
  m[c("formula", "aic", "deviance")]
}))

model_logit <- glm(
  event ~ surgery * transplant * age,
  data = heart,
  family = binomial
)

final_gam_model_logit <- gam_mods_glm[[which.min(sapply(gam_mods_glm, function(m) m$aic))]]

#
# # # Create table from AIC/deviance
# gam_mods_table <- bind_rows(lapply(gam_mods, function(mod) {
#   par_terms <- cbind(tidy(mod$reml, parametric = TRUE), parametric = TRUE)
#   nonpar_terms <- tidy(mod$reml, parametric = FALSE)
#
#   if (nrow(nonpar_terms) == 0) {
#     return(par_terms)
#   } else {
#     return(bind_rows(par_terms, cbind(nonpar_terms, parametric = FALSE)))
#   }
# }), .id = "mod")
