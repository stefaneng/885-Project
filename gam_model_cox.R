heart2 <- heart
heart <- survival::heart

heart$futime <- heart$stop - heart$start
heart$age <- heart$age + 48

gam_cox_formulas <- list(
  "futime ~ surgery * transplant * age",
  'futime ~ surgery + transplant + s(age, by = surgery, bs = "bs")',
  "futime ~ surgery + transplant + s(age, by = transplant, bs = 'bs')",
  "futime ~ surgery:transplant + surgery + transplant + s(age, by = transplant, bs = 'bs') + s(age, by = surgery, bs = 'bs')"
)

nice_formulas <- lapply(gam_cox_formulas, function(fc) {
  f <- formula(fc)
  reformulate(labels(terms(f)), f[[2]])[[3]]
})

gam_mods <- lapply(gam_cox_formulas, function(fc) {
  f <- formula(fc)
  nice_formula <- reformulate(labels(terms(f)), f[[2]])[[3]]

  reml_mod <- gam(
    formula = formula(f),
    family = cox.ph,
    data = heart,
    weights = event,
    method = "REML"
  )
  ml_mod <- gam(
    formula = formula(f),
    family = cox.ph,
    data = heart,
    weights = event,
    method = "ML"
  )

  list( reml = reml_mod,
    ml = ml_mod,
    aic = AIC(ml_mod),
    deviance = ml_mod$deviance,
    formula = fc
  )
})

gam_table_aic <- bind_rows(lapply(gam_mods, function(m) {
  m[c("formula", "aic", "deviance")]
}))

final_gam_model_cox <- gam_mods[[which.min(gam_table_aic$aic)]]

# # Create table from AIC/deviance
gam_mods_table <- bind_rows(lapply(gam_mods, function(mod) {
  par_terms <- cbind(tidy(mod$reml, parametric = TRUE), parametric = TRUE)
  nonpar_terms <- tidy(mod$reml, parametric = FALSE)

  if (nrow(nonpar_terms) == 0) {
    return(par_terms)
  } else {
    return(bind_rows(par_terms, cbind(nonpar_terms, parametric = FALSE)))
  }
}), .id = "mod")


heart <- heart2
