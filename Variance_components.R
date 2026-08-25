library(brms); library(posterior)
load("RData/creatinine_models.RData"); load("RData/bun_models.RData")

f <- function(x) sprintf("%.2f (%.2f-%.2f)", median(x), quantile(x, .025), quantile(x, .975))
vc <- function(m) {
  d  <- as_draws_df(m)
  s1 <- d[["sd_STUDY__Intercept"]]; s2 <- d[["sd_STUDY:SITE_STUDY__Intercept"]]
  data.frame(n = nobs(m),
             n_study = length(unique(m$data$STUDY)),
             n_site  = length(unique(m$data$SITE_STUDY)),
             sd_study = f(s1), sd_site = f(s2),
             sd_total = f(sqrt(s1^2 + s2^2)),
             ICC = f((s1^2 + s2^2) / (s1^2 + s2^2 + pi^2/3)),
             MOR_study = f(exp(sqrt(2 * s1^2) * qnorm(0.75))),
             MOR_site  = f(exp(sqrt(2 * s2^2) * qnorm(0.75))))
}
mods <- list(`Cr FC (Pottel/CG), t2`   = Creat_tensor_prod_fit_Pottel_CG,
             `Cr FC (Schwartz), t2`    = Creat_tensor_prod_fit_Schwartz,
             `Cr absolute, t2`         = Creat_tensor_prod_fit_abs,
             `Cr FC (Pottel/CG), fs`   = Creat_fs_mod_fit_Pottel_CG,
             `Cr FC (Schwartz), fs`    = Creat_fs_mod_fit_Schwartz,
             `BUN, t2`                 = BUN_tensor_prod_fit,
             `BUN, fs`                 = BUN_fs_mod_fit)
do.call(rbind, lapply(mods, vc))
