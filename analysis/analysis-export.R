
library(survey)
library(parallel)
library(texreg)

load("data/main-data-cps-all-years.RData")

sim_ages <- 25:75
sim_edu <- levels(cps_svy[[1]]$variables$edu_rcd)
sim_hh <- levels(cps_svy[[1]]$variables$hh_rcd)
len_ages <- length(25:75)
len_edu <- length(sim_edu)
len_hh <- length(sim_hh)

sim_vars_a <- data.frame(
  a_age = sim_ages, 
  edu_rcd = rep(sim_edu, each = len_ages)
  )

sim_vars_b <- data.frame(
  a_age = sim_ages, 
  edu_rcd = rep(sim_edu, each = len_ages), 
  hh_rcd = rep(sim_hh, each = len_ages*len_edu)
  )

cl <- makeCluster(7)
clusterEvalQ(cl, library(survey))
clusterEvalQ(cl, library(texreg))

tbl_age_group <- parLapply(
  cl, cps_svy, 
  function(x){
    survey::svyby(
      ~ pearnval, 
      ~ age_rcd + edu_rcd + hh_rcd, 
      x, svymean, 
      keep.names = FALSE
     ) 
   }
  )

stopCluster(cl)

mdl_fun_a <- pearnval ~ edu_rcd + a_age + I(a_age ^ 2)
mdl_fun_b <- pearnval ~ edu_rcd + hh_rcd + a_age + I(a_age ^ 2)
mdl_fun_c <- pearnval ~ edu_rcd + hh_rcd*a_age + hh_rcd*I(a_age ^ 2)
#clusterExport(cl, c("mdl_fun_a", "mdl_fun_b", "mdl_fun_c"))



mdl_ols_a <- lapply(
  cps_svy, 
  function(x){
    lm_out <- lm(mdl_fun_a, data = x$variables)
    list(
      tab = extract(lm_out), 
      sim = cbind(sim_vars_a, predict(lm_out, sim_vars_a))
    )}
  )

mdl_ols_b <- lapply(
  cps_svy, 
  function(x){
    lm_out <- lm(mdl_fun_b, data = x$variables)
    list(
      tab = extract(lm_out), 
      sim = cbind(sim_vars_b, predict(lm_out, sim_vars_b))
    )}
  )

mdl_ols_c <- lapply(
  cps_svy, 
  function(x){
    lm_out <- lm(mdl_fun_c, data = x$variables)
    list(
      tab = extract(lm_out), 
      sim = cbind(sim_vars_b, predict(lm_out, sim_vars_b))
    )}
  )

mdl_ols_c_sim <- lapply(
  names(mdl_ols_c), 
  function(x){
    transform(mdl_ols_c[[x]][[2]], year = as.numeric(x))
  }
  )
mdl_ols_c_sim <- do.call("rbind", mdl_ols_c_sim)

tbl_age_group <- lapply(
  names(tbl_age_group), 
  function(x){transform(tbl_age_group[[x]], year = as.numeric(x))}
  )

tbl_age_group <- do.call("rbind", tbl_age_group)



#save(
#  tbl_age_group, mdl, 
#  file = "analysis/analysis-export.RData"
#  )

ggplot(subset(mdl_ols_c_sim, year %in% c(1992, 1998, 2003, 2008, 2013))) + 
  geom_line(aes(a_age, predict.lm_out..sim_vars_b., color = hh_rcd)) + 
  facet_grid(edu_rcd ~ year)