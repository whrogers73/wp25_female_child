
library(survey)
library(parallel)

load("data/main-data-cps-all-years.RData")

cl <- makeCluster(6)
clusterEvalQ(cl, library(survey))

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

tbl_age_group <- lapply(
  names(tbl_age_group), 
  function(x){transform(tbl_age_group[[x]], year = as.numeric(x))}
  )
tbl_age_group <- do.call("rbind", tbl_age_group)

mdl_fun_a <- pearnval ~ edu_rcd + a_age + I(a_age ^ 2)
mdl_fun_b <- pearnval ~ edu_rcd + hh_rcd + a_age + I(a_age ^ 2)
mdl_fun_c <- pearnval ~ edu_rcd + hh_rcd*a_age + hh_rcd*I(a_age ^ 2)

llply(cps_svy, function(x){summary(svyglm(mdl_fun_a, design = x))})


#ggplot(tbl_age, aes(a_age, pearnval, colour = hh_rcd)) + 
#  geom_pointrange(aes(ymin = I(pearnval - se), ymax = I(pearnval + se))) +
#  facet_wrap(~ edu_rcd) + 
#  scale_y_continuous() + 
#  theme_bw()

mdl_reg_a <- summary(svyglm(mdl_fun_a, design = cps_svy[["2014"]]))
mdl_reg_b <- summary(svyglm(mdl_fun_b, design = cps_svy[["2014"]]))
mdl_reg_c <- summary(svyglm(mdl_fun_c, design = cps_svy[["2014"]]))

AIC(mdl_reg_a, mdl_reg_b, mdl_reg_c)

