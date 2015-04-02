
library(survey)
library(parallel)

load("data/main-data-cps-all-years.RData")

cl <- makeCluster(7)
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


mdl_fun_a <- pearnval ~ edu_rcd + a_age + I(a_age ^ 2)
clusterExport(cl, "mdl_fun_a")

mdl <- parLapply(
  cl, cps_svy, 
  function(x){summary(svyglm(mdl_fun_a, design = x))}
  )

stopCluster(cl)

tbl_age_group <- lapply(
  names(tbl_age_group), 
  function(x){transform(tbl_age_group[[x]], year = as.numeric(x))}
)

tbl_age_group <- do.call("rbind", tbl_age_group)

#save(
#  tbl_age_group, mdl, 
#  file = "analysis/analysis-export.RData"
#  )