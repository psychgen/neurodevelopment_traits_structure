library(tidyverse)
library(lavaan)
library(semTools)
#measurement Invariance 

data <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds")

#model
corfactors <- '

motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG227 + GG228 + GG229 + GG230 + GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342


'

#MG-CFA

corfactors_sex.fit_delta <- cfa(corfactors,
                                data = data, 
                                estimator = "WLSMV",
                                missing = "pairwise",
                                parameterization = "delta",
                                std.lv = TRUE,
                                group = "sex",
                                ordered = c(grep("GG",colnames(data), value = TRUE)))

saveRDS(corfactors_sex.fit, file = "output/corfactors_sex.rds")

#model constraining heywood cases from MG-CFA
corfactors_constrain <- '

motor =~ c(l222.1, l222.2)*GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ c(l227.1, l227.2)*GG227 + GG228 + GG229 + GG230 + c(l244.1, l244.2)*GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + c(l247.1,l247.2)*GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342

l222.2 < 0.95
l244.2 < 0.965
l244.1 < 0.98
l227.1 < 0.93
l247.1 < 0.98

'

#re-run new model 

corfactors_sex.fit_delta <- cfa(corfactors_constrain,
                                data = data, 
                                estimator = "WLSMV",
                                missing = "pairwise",
                                parameterization = "delta",
                                std.lv = TRUE,
                                group = "sex",
                                ordered = c(grep("GG",colnames(data), value = TRUE)))



#Formal Measurement Invariance 

mi.list_cor <- list()

mi.list_cor$baseline <- measEq.syntax(configural.model = corfactors_constrain,
                                      data = data,
                                      ordered = c(grep("GG",colnames(data), value = TRUE)),
                                      parameterization = "delta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = "sex",
                                      group.equal = "configural" ,
                                      missing = "pairwise",
                                      return.fit = TRUE)

mi.list_cor$prop.11 <- measEq.syntax(configural.model = corfactors_constrain,
                                     data = data,
                                     ordered = c(grep("GG",colnames(data), value = TRUE)),
                                     parameterization = "delta",
                                     ID.fac = "std.lv",
                                     ID.cat = "Wu.Estabrook.2016",
                                     group = "sex",
                                     group.equal = c("thresholds", "loadings", "intercepts") ,
                                     missing = "pairwise",
                                     return.fit = TRUE)

mi.results <- compareFit(mi.list_cor)

loadings_diff <- lavInspect(mi.list_cor$baseline, what = "std")$Female$lambda -lavInspect(mi.list_cor$baseline, what = "std")$Male$lambda
threshold_diff <- lavInspect(mi.list_cor$baseline, what = "std")$Female$tau -lavInspect(mi.list_cor$baseline, what = "std")$Male$tau
psi_dif <- lavInspect(mi.list_cor$baseline, what = "std")$Female$psi -lavInspect(mi.list_cor$baseline, what = "std")$Male$psi

saveRDS(mi.list_cor, file = "N:/durable/projects/neurodev_structure/output/measurement_invariance.rds")

#fit

fit <- list(model_name = c("baseline", "prop.11"),
            model = c( mi.list_cor$baseline, mi.list_cor$prop.11)) %>%
  as_tibble() %>%
  mutate(cfi = map_dbl(model, ~fitmeasures(., fit.measures = "cfi.scaled")),
         tli = map_dbl(model, ~fitmeasures(., fit.measures = "tli.scaled")),
         rmsea = map_dbl(model, ~fitmeasures(., fit.measures = "rmsea.scaled")), 
         srmr = map_dbl(model, ~fitmeasures(., fit.measures = "srmr"))) %>%
  select(-model)



openxlsx::write.xlsx(fit, file = "results/results_final/measurement_invariance.xlsx")
