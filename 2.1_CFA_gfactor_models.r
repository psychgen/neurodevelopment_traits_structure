library(tidyverse)
library(lavaan)
library(openxlsx)
source("N:/durable/projects/neurodev_structure/scripts/scripts_final/rename_items.R")


data <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds")

####### Full sample - testing g-factor models ######

#model syntax#

#Constrained cor factor model in full sample

model.11.cor <- '

motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG229 + GG228 + l2*GG227 + GG230 + l1*GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342

l1 < .95
l2 < .95


'

#Bi-factor model 

model.11.bi <- 'motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG229 + GG228 + GG227 + GG230 + GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd_in =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342


Gen =~ GG222 + GG223 + GG224 + GG225 + GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257 + GG231 + GG232 + GG233 + GG234 + GG235 + GG236 + GG227 + GG228 + GG229 + GG230 + 
GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287 + GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273 +
GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292 + GG314 + GG332 + GG315 + GG341 + GG345 + GG258 + GG260 + GG261 + GG262 + GG288 + GG289 + GG293 + GG294 + GG316 + GG320 + GG340 + GG327 + GG339 + GG342 


'

#Higher-order model

model.11.high <- 'motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG229 + GG228 + GG227 + GG230 + GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd_in =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342


Gen =~ motor + language + prosocial + nvcc + rrb + social_recept_int + adhd_in + idio_speech + play + wait + impulse
'


####running models####

fit_cor_full <- cfa(model.11.cor,
    data = data, 
    estimator = "WLSMV",
    missing = "pairwise",
    ordered = c(grep("GG",colnames(data), value = TRUE)))

saveRDS(fit_cor_full, "output/corfactors_fullsample.rds")

cor_table_loadings <- parameterestimates(fit_cor_full, standardized = TRUE) %>%
  filter(op == "=~") %>%  
  select("lhs", "rhs", "std.all", "est", "se", "z", "pvalue") %>%
  rename("factor" = "lhs", 
         "item" = "rhs") %>%
  rename_items()

cor_table_factor_var <- parameterestimates(fit_cor_full) %>%
  filter(op == "~~") %>%  
  filter(!str_starts(lhs, "GG"))%>%
  filter(lhs == rhs)  %>% 
  select("lhs", "est", "se", "z", "pvalue","ci.lower","ci.upper" ) %>%
  filter(!is.na(pvalue)) %>%
  rename("factor" = "lhs")

cor_table_item_var <- parameterestimates(fit_cor_full) %>%
  filter(op == "~~") %>%  
  filter(str_starts(lhs, "GG"))%>%
  select("lhs", "est", "se", "z", "pvalue","ci.lower","ci.upper" ) %>%
  rename("item" = "lhs") %>%
  rename_items()

cor_table_item_threshold <- parameterestimates(fit_cor_full, standardized = TRUE) %>%
  filter(op == "|") %>%  
  filter(str_starts(lhs, "GG"))%>%
  select("lhs", "rhs","std.all","est", "se", "z", "pvalue","ci.lower","ci.upper" ) %>%
  rename("item" = "lhs", 
         "threshold" = "rhs") %>%
  rename_items()


fit_bi <- cfa(model.11.bi,
    data = data, 
    estimator = "WLSMV",
    missing = "pairwise",
    orthogonal = TRUE,
    ordered = c(grep("GG",colnames(data), value = TRUE)))

saveRDS(fit_bi, "output/bifactor_fullsample.rds")

bi_table_factor_var <- parameterestimates(fit_bi) %>%
  filter(op == "~~") %>%  
  filter(!str_starts(lhs, "GG"))%>%
  select("lhs", "est", "se", "z", "pvalue","ci.lower","ci.upper" ) %>%
  filter(!is.na(pvalue)) %>%
  rename("factor" = "lhs")

bi_table_loadings <- parameterestimates(fit_bi, standardized = TRUE) %>%
  filter(op == "=~") %>%  
  select("lhs", "rhs", "std.all", "est", "se", "z", "pvalue") %>%
  rename("factor" = "lhs", 
         "item" = "rhs") %>%
  rename_items()

fit_high <- cfa(model.11.high,
    data = data, 
    estimator = "WLSMV",
    missing = "pairwise",
    orthogonal = TRUE,
    ordered = c(grep("GG",colnames(data), value = TRUE)))

saveRDS(fit_high, "output/higherorder_fullsample.rds")

high_table_factor_var <- parameterestimates(fit_high) %>%
  filter(op == "~~") %>%  
  filter(!str_starts(lhs, "GG"))%>%
  select("lhs", "est", "se", "z", "pvalue","ci.lower","ci.upper" ) %>%
  filter(!is.na(pvalue)) %>%
  rename("factor" = "lhs")

high_table_loadings <- parameterestimates(fit_high, standardized = TRUE) %>%
  filter(op == "=~") %>%  
  select("lhs", "rhs", "std.all", "est", "se", "z", "pvalue") %>%
  rename("factor" = "lhs", 
         "item/factor" = "rhs") %>%
  rename_items(.,col = "item/factor")

 
#table for fit measures 

fits <- list(model = c(fit_high, fit_bi, fit_cor_full), 
             name =c("Higherorder", "Bifactor", "Correlated_Factors")) %>%
  as_tibble() %>%
  mutate(cfi = map_dbl(model, ~fitmeasures(., fit.measures = "cfi.scaled")),
         tli = map_dbl(model, ~fitmeasures(., fit.measures = "tli.scaled")),
         rmsea = map_dbl(model, ~fitmeasures(., fit.measures = "rmsea.scaled")), 
         srmr = map_dbl(model, ~fitmeasures(., fit.measures = "srmr"))) %>%
  select(-model)

write.xlsx(fits, file = "results/results_final/CFA_fullsample_modelfits.xlsx")

#CFA full sample
cor_tables <- list("factor variance est." = cor_table_factor_var, 
                   "item threshold est." =cor_table_item_threshold, 
                   "item variance est." =cor_table_item_var, 
                   "item loadings est." =cor_table_loadings) 
write.xlsx(cor_tables, file = "results/results_final/CFA_correlated_factor_model_fullsample.xlsx")

#general factor models 
gen_tables <- list("bifactor factor var est." = bi_table_factor_var, 
                   "bifactor loadings est." = bi_table_loadings,
                   "higherorder factor var est." = high_table_factor_var, 
                   "higherorder loadings est." = high_table_loadings)
write.xlsx(gen_tables, file = "results/results_final/CFA_general_models.xlsx")
