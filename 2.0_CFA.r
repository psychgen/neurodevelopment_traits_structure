#Prep for CFA
#went forward with first subset of data 9-11 factors extracted 
library(tidyverse)
library(readxl)
library(openxlsx)
library(lavaan)

#read in factor loading tables

#select items for factors 
#Items only allowed to load onto one factor
# 1. For a factor to be identified - top 3 loading items on that factor are assigned to it
# 2. Then remaining items assigned to factor in which it loads most strongly onto
# 3. Only items with loadings above 0.3 are to be included 


#function that leaves only values in a factors top 3 loadings or a row max and removes values under 0.3

#function that leaves only values in a factors top 3 loadings or a row max
assign_val <- function(df){
  
  max3_col <- function(x){
    top_3 <-sort(x, decreasing = TRUE)[1:3]
    return(top_3)
  }
  
  top3_list <- suppressWarnings(apply(X=df[2:length(df)], MARGIN=2, FUN=max3_col))
  
  maxrow_list <- suppressWarnings(apply(X=df[2:length(df)], MARGIN=1, FUN=max, na.rm =TRUE))
  maxrow_list <- maxrow_list[maxrow_list != -Inf]
  
  filter_fun <- function(x){ 
    ifelse(x %in% top3_list, x, ifelse(
      x %in% maxrow_list, x, NA))
    
    
  }
  out <- df %>%
    mutate_if(is.numeric, filter_fun) %>%
    mutate(across(.cols = starts_with("factor"), .fns = ~ifelse(. < 0.3, NA_real_, .))) %>%
    mutate(isnot.na = rowSums(!is.na(.[,2:ncol(.)])))
  return(out)
  
}


#reading in loading tables for CFAs
#11 factors
factor.11 <- read_excel("results/results_final/EFA_0threshold_clustered.xlsx", sheet = "11_factor") %>%
  mutate(across(.cols = starts_with("factor"), .fns = ~str_remove(.,"\\*"))) %>%
  mutate(across(.cols = starts_with("factor"), .fns = as.numeric)) %>%
  assign_val(.)

rownames(factor.11) <- factor.11$item

#items to manually check
item.check.11 <- factor.11 %>% 
  filter(isnot.na != 0 & isnot.na != 1) %>%
  row.names(.)  #"GG247" "GG249" "GG258" "GG261" "GG262"


#items without a factor
item.na.11 <- factor.11 %>% 
  filter(isnot.na == 0) %>%
  row.names(.) # "GG254" "GG274" "GG330"

# vectors of items in each factor to double check with full spread sheet 
motor.11 <- factor.11 %>% filter(!is.na(factor_1)) %>% rownames()
lang.11 <- factor.11 %>% filter(!is.na(factor_2)) %>% rownames()
prosoc.11 <- factor.11 %>% filter(!is.na(factor_3)) %>% rownames()
nvcc.11 <- factor.11 %>% filter(!is.na(factor_4)) %>% filter(!(item %in% c("GG256", "GG281"))) %>% rownames() #removed GG256 and GG281
rrb.11 <- factor.11 %>% filter(!is.na(factor_5)) %>% filter(!(item %in% c("GG262", "GG261", "GG258"))) %>%rownames() #removed GG262, GG261, and GG258
socint.11 <- factor.11 %>% filter(!is.na(factor_6)) %>% rownames()
adhd.11 <- factor.11 %>% filter(!is.na(factor_7)) %>% filter(!(item %in% c("GG249", "GG342"))) %>% rownames() #removed GG249 and GG342
idiospeech.11  <- factor.11 %>% filter(!is.na(factor_8)) %>% rownames() 
play.11 <- factor.11 %>% filter(!is.na(factor_9)) %>% filter(!(item %in% c("GG247"))) %>% rownames() #note GG247 if following criteria should be removed from soc interaction but makes more theoretical sense to remove from here instead
wait.11 <- factor.11 %>% filter(!is.na(factor_10)) %>% rownames()
impulse.11 <- factor.11 %>% filter(!is.na(factor_11)) %>% rownames()


model.11.factors <- '

motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG227 + GG228 + GG229 + GG230 + GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287
rrb =~ GG251 + GG260 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG261 + GG262
play =~ GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342

'
# with constraints for -ov estimates 

model.11.factors.1 <- '

motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG227 + GG228 + GG229 + GG230 + l1*GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + l2*GG287
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

#model for sensitivity analysis allowing correlated error variance of GG342 (excessive talking) with language factor 

model.11.factors.2 <- '

motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ GG227 + GG228 + GG229 + GG230 + l1*GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + l2*GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
impulse=~ GG327 + GG339 + GG342

l1 < .95
l2 < .95


language ~~ GG342
'




#10 factors

factor.10 <- read_excel("results/results_final/EFA_0threshold_clustered.xlsx", sheet = "10_factor") %>%
  mutate(across(.cols = starts_with("factor"), .fns = ~str_remove(.,"\\*"))) %>%
  mutate(across(.cols = starts_with("factor"), .fns = as.numeric)) %>%
  assign_val(.)
rownames(factor.10) <- factor.10$item

#items to manually check
item.check.10 <- factor.10 %>% 
  filter(isnot.na != 0 & isnot.na != 1) %>%
  row.names(.) #"GG237" "GG247" "GG249" "GG258" "GG261" "GG262" "GG290" "GG293" "GG340"


#items without a factor
item.na.10 <- factor.10 %>% 
  filter(isnot.na == 0) %>%
  row.names(.) # "GG254" "GG274" "GG330"

# vectors of items in each factor to double check with full spread sheet 
motor.10 <- factor.10 %>% filter(!is.na(factor_1)) %>% rownames()
lang.10 <- factor.10 %>% filter(!is.na(factor_2)) %>% filter(!(item %in% c("GG293"))) %>% rownames()
soccom.10 <- factor.10 %>% filter(!is.na(factor_3)) %>% filter(!(item %in% c("GG247", "GG290", "GG237"))) %>% rownames()
prosoc.10 <- factor.10 %>% filter(!is.na(factor_4)) %>% rownames() 
rrb.10 <- factor.10 %>% filter(!is.na(factor_5)) %>% filter(!(item %in% c("GG262", "GG261", "GG258"))) %>% rownames() 
adhdin.10<- factor.10 %>% filter(!is.na(factor_6)) %>% filter(!(item %in% c("GG249", "GG340")))%>% rownames()
idiospeech.10 <- factor.10 %>% filter(!is.na(factor_7)) %>% rownames() 
socint.10  <- factor.10 %>% filter(!is.na(factor_8)) %>% rownames() 
play.10 <- factor.10 %>% filter(!is.na(factor_9)) %>% rownames() 
adhdhyp.10 <- factor.10 %>% filter(!is.na(factor_10)) %>% rownames()


model.10.factors <- ' 
motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
soc_com =~ GG227 + GG228 + GG229 + GG230 + GG243 + GG244 + GG245 + GG246 + GG248 + GG249 + GG250 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG281 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287 + GG288 + GG292
rrb =~ GG251 + GG260 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
adhd =~ GG314 + GG332 + GG315 + GG327 + GG339 + GG341 + GG345 + GG342
idio_speech =~ GG258 + GG261 + GG262
soc_int =~ GG247 + GG290 + GG291
play =~ GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
'

# with constraints for -ov estimates
model.10.factors.1 <- ' 
motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
soc_com =~ GG227 + GG228 + GG229 + GG230 + GG243 + l1*GG244 + GG245 + GG246 + GG248 + GG249 + GG250 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG281 + GG282 + GG283 + GG284 + GG285 + GG286 + l2*GG287 + GG288 + GG292
rrb =~ GG251  + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
adhd =~ GG314 + GG332 + GG315 + GG327 + GG339 + GG341 + GG345 + GG342
idio_speech =~ GG258 + GG260 + GG261 + GG262
soc_int =~ l3*GG247 + GG290 + GG291
play =~ GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340

l1 < .95
l2 < .95
l3 < .95
'

# 9 factors extracted


factor.9 <- read_excel("results/results_final/EFA_0threshold_clustered.xlsx", sheet = "9_factor") %>%
  mutate(across(.cols = starts_with("factor"), .fns = ~str_remove(.,"\\*"))) %>%
  mutate(across(.cols = starts_with("factor"), .fns = as.numeric)) %>%
  assign_val(.) 


rownames(factor.9) <- factor.9$item
#items to manually check

item.check.9 <- factor.9 %>% 
  filter(isnot.na != 0 & isnot.na != 1) %>%
  row.names(.) #"GG222" "GG247" "GG258" "GG261" "GG262" "GG289" "GG316" "GG340"

# vectors of items in each factor to double check with full spread sheet 
motor.9 <- factor.9 %>% filter(!is.na(factor_1)) %>% rownames()
soccom.9 <- factor.9 %>% filter(!is.na(factor_2)) %>% filter(!(item %in% c("GG222", "GG247"))) %>% rownames()
lang.9 <- factor.9 %>% filter(!is.na(factor_3)) %>% filter(!(item %in% c("GG289"))) %>% rownames()
prosoc.9 <- factor.9 %>% filter(!is.na(factor_4)) %>% rownames() 
rrb.9 <- factor.9 %>% filter(!is.na(factor_5)) %>% filter(!(item %in% c("GG262", "GG261", "GG258", "GG254"))) %>% rownames() #GG254 under 0.3
adhdin.9<- factor.9 %>% filter(!is.na(factor_6)) %>% filter(!(item %in% c("GG316", "GG340")))%>% rownames()
idiospeech.9 <- factor.9 %>% filter(!is.na(factor_7)) %>%  #GG261 under 0.3
  play.9 <- factor.9 %>% filter(!is.na(factor_8)) %>% filter(!(item %in% c("GG274"))) %>% rownames() #GG274 under 0.3
adhdhyp.9 <- factor.9 %>% filter(!is.na(factor_9)) %>% rownames()


paste(play.9, collapse = " + ")

model.9.factors <- ' 
motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
soc_com_int =~ GG227 + GG228 + GG229 + GG230 + GG243 + GG244 + GG245 + GG246 + GG248 + GG249 + GG250 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG281 + GG282 + GG283 + GG284 + GG285 + GG286 + GG287 + GG288 + GG290 + GG291 + GG292 + GG330
rrb =~ GG251 + GG260 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
adhd =~ GG314 + GG332 + GG315 + GG327 + GG339 + GG341 + GG345 + GG342
idio_speech =~ GG258 + GG261 + GG262
play =~ GG247 + GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340
'

#with constraints 


model.9.factors.1 <- ' 
motor =~ GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
soc_com_int =~ GG227 + GG228 + GG229 + GG230 + GG243 + l1*GG244 + GG245 + GG246 + l3*GG247 + GG248 + GG249 + GG250 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG281 + GG282 + GG283 + GG284 + GG285 + GG286 + l2*GG287 + GG288 + GG290 + GG291 + GG292 
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
adhd =~ GG314 + GG332 + GG315 + GG327 + GG339 + GG341 + GG345 + GG342
idio_speech =~ GG258 + GG260 +  GG261 + GG262
play =~  GG289 + GG293 + GG294
wait=~ GG316 + GG320 + GG340


l1 < 1
l2 < 1
l3 < 1
'

###### running models ######

dat <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds") %>%
  filter(split.50 == 1 ) #split sample

#models based on EFA
fit.11f <- cfa(model.11.factors, 
               data = dat, 
               estimator = "WLSMV",
               missing = "pairwise",
               ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.11f, file = "output/fit.11f.rds")

fit.10f <- cfa(model.10.factors, 
               data = dat, 
               estimator = "WLSMV",
               missing = "pairwise",
               ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.10f, file = "output/fit.10f.rds")

fit.9f <- cfa(model.9.factors, 
              data = dat, 
              estimator = "WLSMV",
              missing = "pairwise",
              ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.9f, file = "output/fit.9f.rds")

#models with constraints and sensitivity analyses 
fit.11f.1 <- cfa(model.11.factors.1, #items with -ov loadings constrained to < 1, GG260 (pronouns question) moved to indo. speech, GG247 (interest in children) moved to social recpt. and interest
                 data = dat, 
                 estimator = "WLSMV",
                 missing = "pairwise",
                 std.lv = TRUE,
                 ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.11f, file = "output/fit.11f_1.rds")

fit.11f.2 <- cfa(model.11.factors.2, #with lang ~~ excess talking
                 data = dat, 
                 estimator = "WLSMV",
                 missing = "pairwise",
                 std.lv = TRUE,
                 ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.11f.2, file = "output/fit.11f_2.rds")



fit.10f.1 <- cfa(model.10.factors.1,  #items with -ov loadings constrained to < 1, GG260 (pronouns question) moved to indo. speech
                 data = dat, 
                 estimator = "WLSMV",
                 missing = "pairwise",
                 std.lv = TRUE,
                 ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.10f, file = "output/fit.10f_1.rds")


fit.9f.1 <- cfa(model.9.factors.1,  #items with -ov loadings constrained to < 1, GG260 (pronouns question) moved to indo. speech, GG247 (interest in children) moved to social communication and interest, GG330 (clumsy) removed 
                data = dat, 
                estimator = "WLSMV",
                missing = "pairwise",
                std.lv = TRUE,
                ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.9f, file = "output/fit.9f_1.rds")



### 11 factor removing siblings 
length(unique(dat$m_id)) #n without siblings 

dat_unrelated <- dat %>% 
  distinct(m_id, .keep_all = TRUE)

fit.11f.1_unrelated <- cfa(model.11.factors.1, #using model with -ov neg items constrained 
                 data= dat_unrelated,
                 estimator = "WLSMV",
                 missing = "pairwise",
                 std.lv = TRUE,
                 ordered = c(grep("GG",colnames(dat), value = TRUE)))

saveRDS(fit.11f.1_unrelated, file = "output/fit.11f_unrelat.rds")

fits <- list(model = c(fit.11f,fit.10f,fit.9f,fit.11f.1,fit.10f.1,fit.9f.1,fit.11f.2, fit.11f.1_unrelated), 
             name =c("11_factor", "10_factor", "9_factor","11_factor_constrained", "10_factor_constrained", "9_factor_constrained", "11_factor_lang", "11_factor_unrelated")) %>%
  as_tibble() %>%
  mutate(cfi = map_dbl(model, ~fitmeasures(., fit.measures = "cfi.scaled")),
         tli = map_dbl(model, ~fitmeasures(., fit.measures = "tli.scaled")),
         rmsea = map_dbl(model, ~fitmeasures(., fit.measures = "rmsea.scaled")), 
         srmr = map_dbl(model, ~fitmeasures(., fit.measures = "srmr"))) %>%
  select(-model)

write.xlsx(fits, file = "results/results_final/CFA_halfsample_modelfits.xlsx")
