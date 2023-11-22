#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(lavaan)
library(tidyverse)
#run on computing cluster - example script for multiple regression outcome models. Run for each outcome. high order model

outcome_name <- args[1]
data_file_path <- args[2]
output_filepath <- args[3]
categorical_out <- args[4]
return_fit <- args[5]
jobname <- args[6]

modelRun_h <- function(outcome_name, 
                         data_file_path, 
                         output_filepath, 
                         categorical_out = TRUE,
                         return_fit = FALSE,
                         jobname) { 
  

  model_gen <- paste0('
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

Gen =~ motor + language + prosocial + nvcc + rrb + social_recept_int + adhd + idio_speech + play + wait + impulse

#Regression
', outcome_name, '~ Gen')

model_domain <- paste0('
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


Gen =~ motor + language + prosocial + nvcc + rrb + social_recept_int + adhd + idio_speech + play + wait + impulse

#Regression
', outcome_name, '~ motor + c(lang.1,lang.2)*language + prosocial + c(nvc.1, nvc.2)*nvcc + c(rb.1,rb.2)*rrb + c(soc_rep.1, soc_rep.2)*social_recept_int + c(ain.1, ain.2)*adhd + c(isp.1,isp.2)*idio_speech + c(pl.1,pl.2)*play + c(hyp.1,hyp.2)*wait + c(aothr.1,aothr.2)*impulse



#group 2
#lang.2  < 0.3
#pl.2 < 0.3
#soc_rep.2 < 0.3
#nvc.2 < 0.3
#rb.2 < 0.3
#isp.2 < 0.3
#lang.2  > -0.3
#pl.2 > -0.3
#soc_rep.2 > -0.3
#nvc.2 > -0.3
#rb.2 > -0.3
#isp.2 > -0.3

                  ')    
 
  data <- readRDS(data_file_path)
  
  if(categorical_out == TRUE){ 
    fit_gen<- lavaan::cfa(model_gen,
                     data = data,
                     estimator = "WLSMV",
                     missing = "pairwise",
                     std.lv = TRUE,
					 orthogonal = TRUE,
                     group = "sex",
                     ordered = c(grep("GG",colnames(data), value = TRUE), outcome_name)) 
    fit_domain<- lavaan::cfa(model_domain,
                          data = data,
                          estimator = "WLSMV",
                          missing = "pairwise",
						  orthogonal = TRUE,
                          std.lv = TRUE,
                          group = "sex",
                          ordered = c(grep("GG",colnames(data), value = TRUE), outcome_name)) 
  }
  
  if(categorical_out == FALSE){ 
    fit_gen<- lavaan::cfa(model_gen,
                         data = data,
                         estimator = "WLSMV",
                         missing = "pairwise",
                         std.lv = TRUE,
						 orthogonal = TRUE,
                         group = "sex",
                         ordered = c(grep("GG",colnames(data), value = TRUE)))
    fit_domain<- lavaan::cfa(model_domain,
                          data = data,
                          estimator = "WLSMV",
                          missing = "pairwise",
						  orthogonal = TRUE,
                          std.lv = TRUE,
                          group = "sex",
                          ordered = c(grep("GG",colnames(data), value = TRUE)))
  }
  
  
  anova_result <- lavaan::lavTestLRT(fit_gen, fit_domain)
  
    if(return_fit == TRUE) {
    fit <- list(fit_domain, fit_gen)
    saveRDS(fit, paste0(output_filepath,"/",jobname,"_higherorder_fitobj.rds"))}
  
  
  
  #extracting outputs for gen
  grp_1 <- strsplit(fit_gen@Data@group.label[1],"")[[1]][[1]]
  grp_2 <- strsplit(fit_gen@Data@group.label[2],"")[[1]][[1]]
  
  model_est <- lavaan::parameterEstimates(fit_gen, standardized = TRUE)
  model_std <- standardizedsolution(fit_gen) %>% filter (op == "~" )
  
  reg_est <- model_est %>% 
    filter(op == "~")
  
  reg_1 <- reg_est %>% filter(group == 1) %>% select(c(lhs, op, est, se, pvalue, ci.lower, ci.upper, std.all, rhs)) %>%
    rename_with(., ~paste0(., "_", grp_1), .cols = -c(lhs,op,rhs))
  reg_2 <- reg_est %>% filter(group == 2) %>% select(c(lhs, op, est, se, pvalue, ci.lower, ci.upper, std.all, rhs)) %>%
    rename_with(., ~paste0(., "_", grp_2), .cols = -c(lhs,op,rhs))
  
  reg_est <- merge(reg_1, reg_2, by = c("lhs", "op", "rhs"), all = T)
  
  std_1 <- model_std %>% filter(group == 1) %>% select(c(lhs, op, est.std, se, pvalue, ci.lower, ci.upper,rhs)) %>%
    rename_with(., ~paste0(., "_", grp_1), .cols = -c(lhs,op,rhs))
  std_2 <- model_std %>% filter(group == 2) %>% select(c(lhs, op, est.std, se, pvalue, ci.lower, ci.upper,rhs)) %>%
    rename_with(., ~paste0(., "_", grp_2), .cols = -c(lhs,op,rhs))
  
  std_est <- merge(std_1, std_2, by = c("lhs", "op", "rhs"), all = T)
  
  
  reg_est_sig_m <- std_est %>% filter(., pvalue_M < 0.01)
  reg_est_sig_f <- std_est %>% filter(., pvalue_F < 0.01)
  
  heywood <- model_est %>% filter(op == "=~") %>% filter(std.all > 1) %>% select(c(lhs, rhs, std.all, group)) %>%
    mutate(group, if_else(group == 1, grp_1, grp_2)) 
  
  regression.estimates <- list(std.est = std_est, est.all = reg_est, sig.Male = reg_est_sig_m, sig.Female = reg_est_sig_f)
  
  fit.measures_g <- lavaan::fitmeasures(fit_gen)
  
  output_gen <- list(converged = fit_gen@Fit@converged, 
				 nobs = fit_gen@Data@nobs,
				 groups = fit_gen@Data@group.label,
         r2.Female = lavInspect(fit_gen, what = "r2")$Female[outcome_name], 
         r2.Male = lavInspect(fit_gen, what = "r2")$Male[outcome_name], 
				 lambda.Female = lavInspect(fit_gen, what = "std")$Female["lambda"], 
         lambda.Male = lavInspect(fit_gen, what = "std")$Male["lambda"],
				 tau.Female = lavInspect(fit_gen, what = "std")$Female["tau"],
				 tau.Male = lavInspect(fit_gen, what = "std")$Male["tau"],
				 psi.Female = lavInspect(fit_gen, what = "std")$Female["psi"],
				 psi.Male = lavInspect(fit_gen, what = "std")$Male["psi"],
         regression.estimates = regression.estimates, 
         fit.measures = fit.measures_g, 
         heywood.cases = heywood)
  
  #extracting outputs for domain
  grp_1_d <- strsplit(fit_domain@Data@group.label[1],"")[[1]][[1]]
  grp_2_d <- strsplit(fit_domain@Data@group.label[2],"")[[1]][[1]]
  
  model_est_d <- lavaan::parameterEstimates(fit_domain, standardized = TRUE)
  model_std_d <- standardizedsolution(fit_domain) %>% filter (op == "~" )
  
  reg_est_d <- model_est_d %>% 
    filter(op == "~")
  
  reg_1_d <- reg_est_d %>% filter(group == 1) %>% select(c(lhs, op, est, se, pvalue, ci.lower, ci.upper, std.all, rhs)) %>%
    rename_with(., ~paste0(., "_", grp_1_d), .cols = -c(lhs,op,rhs))
  reg_2_d <- reg_est_d %>% filter(group == 2) %>% select(c(lhs, op, est, se, pvalue, ci.lower, ci.upper, std.all, rhs)) %>%
    rename_with(., ~paste0(., "_", grp_2_d), .cols = -c(lhs,op,rhs))
  
  reg_est_d <- merge(reg_1_d, reg_2_d, by = c("lhs", "op", "rhs"), all = T)
  
  std_1_d <- model_std_d %>% filter(group == 1) %>% select(c(lhs, op, est.std, se, pvalue, ci.lower, ci.upper,rhs)) %>%
    rename_with(., ~paste0(., "_", grp_1_d), .cols = -c(lhs,op,rhs))
  std_2_d <- model_std_d %>% filter(group == 2) %>% select(c(lhs, op, est.std, se, pvalue, ci.lower, ci.upper,rhs)) %>%
    rename_with(., ~paste0(., "_", grp_2_d), .cols = -c(lhs,op,rhs))
  
  std_est_d <- merge(std_1_d, std_2_d, by = c("lhs", "op", "rhs"), all = T)
  
  
  reg_est_sig_m_d <- std_est_d %>% filter(., pvalue_M < 0.01)
  reg_est_sig_f_d <- std_est_d %>% filter(., pvalue_F < 0.01)
  
  heywood_d <- model_est_d %>% filter(op == "=~") %>% filter(std.all > 1) %>% select(c(lhs, rhs, std.all, group)) %>%
    mutate(group, if_else(group == 1, grp_1_d, grp_2_d)) 
  
  regression.estimates_d <- list(std.est = std_est_d, est.all = reg_est_d, sig.Male = reg_est_sig_m_d, sig.Female = reg_est_sig_f_d)
  
  fit.measures_d <- lavaan::fitmeasures(fit_domain)
  
  output_domain <- list(converged = fit_domain@Fit@converged, 
                     nobs = fit_domain@Data@nobs,
                     groups = fit_domain@Data@group.label,
                     r2.Female = lavInspect(fit_domain, what = "r2")$Female[outcome_name], 
                     r2.Male = lavInspect(fit_domain, what = "r2")$Male[outcome_name], 
                     lambda.Female = lavInspect(fit_domain, what = "std")$Female["lambda"], 
                     lambda.Male = lavInspect(fit_domain, what = "std")$Male["lambda"],
                     tau.Female = lavInspect(fit_domain, what = "std")$Female["tau"],
                     tau.Male = lavInspect(fit_domain, what = "std")$Male["tau"],
                     psi.Female = lavInspect(fit_domain, what = "std")$Female["psi"],
                     psi.Male = lavInspect(fit_domain, what = "std")$Male["psi"],
                     regression.estimates = regression.estimates_d, 
                     fit.measures = fit.measures_d, 
                     heywood.cases = heywood_d)
  

  
  #combined regression 
  
  reg_est_all <- rbind(reg_est, reg_est_d)
  std_est_all <- rbind(std_est, std_est_d)
  est_sig_f_all <- rbind(reg_est_sig_f, reg_est_sig_f_d)
  est_sig_m_all <- rbind(reg_est_sig_m, reg_est_sig_m_d)
  
  regression.estimates.all <- list(std.est = std_est_all, est.all = reg_est_all, sig.Male = est_sig_m_all, sig.Female = est_sig_f_all)
  
  output <- list(general = output_gen,
                 domain = output_domain, 
                 regression.estimates = regression.estimates.all,
                 anova = anova_result)
  
  saveRDS(output, paste0(output_filepath,"/",jobname,"_higherorder.rds"))
  }
 

obj <- modelRun_h(outcome_name, 
                    data_file_path, 
                    output_filepath, 
                    categorical_out,
                    return_fit,
                    jobname)
