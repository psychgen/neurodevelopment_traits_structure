#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(lavaan)
library(tidyverse)
#run on computing cluster - example script for multiple regression outcome models. Run for each outcome, no constrianing of factor effects to be equal. Run for comparison of r2 to higherorder model
outcome_name <- args[1]
data_file_path <- args[2]
output_filepath <- args[3]
categorical_out <- args[4]
return_fit <- args[5]
jobname <- args[6]

modelRun_cor <- function(outcome_name, 
                         data_file_path, 
                         output_filepath, 
                         categorical_out = TRUE,
                         return_fit = FALSE,
                         jobname) { 
  model <- paste0('
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



#Regression
', outcome_name, '~ motor + c(lang.1,lang.2)*language + prosocial + c(nvc.1, nvc.2)*nvcc + c(rb.1,rb.2)*rrb + c(soc_rep.1, soc_rep.2)*social_recept_int + c(ain.1, ain.2)*adhd + c(isp.1,isp.2)*idio_speech + c(pl.1,pl.2)*play + c(hyp.1,hyp.2)*wait + c(aothr.1,aothr.2)*impulse



#group 2
#lang.2  < 0.4
#pl.2 < 0.4
#soc_rep.2 < 0.4
#nvc.2 < 0.4
#rb.2 < 0.4
#isp.2 < 0.4
#lang.2  > -0.4
#pl.2 > -0.4
#soc_rep.2 > -0.4
#nvc.2 > -0.4
#rb.2 > -0.4
#isp.2 > -0.4

                  ')
 
  data <- readRDS(data_file_path)
  
  if(categorical_out == TRUE){ 
    fit<- lavaan::cfa(model,
                     data = data,
                     estimator = "WLSMV",
                     missing = "pairwise",
                     std.lv = TRUE,
                     group = "sex",
                     ordered = c(grep("GG",colnames(data), value = TRUE), outcome_name)) 
  }
  
  if(categorical_out == FALSE){ 
    fit<- lavaan::cfa(model,
                         data = data,
                         estimator = "WLSMV",
                         missing = "pairwise",
                         std.lv = TRUE,
                         group = "sex",
                      ordered = c(grep("GG",colnames(data), value = TRUE))) 
  }
  
  
  
  grp_1 <- strsplit(fit@Data@group.label[1],"")[[1]][[1]]
  grp_2 <- strsplit(fit@Data@group.label[2],"")[[1]][[1]]
  
  model_est <- lavaan::parameterEstimates(fit, standardized = TRUE)
  model_std <- standardizedsolution(fit) %>% filter (op == "~" )
  
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
  
  fit.measures <- lavaan::fitmeasures(fit)
  
  output <- list(converged = fit@Fit@converged, 
				 nobs = fit@Data@nobs,
                 r2.Female = lavInspect(fit, what = "r2")$Female[outcome_name], 
                 r2.Male = lavInspect(fit, what = "r2")$Male[outcome_name], 
				 lambda.Female = lavInspect(fit, what = "std")$Female["lambda"], 
                 lambda.Male = lavInspect(fit, what = "std")$Male["lambda"],
				 tau.Female = lavInspect(fit, what = "std")$Female["tau"],
				 tau.Male = lavInspect(fit, what = "std")$Male["tau"],
				 psi.Female = lavInspect(fit, what = "std")$Female["psi"],
				 psi.Male = lavInspect(fit, what = "std")$Male["psi"],
                 regression.estimates = regression.estimates, 
                 fit.measures = fit.measures, 
                 heywood.cases = heywood)
  
  saveRDS(output, paste0(output_filepath,"/",jobname,"_cor.rds"))
  
  if(return_fit == TRUE) {saveRDS(fit, paste0(output_filepath,"/",jobname,"_cor_fitobj.rds"))}
  
  }


obj <- modelRun_cor(outcome_name, 
             data_file_path, 
             output_filepath, 
             categorical_out,
	     return_fit,
	     jobname)
