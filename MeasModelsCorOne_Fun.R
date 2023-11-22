#!/usr/bin/env Rscript

#run on computing cluster - example script for univariate outcome models. run for each factor/outcome pair 
args = commandArgs(trailingOnly=TRUE)

library(lavaan)
library(tidyverse)

outcome_name <- args[1] #outcome
domain_name <- args[2] #factor
data_file_path <- args[3]
output_filepath <- args[4]
categorical_out <- args[5]
return_fit <- args[6]


modelRun_cor <- function(outcome_name,
                         domain_name,
                         data_file_path, 
                         output_filepath, 
                         categorical_out = TRUE,
                         return_fit = FALSE) { 
  model <- paste0('
motor =~ c(l5.1, l5.2)*GG222 + GG223 + GG224 + GG225
language =~ GG237 + GG238 + GG239 + GG240 + GG241 + GG242 + GG256 + GG257
prosocial =~ GG231 + GG232 + GG233 + GG234 + GG235 + GG236
nvcc =~ c(l3.1, l3.2)*GG227 + GG228 + GG229 + GG230 + c(l1.1, l1.2)*GG244 + GG245 + GG246 + GG248 + GG253 + GG264 + GG275 + GG276 + GG277 + GG279 + GG280 + GG282 + GG283 + GG284 + GG285 + GG286 + c(l2.1,l2.2)*GG287
rrb =~ GG251 + GG263 + GG265 + GG266 + GG267 + GG268 + GG269 + GG270 + GG272 + GG273
social_recept_int =~ GG243 + c(l4.1, l4.2)*GG247 + GG249 + GG250 + GG281 + GG290 + GG291 + GG292
adhd_in =~ GG314 + GG332 + GG315 + GG341 + GG345
idio_speech =~ GG258 + GG260 + GG261 + GG262
play =~  GG288 + GG289 + GG293 + GG294
adhd_hyp=~ GG316 + GG320 + GG340
adhd_other=~ GG327 + GG339 + GG342



#Regression
', outcome_name, '~', domain_name, '  
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
  
  saveRDS(output, paste0(output_filepath,"/",outcome_name,"_", domain_name, "_cor.rds"))
  
  if(return_fit == TRUE) {saveRDS(fit, paste0(output_filepath,"/",outcome_name,"_fitobj_cor.rds"))}
  
  }


obj <- modelRun_cor(outcome_name, 
			 domain_name,
             data_file_path, 
             output_filepath, 
             categorical_out,
	     return_fit)
