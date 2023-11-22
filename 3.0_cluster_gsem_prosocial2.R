library(tzdb, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
library(GenomicSEM, lib = "//cluster/p/p471/cluster/projects/differentiation_genomics/packages")
args = commandArgs(trailingOnly=TRUE)
library(tidyverse)

version <- args[1]

print(paste0("running version: ", version))

pro_sum <- readRDS(file=paste0("sum_prosocial",version,".rds"))

cov_pro <- readRDS(file=paste0("cov_prosocial",version,".rds"))

proGWAS <- commonfactorGWAS(covstruc=cov_pro, SNPs=pro_sum, estimation="DWLS", smooth_check=TRUE, parallel=TRUE, cores = 20)

saveRDS(proGWAS, file=paste0("pro_GWAS_v",version,".rds"))