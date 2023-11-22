library(genotools)
library(tidyverse)
library(psych)

thresholds <- c(5e-08,1e-07,1e-06,1e-05,1e-04,.001,0.01,0.05,0.1,0.5,1)
prs98k <- available_pgs(geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc")

scores <- c("adhd", "asd", "scz2022")
filter_98k <- prs98k %>% 
  filter(Pheno_shortname %in% scores) %>%
  filter(maf == "maf0.01")

pgs_data <- fetch_pgs(scores,
                      geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
                      thresholds = thresholds,
                      maf = "0.01",
                      clump = "250_1_0.1")


pgs_procd <- process_pgs(pgs_data,
                         geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
                         regress_pgs = TRUE, #regress pc onto score
                         return_covs = FALSE)



pgs_pc <- pgs_procd %>% 
  select(IID,FID, matches("res"), c("PREG_ID_2306","F_ID_2306","M_ID_2306","Role", "BARN_NR"))


pgs_pc <- pgs_pca(dat= pgs_pc,
                  indid = "IID",
                  pgs_var_stem = c("adhd","asd", "scz2022"),
                  return = "pcs") 

pgs_procd_final <- pgs_pc %>% 
  select(ends_with("pc"), c("IID","FID","PREG_ID_2306","Role", "BARN_NR")) %>%
  filter(Role == "Child") %>%
  rename("preg_id" = "PREG_ID_2306") %>%
  select(-Role)

dat_pheno <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds")

dat_pgs <-merge.data.frame(dat_pheno, pgs_procd_final, by = c("preg_id", "BARN_NR"), all.x = TRUE)

dat_pgs %>%
  select(ends_with("pc")) %>%
  filter(!is.na(adhd.pgs.pc)) %>%
  nrow()
 

saveRDS(dat_pgs, "N:/durable/projects/neurodev_structure/data/dat_pgs.rds")
