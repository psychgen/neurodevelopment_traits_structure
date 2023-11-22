#descriptives for items for factor analysis and item-level GWAS
library(tidyverse)
source("N:/durable/projects/neurodev_structure/scripts/scripts_final/rename_items.R")

#data
dat_pheno_split <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds")%>% 
  filter(!is.na(preg_id),
         !is.na(BARN_NR)) %>%
  mutate(ID_2306 = paste(preg_id, BARN_NR, sep ="_")) %>%
  as.data.frame(.)

used <- c("GG222","GG223","GG224","GG225","GG237","GG238","GG239","GG240","GG241","GG242","GG256","GG257","GG231","GG232","GG233","GG234","GG235","GG236","GG227","GG228","GG229","GG230",
          "GG244","GG245","GG246","GG248","GG253","GG264","GG275","GG276","GG277","GG279","GG280","GG282","GG283","GG284","GG285","GG286","GG287","GG251","GG263","GG265","GG266","GG267","GG268","GG269","GG270","GG272","GG273",
          "GG243","GG247","GG249","GG250","GG281","GG290","GG291","GG292","GG314","GG332","GG315","GG341","GG345","GG258","GG260","GG261","GG262","GG288","GG289","GG293","GG294","GG316","GG320","GG340","GG327","GG339","GG342") 

n_lvl1 <- function(x){
  y <- as.data.frame(table(x))
  y[[2]][1]
} # function to get number of response categories 

sum_1p <- dat_pheno_split %>%
  select(starts_with("GG")) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(!is.na(.)),
    ~ round(sum(is.na(.))/(n()),3),
    ~ table(.)[1],
    ~ table(.)[2],
    ~ table(.)[3],
    ~ length(table(.))
  ))

#by sex 

sum_1p_sex <- dat_pheno_split %>%
  select(starts_with("GG"), sex) %>%
  filter(!is.na(sex))%>%
  group_by(sex) %>%
  summarise_at(vars(1:(length(.) -1)),list(
    ~ sum(!is.na(.)),
    ~ round(sum(is.na(.))/(n()),3),
    ~ table(.)[1],
    ~ table(.)[2],
    ~ table(.)[3]
  )) %>%
  gather(key, value, -sex) %>%
  separate(key, c("Item", "Function"), sep = "_") %>%
  spread("Function", value)  %>%
  rename(.,
         n = sum,
         Percent_missing = "round", 
         n_1st_lvl = "[..3", 
         n_2nd_lvl = "[..4", 
         n_3rd_lvl = "[..5") %>%
  pivot_wider(
    id_cols = Item,
    names_from = sex,
    values_from = c(n_1st_lvl, n_2nd_lvl,n_3rd_lvl,Percent_missing,n),
    names_sep = "_")

sum_p <- sum_1p %>%
  gather(key, value) %>%
  separate(key, c("item", "Function"), sep = "_") %>%
  spread("Function", value) %>%
  rename(.,
         n = sum,
         Percent_missing = "round", 
         n_1st_lvl_all = "[..3", 
         n_2nd_lv_all = "[..4", 
         n_3rd_lvl_all = "[..5", 
         Number_of_lvl = "length") %>%
  mutate(Item = item) %>%
  rename_items() %>%
  rename(Item_short_name = "item") %>%
  mutate(scale = case_when(Item %in% ASQ ~ "ASQ",
                                   Item %in% CBCL ~ "CBCL",
                                   Item %in% CBM ~ "CBM",
                                   Item %in% ESAT ~ "ESAT",
                                   Item %in% MCHAT ~ "MCHAT",
                                   Item %in% MOBA ~ "MOBA",
                                   Item %in% NVCC ~ "NVCC",
                                   Item %in% SCQ ~ "SCQ",
                                   Item %in% SDQ ~ "SDQ"), 
         In_final_model = if_else(Item %in% used, "yes", "no"))
#inter-item correlation

corObj_all <- dat_pheno_split %>% #Matrix was not positive definite, smoothing was done
  select(starts_with("GG")) %>%
  psych::polychoric(.)
corMat_all <- as.data.frame(corObj_all$rho) %>%
  naniar::replace_with_na_all(condition = ~.x == 1)

interItem_all <- corMat_all %>%
  rowMeans(., na.rm = TRUE) %>%
  as.data.frame() %>%
  mutate(Item = colnames(corMat_all))%>%
  rename(inter_item_cor = ".") 

#Items avg inter item correlation less than 0 - GG271 (complicated movements of their whole body), GG259 (socially inappropriate questions or statements)

sum <- merge(interItem_all,sum_p, by = "Item") %>%
  merge(.,sum_1p_sex, by = "Item") %>%
  relocate(Item_short_name, .after = "Item")%>%
  relocate(scale, .after = "Item") 

openxlsx::write.xlsx(sum, "N:/durable/projects/neurodev_structure/results/results_final/sum_item_data.xlsx", overwrite = TRUE)




