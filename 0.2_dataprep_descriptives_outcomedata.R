library(phenotools)
library(tidyverse)
library(foreign)

npr_diag <- c("F7","F70", "F71","F72","F73", "F78","F79", 
              "F80", "F800", "F801","F802","F803", "F808","F809", 
              "F81", "F810", "F811","F812","F813", "F818","F819", 
              "F82", "F83", 
              "F84", "F840", "F841","F842", "F843","F844","F845","F848","F849",
              "F90", "F95", "F984", "F985", "F986") #NPR ND condition

npr <- curate_npr(npr_diag, recursive = FALSE,
                  linkage_file_root_dir = "//ess01/P471/data/durable/data/Linkage_files/")




preg_id_3 <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds") %>%  .$preg_id #Ids in age 3 data

sex <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_MBRN_541_v12.sav", to.data.frame = TRUE) %>%
  select(PREG_ID_2306,BARN_NR,sex = KJONN) %>%
  rename(preg_id = PREG_ID_2306)
sex$preg_id <- as.character(sex$preg_id)


npr_select <- npr %>%
  select(contains(c("received_dx_F", "childageyrs_first", "n_hosp", "received_dx_2x_F")), 1:5)


hosp_col <- npr_select %>%
  select(contains(c("n_hosp"))) %>% 
  colnames()

#coding vars for combined categories all will be used as outcomes
outcome_diagnosis.1 <- npr_select %>% 
  mutate(ever_inpatient = apply(.[hosp_col] > 0 ,1, any), 
         received_dx_f82_f984_npr = if_else(received_dx_F82_npr == "yes" | received_dx_F984_npr == "yes", "yes","no"), 
         received_dx_f80_f985_f986_npr = if_else(received_dx_F80_npr == "yes" | received_dx_F985_npr == "yes" | received_dx_F985_npr == "yes", "yes", "no"),
         received_dx_f7_f83_npr = if_else(received_dx_F7_npr == "yes" | received_dx_F83_npr == "yes", "yes", "no"),
         received_2dx_f82_f984_npr = if_else(received_dx_2x_F82_npr == "yes" | received_dx_2x_F984_npr == "yes", "yes", "no"), 
         received_2dx_f80_f985_f986_npr = if_else(received_dx_2x_F80_npr == "yes" | received_dx_2x_F985_npr == "yes" | received_dx_2x_F985_npr == "yes", "yes", "no"),
         received_2dx_f7_f83_npr = if_else(received_dx_2x_F7_npr == "yes" | received_dx_2x_F83_npr == "yes", "yes", "no")
  )

npr_col_2x <- c( "received_2dx_f7_f83_npr", "received_2dx_f80_f985_f986_npr",  "received_dx_2x_F81_npr", "received_dx_2x_F82_npr",  
                "received_dx_2x_F83_npr",  "received_dx_2x_F84_npr",  "received_dx_2x_F90_npr",  "received_dx_2x_F95_npr")


npr_col <- c( "received_dx_f7_f83_npr", "received_dx_f80_f985_f986_npr", "received_dx_F81_npr", "received_dx_F82_npr",  
             "received_dx_F83_npr",  "received_dx_F84_npr",  "received_dx_F90_npr",  "received_dx_F95_npr") #vars used as outcomes´(minus F84 which will be coded later for autism)

npr_total_2x <- c("received_dx_2x_F7_npr",  "received_2dx_f80_f985_f986_npr",  "received_dx_2x_F81_npr", 
                   "received_2dx_f82_f984_npr", "received_dx_2x_F83_npr",  
                  "received_dx_2x_F84_npr",  "received_dx_2x_F90_npr",  "received_dx_2x_F95_npr")


npr_total <- c("received_dx_F7_npr", "received_dx_f80_f985_f986_npr",  "received_dx_F81_npr", "received_dx_f82_f984_npr", "received_dx_F83_npr",  
               "received_dx_F84_npr",  "received_dx_F90_npr",  "received_dx_F95_npr") # NPR dx counted towards total, use of combined cat to avoid double counting similar codes a

outcome_diagnosis <- outcome_diagnosis.1 %>%
  mutate(any_dx_npr = apply(.[npr_col] == "yes",1, any), #var for any ND diagnosis
         any_2dx_npr = apply(.[npr_col_2x] == "yes",1, any),
         no_dx_npr = ifelse(any_dx_npr == FALSE, 1, 0), #var for no ND diagnosis
         total_dx_npr = rowSums(.[npr_total] == "yes", na.rm = TRUE), #var for total number of ND dx
         total_2dx_npr = rowSums(.[npr_total_2x] == "yes", na.rm = TRUE),
         multi_dx_npr = case_when(total_dx_npr == 0 ~ "none", #ordinal var for total dx to use as outcome
                                  total_dx_npr == 1 ~ "single", 
                                  total_dx_npr == 2 ~ "two", 
                                  total_dx_npr >= 3 ~ "three+"),
         multi_2dx_npr = case_when(total_2dx_npr == 0 ~ "none",
                                   total_2dx_npr == 1 ~ "single", 
                                   total_2dx_npr == 2 ~ "two", 
                                   total_2dx_npr >= 3 ~ "three+"),
         received_dx_f840_f841_f845_f848_f849_npr = if_else(received_dx_F840_npr == "yes" | received_dx_F841_npr == "yes" | received_dx_F845_npr == "yes" | received_dx_F848_npr == "yes" | received_dx_F849_npr == "yes", 1, 0, missing = 0), #coding autism
         received_dx_f840_f841_f845_f848_f849_npr = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F842_npr == "no" & received_dx_F843_npr == "no" & received_dx_F844_npr == "no", 1,0, missing = 0),
         received_2dx_f840_f841_f845_f848_f849_npr = if_else(received_dx_2x_F840_npr == "yes" | received_dx_2x_F841_npr == "yes" | received_dx_2x_F845_npr == "yes" | received_dx_2x_F848_npr == "yes" | received_dx_2x_F849_npr == "yes", 1, 0, missing = 0),
         received_2dx_f840_f841_f845_f848_f849_npr = if_else(received_2dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F842_npr == "no" & received_dx_F843_npr == "no" & received_dx_F844_npr == "no", 1,0, missing = 0),
         received_dx_aut_noID_npr = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F7_npr == "no", 1,0, missing = 0),
         received_dx_aut_wID_npr = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F7_npr == "yes",1,0, missing = 0),
         received_dx_aut_noADHD_npr = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F90_npr == "no", 1,0, missing = 0),
         received_dx_aut_wADHD_npr = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_F90_npr == "yes",1,0, missing = 0),
         received_dx_adhd_noAut_npr = if_else(received_dx_F90_npr == "yes" & received_dx_f840_f841_f845_f848_f849_npr == 0,1,0,missing = 0),
         received_2dx_aut_noID_npr = if_else(received_2dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_2x_F7_npr == "no", 1,0, missing = 0),
         received_2dx_aut_wID_npr = if_else(received_2dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_2x_F7_npr == "yes",1,0, missing = 0),
         received_2dx_aut_noADHD_npr = if_else(received_2dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_2x_F90_npr == "no", 1,0, missing = 0),
         received_2dx_aut_wADHD_npr = if_else(received_2dx_f840_f841_f845_f848_f849_npr == 1 & received_dx_2x_F90_npr == "yes",1,0, missing = 0),
         received_2dx_adhd_noAut_npr = if_else(received_dx_2x_F90_npr == "yes" & received_2dx_f840_f841_f845_f848_f849_npr == 0,1,0,missing = 0),
         age_autism = if_else(received_dx_f840_f841_f845_f848_f849_npr == 1 , childageyrs_first_dx_F84_npr, NA_real_),
         age_aut_noID = if_else(received_dx_aut_noID_npr == 1 , childageyrs_first_dx_F84_npr, NA_real_), 
         age_autism_wID = if_else(received_dx_aut_wID_npr == 1 , childageyrs_first_dx_F84_npr, NA_real_),
         age_autism_noADHD = if_else(received_dx_aut_noADHD_npr == 1 , childageyrs_first_dx_F84_npr, NA_real_),
         age_autism_wADHD = if_else(received_dx_aut_wADHD_npr == 1 , childageyrs_first_dx_F84_npr, NA_real_),
         age_f7_f83 = case_when(received_dx_f7_f83_npr == "yes" & received_dx_F7_npr == "yes" & received_dx_F83_npr == "no" ~ childageyrs_first_dx_F7_npr, 
                                received_dx_f7_f83_npr == "yes" & received_dx_F7_npr == "no" & received_dx_F83_npr == "yes" ~ childageyrs_first_dx_F83_npr, 
                                received_dx_f7_f83_npr == "yes" & received_dx_F7_npr == "yes" & received_dx_F83_npr == "yes" & childageyrs_first_dx_F83_npr > childageyrs_first_dx_F7_npr ~ childageyrs_first_dx_F7_npr,
                                received_dx_f7_f83_npr == "yes" & received_dx_F7_npr == "yes" & received_dx_F83_npr == "yes" & childageyrs_first_dx_F7_npr > childageyrs_first_dx_F83_npr ~ childageyrs_first_dx_F83_npr,
                                TRUE ~ NA_real_), 
         age_f80_f895_f896 = case_when(received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "yes" & received_dx_F985_npr == "no" & received_dx_F986_npr == "no" ~ childageyrs_first_dx_F80_npr, 
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "no" & received_dx_F985_npr == "yes" & received_dx_F986_npr == "no" ~ childageyrs_first_dx_F985_npr, 
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "no" & received_dx_F985_npr == "no" & received_dx_F986_npr == "yes" ~ childageyrs_first_dx_F986_npr,
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "yes" & received_dx_F985_npr == "yes" & childageyrs_first_dx_F80_npr > childageyrs_first_dx_F985_npr  ~ childageyrs_first_dx_F985_npr,
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "yes" & received_dx_F985_npr == "yes" & childageyrs_first_dx_F80_npr < childageyrs_first_dx_F985_npr  ~ childageyrs_first_dx_F80_npr,
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "yes" & received_dx_F986_npr == "yes" & childageyrs_first_dx_F80_npr > childageyrs_first_dx_F986_npr  ~ childageyrs_first_dx_F986_npr,
                                       received_dx_f80_f985_f986_npr == "yes" & received_dx_F80_npr == "yes" & received_dx_F986_npr == "yes" & childageyrs_first_dx_F80_npr < childageyrs_first_dx_F986_npr  ~ childageyrs_first_dx_F986_npr,
                                       TRUE ~ NA_real_), 
         age_ADHD_noaut = if_else(received_dx_adhd_noAut_npr == 1 , childageyrs_first_dx_F90_npr, NA_real_), 
         age_F81 = childageyrs_first_dx_F81_npr, 
         age_F82 = childageyrs_first_dx_F82_npr, 
         age_F83 = childageyrs_first_dx_F83_npr, 
         age_F90 = childageyrs_first_dx_F90_npr, 
         age_F95 = childageyrs_first_dx_F95_npr) %>% 
  select(-c(hosp_col))


out_dat <- outcome_diagnosis %>% #creating outcome data set
  mutate(across(.cols = c(npr_col, npr_col_2x), ~case_when(. == "yes" ~ 1,
                                                           . == "no" ~ 0))) %>%
  mutate(across(.cols = c("ever_inpatient", "any_dx_npr", "any_2dx_npr"), ~if_else(. == TRUE, 1,0, missing = NA_real_))) %>%
  select(c("preg_id", "BARN_NR", "m_id", "f_id", "birth_yr", 
           received_dx_f840_f841_f845_f848_f849_npr,received_2dx_f840_f841_f845_f848_f849_npr, 
           no_dx_npr,
           contains("_aut_"), contains("_adhd_"),
           contains("any"),contains("multi"),contains("ever"), contains("total"),contains("age_"), npr_col, npr_col_2x)) %>%
  left_join(sex, by = c("preg_id","BARN_NR"))




##ever inpatient psychiatric conditions
#adding ever inpatient psychiatric conditions

nprdx_psychiatric = c("F1", "F2", "F3", "F4","F50", "F51", "F55", "F60", "F61", "F62", "F63", "F91", "F92", "F93", "F94")

psych_dx <- curate_npr(nprdx_psychiatric, group_all = TRUE,
                       linkage_file_root_dir = "//ess01/P471/data/durable/data/Linkage_files/")

hosp_col <- psych_dx %>%
  select(contains(c("n_hosp"))) %>% 
  colnames()

outcome_psych_hosp <- psych_dx %>% 
  rename("n_hosp"= !!hosp_col) %>%
  mutate(ever_inpatient_psych = if_else(n_hosp > 0 ,1, 0, missing = 0)) %>%
  select(c(preg_id, ever_inpatient_psych, BARN_NR, m_id))


out_dat2 <-merge(out_dat, outcome_psych_hosp, by = c("preg_id","BARN_NR","m_id")) %>%
  rename(ever_inpatient_nd = ever_inpatient) %>%
  mutate(ever_inpatient = if_else(ever_inpatient_nd == 1 | ever_inpatient_psych == 1, 1, 0, missing = 0)) #combining ever hospitalized  with ND code and Psych code - outcome for analyses

saveRDS(out_dat2, "data/outcomes_npr_updatedNPR_withage.rds") #save data

outcome_diagnosis <- outcome_diagnosis %>%
  mutate(across(.cols = c( "received_dx_f840_f841_f845_f848_f849_npr" , "received_2dx_f840_f841_f845_f848_f849_npr" , "received_dx_aut_noID_npr" , 
                           "received_dx_aut_wID_npr" , "received_dx_aut_noADHD_npr" , "received_dx_aut_wADHD_npr" , "received_2dx_aut_noID_npr" , "received_2dx_aut_wID_npr" , "received_2dx_aut_noADHD_npr" , 
                           "received_2dx_aut_wADHD_npr" , "received_dx_adhd_noAut_npr" , "received_2dx_adhd_noAut_npr", "no_dx_npr"), ~if_else(. == 0,NA_real_, 1)))

##summary tables for NPR outcomes
dat_sum1 <- out_dat2  %>%
  select(starts_with(c("receive", "any", "ever", "no")),ever_inpatient) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == 1, na.rm = TRUE)
  )) %>%
  gather(diagnosis_var, n_MoBa)


dat_sum2 <- out_dat2 %>%
  select(contains("multi")) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == "two", na.rm = TRUE),
    ~ sum(. == "single", na.rm = TRUE),
    ~ sum(. == "three+", na.rm = TRUE))) %>%
  rename(two_2dx_npr = multi_2dx_npr_sum..1,
         single_2dx_npr = multi_2dx_npr_sum..2,
         threeplus_2dx_npr = multi_2dx_npr_sum..3,
         two_dx_npr = multi_dx_npr_sum..1,
         single_dx_npr = multi_dx_npr_sum..2,
         threeplus_dx_npr = multi_dx_npr_sum..3,
  ) %>%
  gather(diagnosis_var, n_MoBa)

dat_sum1_3 <- out_dat2 %>%
  filter(preg_id %in% preg_id_3) %>% 
  select(starts_with(c("receive", "any", "ever", "no")),ever_inpatient)  %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == 1, na.rm = TRUE)
  )) %>%
  gather(diagnosis_var, n_Q3)

dat_sum2_3 <- outcome_diagnosis %>%
  filter(preg_id %in% preg_id_3) %>%
  select(contains("multi")) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == "two", na.rm = TRUE),
    ~ sum(. == "single", na.rm = TRUE),
    ~ sum(. == "three+", na.rm = TRUE))) %>%
  rename(two_2dx_npr = multi_2dx_npr_sum..1,
         single_2dx_npr = multi_2dx_npr_sum..2,
         threeplus_2dx_npr = multi_2dx_npr_sum..3,
         two_dx_npr = multi_dx_npr_sum..1,
         single_dx_npr = multi_dx_npr_sum..2,
         threeplus_dx_npr = multi_dx_npr_sum..3)  %>%
  gather(diagnosis_var, n_Q3) %>%
  rbind(., dat_sum1_3)

dat_sum1_3_sex <- out_dat2 %>%
  filter(preg_id %in% preg_id_3) %>%
  group_by(sex) %>%
  select(starts_with(c("receive", "any", "ever", "no")),ever_inpatient)  %>%
  summarise_at(vars(1:length(.)-1),list(
    ~ sum(. == 1, na.rm = TRUE)
  )) %>%
  gather(diagnosis_var, n_Q3, -sex) %>%
  filter(sex == "Female" | sex == "Male") %>%
  spread(sex, n_Q3) %>%
  rename("N_Male_q3" = "Male",
         "N_Female_q3" = "Female")

dat_sum2_3_sex <- out_dat2 %>%
  filter(preg_id %in% preg_id_3) %>%
  group_by(sex) %>%
  select(contains(c("multi", "sex"))) %>%
  summarise_at(vars(1:length(.)-1),list(
    ~ sum(. == "two", na.rm = TRUE),
    ~ sum(. == "single", na.rm = TRUE),
    ~ sum(. == "three+", na.rm = TRUE))) %>%
  rename(two_2dx_npr = multi_2dx_npr_sum..1,
         single_2dx_npr = multi_2dx_npr_sum..2,
         threeplus_2dx_npr = multi_2dx_npr_sum..3,
         two_dx_npr = multi_dx_npr_sum..1,
         single_dx_npr = multi_dx_npr_sum..2,
         threeplus_dx_npr = multi_dx_npr_sum..3)  %>%
  gather(diagnosis_var, n_Q3, -sex) %>%
  filter(sex == "Female" | sex == "Male") %>%
  spread(sex, n_Q3) %>%
  rename("N_Male_q3" = "Male",
         "N_Female_q3" = "Female") %>%
  rbind(., dat_sum1_3_sex)

dat_sum1_m <- out_dat2 %>%
  filter(multi_2dx_npr == "two" |
           multi_2dx_npr == "three+") %>%
  select(starts_with(c("receive", "any", "ever", "no"))) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == 1, na.rm = TRUE)
  )) %>%
  gather(diagnosis_var, n_multi_2x)

dat_sum2_m <- out_dat2 %>%
  filter(multi_2dx_npr == "two" |
           multi_2dx_npr == "three+") %>%
  select(contains("multi")) %>%
  summarise_at(vars(1:length(.)),list(
    ~ sum(. == "two", na.rm = TRUE),
    ~ sum(. == "single", na.rm = TRUE),
    ~ sum(. == "three+", na.rm = TRUE))) %>%
  rename(two_2dx_npr = multi_2dx_npr_sum..1,
         single_2dx_npr = multi_2dx_npr_sum..2,
         threeplus_2dx_npr = multi_2dx_npr_sum..3,
         two_dx_npr = multi_dx_npr_sum..1,
         single_dx_npr = multi_dx_npr_sum..2,
         threeplus_dx_npr = multi_dx_npr_sum..3) %>%
  gather(diagnosis_var, n_multi_2x) %>%
  rbind(., dat_sum1_m)



dat_sumage2008later_3 <- outcome_diagnosis %>%  
  filter(preg_id %in% preg_id_3) %>%
  filter(birth_yr >= 2008) %>%
  select(contains("age_")) %>%
  summarise_at(vars(1:length(.)),list(
    ~mean(. , na.rm = TRUE))) %>% 
  gather(diagnosis_var) %>%
  rename(mean_age_firstdx_birthyr2008andlater = value) 

dat_2005later_percentdx_3andunder <- outcome_diagnosis %>%  
  filter(preg_id %in% preg_id_3) %>%
  filter(birth_yr >= 2005) %>%
  select(contains("age_")) %>%
  summarise_at(vars(1:length(.)),list( 
    ~sum(. <= 3, na.rm = TRUE) / sum(!is.na(.)))) %>% 
  gather(diagnosis_var) %>%
  rename(percentdx_atorbefore3_birthyr2005andlater= value)

#need to run questionnaire data section below for this!  
percent_early.refer_andDX <- merge.data.frame(outcome_diagnosis, outcome_questionr, by = c("preg_id", "BARN_NR", "m_id")) %>% 
  filter(preg_id %in% preg_id_3) %>%
  filter(birth_yr < 2005) %>%
  select(contains("age_"), early.service.refer) %>%
  summarise_at(vars(1:(length(.) -1)),list( 
    ~sum(!is.na(.) & early.service.refer == 1, na.rm = TRUE) / sum(!is.na(.)))) %>% 
  gather(diagnosis_var) %>%
  rename(percent_earlyrefer_birthyr_before2005 = value)
#done with section that needs questionnaire data
#% of sample born 2008 (when NPR data is available) and later 
sum(out_dat4$birth_yr >= 2008) / nrow(out_dat4)
#% of sample born 2005 and later (have NPR data available for age 3) 
sum(out_dat4$birth_yr >= 2005) / nrow(out_dat4)

sum_age <- dat_sumage2008later_3 %>%
  merge(.,dat_2005later_percentdx_3andunder, by = c("diagnosis_var")) %>%
  merge(.,percent_early.refer_andDX , by = c("diagnosis_var")) %>%
  mutate(diagnosis_var = case_when(diagnosis_var == "age_autism" ~ "received_dx_f840_f841_f845_f848_f849_npr",
                                   diagnosis_var == "age_aut_noID" ~ "received_dx_aut_noID_npr",
                                   diagnosis_var == "age_autism_wID" ~ "received_dx_aut_wID_npr",
                                   diagnosis_var == "age_autism_noADHD" ~ "received_dx_aut_noADHD_npr",
                                   diagnosis_var == "age_autism_wADHD" ~ "received_dx_aut_wADHD_npr",
                                   diagnosis_var == "age_f7_f83" ~ "received_dx_f7_f83_npr" ,
                                   diagnosis_var == "age_f80_f895_f896" ~ "received_dx_f80_f985_f986_npr",
                                   diagnosis_var == "age_ADHD_noaut" ~ "received_dx_adhd_noAut_npr",
                                   diagnosis_var == "age_F81" ~ "received_dx_F81_npr",
                                   diagnosis_var == "age_F82" ~ "received_dx_F82_npr",
                                   diagnosis_var == "age_F83" ~ "received_dx_F83_npr",
                                   diagnosis_var == "age_F90" ~ "received_dx_F90_npr",
                                   diagnosis_var == "age_F95" ~ "received_dx_F95_npr"))
  
outcomes_f <- c("preg_id" , "BARN_NR" , "m_id" , "f_id" , "birth_yr" , "received_dx_f840_f841_f845_f848_f849_npr" ,  #final outcomes to be in sum table 
                "received_dx_aut_noID_npr" , "received_dx_aut_wID_npr" , "received_dx_aut_noADHD_npr" , "received_dx_aut_wADHD_npr" , 
                "received_dx_adhd_noAut_npr" ,  "any_dx_npr" ,  "multi_dx_npr" , "no_dx_npr", 
                "total_dx_npr" ,  "received_dx_F7_npr" , "received_dx_f7_f83_npr" , "received_dx_f80_f985_f986_npr" ,
                "received_dx_F81_npr" , "received_dx_F82_npr" ,  "received_dx_F83_npr" ,  "received_dx_F90_npr" , "received_dx_F95_npr", "ever_inpatient")

outcome_sum <- dat_sum1 %>%
  rbind(.,dat_sum2) %>%
  merge(dat_sum2_3, by = c("diagnosis_var")) %>%
  merge(dat_sum2_m, by = c("diagnosis_var")) %>%
  merge(dat_sum2_3_sex, by = c("diagnosis_var")) %>%
  merge(sum_age, by = c("diagnosis_var")) %>%
  mutate(percent_multi_2dx_MoBa =  n_multi_2x / n_MoBa ,
         attrition_rate_age3 = 1 - (n_Q3 / n_MoBa),
         sex_ratio_Q3 = paste0("1:", round((N_Male_q3/N_Female_q3),2)),
         diagnosis = case_when(diagnosis_var == "any_dx_both"	~	"Any dx code",
                               diagnosis_var == "any_dx_either"	~	"Any dx code",
                               diagnosis_var == "any_dx_kuhr"	~	"Any dx code",
                               diagnosis_var == "any_dx_npr"	~	"Any dx code",
                               diagnosis_var ==  "multi_dx_either"	~	"dx codes in multiple diagnositic domains",
                               diagnosis_var ==  "multi_2dx_either"	~	"2 dx codes for same diagnosis in multiple diagnositic domains",
                               grepl("no_dx", diagnosis_var) ~ "no dx codes available",
                               diagnosis_var == "received_dx_F7_npr"	~	 "intellectual disability - Any",
                               diagnosis_var == "received_dx_f7_p85_either"	~	 "intellectual disability - Any",
                               diagnosis_var == "received_dx_F70_npr"	~	 "intellectual disability - Mild",
                               diagnosis_var == "received_dx_F71_npr"	~	"intellectual disability - Moderate",
                               diagnosis_var == "received_dx_F72_npr"	~	"intellectual disability - Severe",
                               diagnosis_var == "received_dx_F73_npr"	~	 "intellectual disability - Profound",
                               diagnosis_var == "received_dx_F78_npr"	~	 "intellectual disability - Other",
                               diagnosis_var == "received_dx_F79_npr"	~	 "intellectual disability - Unspecified",
                               diagnosis_var == "received_dx_f80_f81_f82_f83_p24_either"	~	"Specific psychological development - Any",
                               diagnosis_var == "received_dx_F80_npr"	~	"Specific psychological development - speech and language - Any",
                               diagnosis_var == "received_dx_F800_npr"	~	"Specific psychological development - speech and language - articulation",
                               diagnosis_var == "received_dx_F801_npr"	~	"Specific psychological development - speech and language - expressive",
                               diagnosis_var == "received_dx_F802_npr"	~	"Specific psychological development - speech and language - receptive",
                               diagnosis_var == "received_dx_F803_npr"	~	"Specific psychological development - speech and language -  acquired aphasia w/ epilepsy",
                               diagnosis_var == "received_dx_F808_npr"	~	"Specific psychological development - speech and language - other",
                               diagnosis_var ==  "received_dx_F809_npr"	~	"Specific psychological development - speech and language - unspecified",
                               diagnosis_var == "received_dx_F81_npr"	~	"Specific psychological development - scholastic skills - Any",
                               diagnosis_var ==  "received_dx_F810_npr"	~	"Specific psychological development - scholastic skills - reading",
                               diagnosis_var ==  "received_dx_F811_npr"	~	"Specific psychological development - scholastic skills - spelling",
                               diagnosis_var == "received_dx_F812_npr"	~	"Specific psychological development - scholastic skills - arithmatic",
                               diagnosis_var == "received_dx_F813_npr"	~	"Specific psychological development - scholastic skills - mixed",
                               diagnosis_var == "received_dx_F818_npr"	~	"Specific psychological development - scholastic skills - other",
                               diagnosis_var ==  "received_dx_F819_npr"	~	"Specific psychological development - scholastic skills - unspecified",
                               diagnosis_var == "received_dx_F82_npr"	~	"Specific psychological development - motor",
                               diagnosis_var == "received_dx_F83_npr"	~	"Specific psychological development - mixed",
                               diagnosis_var == "received_dx_F84_npr"	~	"Prevasive developmental - Any",
                               diagnosis_var == "received_dx_F840_npr"	~	"Prevasive developmental - childhood autism",
                               diagnosis_var == "received_dx_F841_npr"	~	"Prevasive developmental - Atypical autism",
                               diagnosis_var == "received_dx_F842_npr"	~	"Prevasive developmental - Rett",
                               diagnosis_var == "received_dx_F843_npr"	~	"Prevasive developmental - other disintegrative",
                               diagnosis_var == "received_dx_F844_npr"	~	"Prevasive developmental - overactive w/ ID and sterotyped movements",
                               diagnosis_var == "received_dx_F845_npr"	~	"Prevasive developmental - Asperger",
                               diagnosis_var == "received_dx_F848_npr"	~	"Prevasive developmental - other",
                               diagnosis_var == "received_dx_F849_npr"	~	"Prevasive developmental - unspecified",
                               diagnosis_var ==  "received_dx_F90_npr"	~	"Behavioural & emotional - hyperkinetic (ADHD) - Any",
                               diagnosis_var == "received_dx_f90_p81_either"	~	"Behavioural & emotional - hyperkinetic (ADHD) - Any",
                               diagnosis_var == "received_dx_F900_npr"	~	"Behavioural & emotional - hyperkinetic (ADHD) - activity & attention",
                               diagnosis_var == "received_dx_F901_npr"	~	"Behavioural & emotional - hyperkinetic (ADHD) - hyperkinetic conduct disorder",
                               diagnosis_var == "received_dx_F908_npr"	~	"Behavioural & emotional - hyperkinetic (ADHD) - other",
                               diagnosis_var == "received_dx_F909_npr"	~	"Behavioural & emotional - hyperkinetic (ADHD) - unspecified",
                               diagnosis_var ==  "received_dx_F95_npr"	~	"Behavioural & emotional - tic conditions  - Any",
                               diagnosis_var == "received_dx_F950_npr"	~	"Behavioural & emotional - tic conditions  - transient",
                               diagnosis_var == "received_dx_F951_npr"	~	"Behavioural & emotional - tic conditions  - chronic motor or vocal",
                               diagnosis_var == "received_dx_F952_npr"	~	"Behavioural & emotional - tic conditions  - combined vocal and motor",
                               diagnosis_var == "received_dx_F958_npr"	~	"Behavioural & emotional - tic conditions  - other",
                               diagnosis_var == "received_dx_F959_npr"	~	"Behavioural & emotional - tic conditions  - unspecified",
                               diagnosis_var ==  "received_dx_F984_npr"	~	"Behavioural & emotional - other - sterotyped movement",
                               diagnosis_var ==  "received_dx_F985_npr"	~	"Behavioural & emotional - stuttering",
                               diagnosis_var ==  "received_dx_F986_npr"	~	"Behavioural & emotional - cluttering",
                               diagnosis_var == "received_dx_p10_f95_f984_f985_f986_either"	~	"Behavioural & emotional - tic & stuttering - Any",
                               diagnosis_var == "received_dx_P10_kuhr"	~	"Behavioural & emotional - Stammering/stuttering/tic",
                               diagnosis_var == "received_dx_P22_kuhr"	~	"Child behaviour symptom/complaint",
                               diagnosis_var == "received_dx_P24_kuhr"	~	"Specific psychological development - Specific learning disability",
                               diagnosis_var == "received_dx_P81_kuhr"	~	"Behavioural & emotional - Hyperkinetic disorder",
                               diagnosis_var == "received_dx_P85_kuhr"	~	"intellectual disability",
                               diagnosis_var == "received_dx_P99_kuhr"	~	"Prevasive developmental - Psychological disorders other",
                               diagnosis_var =="single_dx_either"	~	"single dx code",
                               diagnosis_var == "received_dx_f840_f841_f845_f848_f849_npr" ~ "autism - Any",
                               diagnosis_var == "received_dx_f840_f841_f845_f848_f849_p99_either"  ~ "autism - Any",
                               diagnosis_var == "received_dx_adhd_noAut_npr" ~ "ADHD w/o autism",
                               diagnosis_var == "received_dx_aut_noADHD_npr"  ~ "autism w/o ADHD",
                               diagnosis_var == "received_dx_aut_noID_npr" ~ "autism w/o intellectual disability",
                               diagnosis_var == "received_dx_aut_wADHD_npr" ~ "autism and adhd",
                               diagnosis_var == "received_dx_aut_wID_npr" ~ "autism and intellectual disability",
                               diagnosis_var == "received_dx_f80_f985_f986_npr" ~ "Specific psychological development - Speech, Language, and stuttering", 
                               diagnosis_var == "received_dx_f800_f985_f986_npr" ~ "Specific psychological development - Speech and stuttering", 
                               diagnosis_var == "received_dx_f801_f802_npr" ~ "Specific psychological development - Language",
                               diagnosis_var == "received_dx_f82_f984_npr" ~ "Specific motor and sterotyped motor",
                               diagnosis_var == "received_dx_f7_f83_npr" ~ "General Developmental Delay")) %>%
  filter(diagnosis_var %in% outcomes_f)

saveRDS(outcome_sum, "N:/durable/projects/neurodev_structure/results/sum_outcome_Updatenpr.rds")

openxlsx::write.xlsx(outcome_sum, "N:/durable/projects/neurodev_structure/results/sum_outcome_Updatenpr_withage.xlsx", SheetName = "Outcome_npr", overwrite = TRUE)



## adding to phenotypic data
pheno_dat <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds") %>%
  select(!"m_id", "f_id")

dat_outcome <- merge.data.frame(pheno_dat, out_dat2, by = c("preg_id", "BARN_NR", "sex")) 

#autism with reported conversational ability at age 3 
dat_outcome <- dat_outcome %>%
  mutate(received_dx_aut_wlang3_npr = ifelse(GG239 == 0 & received_dx_f840_f841_f845_f848_f849_npr == 1, 1, 0),
         received_2dx_aut_wlang3_npr = ifelse(GG239 == 0 & received_2dx_f840_f841_f845_f848_f849_npr == 1, 1, 0))


##questionnaire data 
#reading in data
pheno_data_root_dir <- "N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/"
q8y <- read.spss(paste0(pheno_data_root_dir, "PDB2306_Q8yrs_v12.sav"), to.data.frame = TRUE)
q5y <- read.spss(paste0(pheno_data_root_dir, "PDB2306_Q5yrs_v12.sav"), to.data.frame = TRUE)
q3y <- read.spss(paste0(pheno_data_root_dir, "PDB2306_Q6_3yrs_v12.sav"), to.data.frame = TRUE)

sv_info <-
  haven::read_spss(paste0(pheno_data_root_dir,"PDB",2306,"_SV_INFO_v12.sav")) %>%
  dplyr::select(preg_id = dplyr::matches("PREG_ID"),
                m_id = dplyr::matches("M_ID"),
                f_id = dplyr::matches("F_ID")) %>%
  dplyr::mutate(preg_id = as.integer(preg_id))


#creating dataset

q8y_merge <- q8y %>%
  rename(preg_id = PREG_ID_2306) %>%
  merge(sv_info, by = c("preg_id")) %>%
  dplyr::mutate(preg_id=as.character(preg_id),
                m_id=as.character(stringr::str_replace_all(m_id, stringr::fixed (" "), "")),
                f_id=as.character(stringr::str_replace_all(f_id, stringr::fixed (" "), ""))) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
  left_join( sex, by = c("preg_id","BARN_NR"))

q5y_merge <- q5y %>%
  rename(preg_id = PREG_ID_2306) %>%
  merge(sv_info, by = c("preg_id")) %>%
  dplyr::mutate(preg_id=as.character(preg_id),
                m_id=as.character(stringr::str_replace_all(m_id, stringr::fixed (" "), "")),
                f_id=as.character(stringr::str_replace_all(f_id, stringr::fixed (" "), ""))) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
  left_join( sex, by = c("preg_id","BARN_NR"))

q3y_merge <- q3y %>%
  rename(preg_id = PREG_ID_2306) %>%
  merge(sv_info, by = c("preg_id")) %>%
  dplyr::mutate(preg_id=as.character(preg_id),
                m_id=as.character(stringr::str_replace_all(m_id, stringr::fixed (" "), "")),
                f_id=as.character(stringr::str_replace_all(f_id, stringr::fixed (" "), ""))) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
  left_join( sex, by = c("preg_id","BARN_NR"))

q8yr_selct <- q8y_merge %>%
  select(preg_id, BARN_NR, m_id, f_id, sex, "NN242", "NN244", "NN246", "NN248", "NN250", "NN388", "NN389", "NN390", "NN391", "NN392", "NN393", "NN394", "NN395", "NN396")

preg_id_8 <-  q8yr_selct %>%
  .$preg_id
preg_id_3_8 <- preg_id_3[preg_id_3 %in% preg_id_8]

q5yr_selct <- q5y_merge %>%
  select(preg_id, BARN_NR, m_id, f_id, sex, "LL326", "LL327", "LL328", "LL329", "LL330", "LL331", "LL332", "LL333", "LL334", "LL335", "LL336") 

preg_id_5 <-  q5yr_selct %>%
  .$preg_id
preg_id_3_5 <- preg_id_3[preg_id_3 %in% preg_id_5]

q3yr_selct <- q3y_merge %>%
  select(preg_id, BARN_NR, m_id, f_id, sex,"GG173", "GG174", "GG175")  


outcome_questionr <- q8yr_selct %>%   
  merge.data.frame(., q5yr_selct, by = c("preg_id", "BARN_NR", "m_id", "f_id", "sex"), all.x = TRUE, all.y = TRUE) %>%
  merge.data.frame(., q3yr_selct, by = c("preg_id", "BARN_NR", "m_id", "f_id", "sex"), all.x = TRUE, all.y = TRUE) %>%
  mutate(across(starts_with(c("NN", "LL", "GG")), ~case_when(.x == "More than 1 check box filled in" ~ NA_character_, 
                                                                   TRUE ~ as.character(.x)))) %>%
  mutate(across(c("LL332", "LL333", "LL334", "LL335", "NN393", "NN394", "NN395", "NN396"), ~case_when(.x == "No" ~ 0,
                                                                                                      .x == "Yes, a little" ~ 1,
                                                                                                      .x == "Yes, a lot" ~ 2, 
                                                                                                      is.na(.x) ~ NA_real_))) %>%
  mutate(age8.impact = rowSums(.[c("NN393", "NN394", "NN395", "NN396")], na.rm = TRUE),
         age8.impact_n = rowSums(!is.na(.[c("NN393", "NN394", "NN395", "NN396")])),
         age8.impact = round((age8.impact / age8.impact_n)*4), 
         age8.impact = ifelse(age8.impact_n < 2, NA_real_, age8.impact),
         age5.impact = rowSums(.[c("LL332", "LL333", "LL334", "LL335")], na.rm = TRUE),
         age5.impact_n = rowSums(!is.na(.[c("LL332", "LL333", "LL334", "LL335")])),
         age5.impact = round((age5.impact / age5.impact_n)*4), 
         age5.impact = ifelse(age5.impact_n < 2, NA_real_, age5.impact),
         early.service.refer = apply(.[c("GG173", "GG174", "GG175")] == "Yes", 1, any),
         early.service.refer = case_when(early.service.refer == TRUE ~ 1,
                                         early.service.refer == FALSE ~ 0, 
                                         TRUE ~ NA_real_)) %>% 
  mutate(age8.d = if_else(NN388 == "Yes" | NN389 == "Yes" | NN390 == "Yes" | NN391 == "Yes" | NN392 == "Yes", 1, 0),
         age5.impactord = case_when((preg_id %in% preg_id_5) & is.na(age5.impact) ~ 0, #creating ordinal for use in outcome models
                                     age5.impact == 0 ~ 0, 
                                     age5.impact == 1 | age5.impact == 2 ~ 1,
                                     age5.impact == 3 | age5.impact == 4 ~ 2, 
                                     age5.impact > 4 ~ 3, 
                                     TRUE ~ NA_real_),
         age5.impact_new = case_when(LL331 == "Yes" & !is.na(age5.impact) ~ age5.impact, 
                                     TRUE ~ NA_real_),
         age8.impactord = case_when((preg_id %in% preg_id_8) & is.na(age8.impact) ~ 0,
                                     age8.impact == 0 ~ 0, 
                                     age8.impact == 1 | age8.impact == 2 ~ 1,
                                     age8.impact == 3 | age8.impact == 4 ~ 2, 
                                     age8.impact > 4 ~ 3, 
                                     TRUE ~ NA_real_),
         age8.impact_new = case_when(age8.d == 1 & !is.na(age8.impact) ~ age8.impact,
                                     TRUE ~ NA_real_ ))


dat_q_sum1 <- outcome_questionr  %>%
  select(starts_with(c("GG", "early"))) %>%
  summarise_at(vars(1:length(.)),list(
    ~ table(.)[2],
    ~ sum(!is.na(.)),
    ~ length(table(.))
  )) %>%
  gather(key, value) %>%
  separate(key, c("Item", "Function"), sep = "_") %>% 
  spread("Function", value) %>%
  select(c(Item, sum, "[")) %>%
  rename(n_endorsed_MoBa = "[",
         n_MoBa = "sum")

dat_q_sum2 <-  outcome_questionr  %>%
  select(c(age8.impactord, age5.impactord)) %>%
  summarise_at(vars(1:length(.)),list(
    ~sum(!is.na(.)),
    ~table(.)[1],
    ~table(.)[2],
    ~table(.)[3],
    ~table(.)[4]
    )) %>%
  gather(key, value) %>%
  separate(key, c("Item", "Function"), sep = "_") %>% 
  spread("Function", value) %>%
  rename("MoBa_0" = "[..2",
         "MoBa_1" = "[..3",
         "MoBa_2" = "[..4",
         "MoBa_3" = "[..5", 
         n_MoBa = "sum",
 )

dat_q_sum1_3 <- outcome_questionr  %>%
  filter(preg_id %in% preg_id_3) %>%
  select(starts_with(c("GG", "early"))) %>%
summarise_at(vars(1:length(.)),list(
  ~ table(.)[2],
  ~ sum(!is.na(.)),
  ~ length(table(.))
)) %>%
  gather(key, value) %>%
  separate(key, c("Item", "Function"), sep = "_") %>% 
  spread("Function", value) %>%
  select(c(Item, sum, "[")) %>%
  rename(n_endorsed_with_age3 = "[",
         n_with_age3 = "sum")

dat_q_sum2_3 <-  outcome_questionr  %>%
  filter(preg_id %in% preg_id_3) %>%
  select(c(age8.impactord, age5.impactord))%>%
  summarise_at(vars(1:length(.)),list(
    ~sum(!is.na(.)),
    ~table(.)[1],
    ~table(.)[2],
    ~table(.)[3],
    ~table(.)[4]
  )) %>%
  gather(key, value) %>%
  separate(key, c("Item", "Function"), sep = "_") %>% 
  spread("Function", value) %>%
  rename("with_age3_0" = "[..2",
         "with_age3_1" = "[..3",
         "with_age3_2" = "[..4",
         "with_age3_3" = "[..5", 
         n_with_age3 = "sum") %>%
  merge(dat_q_sum1_3, all = TRUE, by = c("Item", 
                                         "n_with_age3"))

outcome_sum2 <- dat_q_sum1 %>%
  merge(dat_q_sum2, all = TRUE, by = c("Item", "n_MoBa")) %>%
  merge(dat_q_sum2_3, all = TRUE, by = c("Item")) %>%
  mutate(percent_with_age3_0 = with_age3_0 /MoBa_0,
         percent_with_age3_1 = with_age3_1 /MoBa_1,
         percent_with_age3_2 = with_age3_2 /MoBa_2,
         percent_with_age3_3 = with_age3_3 /MoBa_3,
         question = case_when( Item == "age5.impact" ~ "SDQ scale of impact on child's life",
                               Item == "age8.impact" ~ "SDQ scale of impact on child's life",
                               Item == "GG173" ~   "Child referred to Habilitation service",
                               Item == "GG174" ~ "Child referred to Educational psychology service",
                               Item == "GG175" ~  "Child referred to Child psychiatric clinic/department",
                               Item == "LL326" ~  "any concerns about how your child speaks and pronounce sounds?",
                               Item == "LL327" ~  "concerned b/c your child is demanding and difficult to cope with?", 
                               Item == "LL328" ~ "concerned b/c your child is hardly interested in playing w/ other children?",
                               Item == "LL329" ~ "concerns because your child's activity level is so high?", 
                               Item == "LL330" ~ "Have others expressed concerns about your child's development?", 
                               Item == "LL331" ~ "had difficulties in one or more of these areas?", 
                               Item == "NN242" ~ "special education - language" , 
                               Item == "NN244" ~ "special education   - arithmetic", 
                               Item == "NN246" ~ "special education - other subject", 
                               Item == "NN248" ~ "special education - other educational support",
                               Item == "NN250" ~ "extra help  because of a disability or a developmental problem", 
                               Item == "NN388" ~ "difficulties in concentration",
                               Item == "NN389" ~ "difficulties in behaviour", 
                               Item == "NN390" ~ "difficulties in emotions", 
                               Item == "NN391" ~ "difficulties in getting along with others", 
                               Item == "NN392" ~ "difficulties in language"),
         age = case_when(grepl("GG|early", Item) ~ "3yr", 
                         grepl("NN|age8", Item ) ~ "8yr", 
                         grepl("JJ", Item) ~ "7yr", 
                         grepl("LL|age5", Item) ~ "5yr")) %>%
  relocate(c(question, age), .after = Item) %>%
  relocate(c(n_with_age3,n_endorsed_with_age3), .after = n_MoBa) %>%
  relocate(c(with_age3_0, percent_with_age3_0), .after = MoBa_0) %>%
  relocate(c(with_age3_1, percent_with_age3_1), .after = MoBa_1) %>%
  relocate(c(with_age3_2, percent_with_age3_2), .after = MoBa_2) %>%
  relocate(c(with_age3_3, percent_with_age3_3), .after = MoBa_3) 
  
openxlsx::write.xlsx(outcome_sum2, "N:/durable/projects/neurodev_structure/results/sum_outcome_questionnaire.xlsx",sheetName = "Outcomes_questionnaire", append = TRUE,overwrite = TRUE)

out_dat3 <- outcome_questionr %>%
  select(c("preg_id", "BARN_NR", "m_id", age8.impactord, age5.impactord, early.service.refer))

out_dat4 <- merge.data.frame(out_dat3, dat_outcome, by = c("preg_id", "BARN_NR", "m_id"))

#saving data

saveRDS(out_dat4, file = "N:/durable/projects/neurodev_structure/data/dat_outcome_final.rds")


#early referral by dx and birth yr
percent_early.refer_andDX <- merge.data.frame(outcome_diagnosis, outcome_questionr, by = c("preg_id", "BARN_NR", "m_id")) %>% 
  filter(preg_id %in% preg_id_3) %>%
  group_by(birth_yr) %>%
  select(contains("age_"), early.service.refer, birth_yr) %>%
  summarise_at(vars(1:(length(.) -1)),list(
    ~round(sum(!is.na(.) & early.service.refer == 1, na.rm = TRUE) / sum(!is.na(.))*100, 3)
    )) %>% 
  gather(diagnosis_var,value, -birth_yr) %>%
  spread(birth_yr, value)

