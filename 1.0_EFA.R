library(tidyverse)
library(MplusAutomation)
library(psych)
library(nFactors)
library(readxl)
library(openxlsx)


dat <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno.rds")

set.seed(25)
dat_Mplus. <- dat %>%
  rowwise() %>%
  mutate(split.50 = sample(0:1, 1))

dat_Mplus <- dat_Mplus. %>%
  filter(split.50 == 0 ) %>%
  select(c(preg_id, BARN_NR, m_id, starts_with("GG")))

saveRDS(dat_Mplus., "N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds") 
prepareMplusData(dat_Mplus, "scripts/Mplus/efa_split.dat", inpfile = TRUE) #input files need editing


dat_Mplus <- readRDS("N:/durable/projects/neurodev_structure/data/dat_pheno_split.rds")%>%
  filter(split.50 == 0 )%>%
  select(c(starts_with("GG")))



#removing one head nodding and those with avg inter item correlation less than 0

corObj_sub1 <- dat_Mplus %>% #Matrix was not positive definite, smoothing was done
  select(c(-GG271, -GG259, -GG278)) %>%
  mixedCor(.)

corMat_sub1 <- as.data.frame(corObj_sub1[["rho"]])

ev2.1 <- eigen(corMat_sub1)
ap2.1 <- parallel(subject = nrow(dat_Mplus) - 3, 
                var = ncol(dat_Mplus) - 3,
                rep = 100, 
                cent =0.05)
nS2.1 <- nScree(x=ev2.1$values,
              aparallel = ap2.1$eigen$qevpea)

plotnScree(nS2.1)

dat2_Mplus <- dat_Mplus %>% 
  select(c(-GG271, -GG259, -GG278))

#scree plot
nS2.1$Analysis %>% 
  mutate(Components = as.numeric(row.names(.))) %>%
  as.data.frame(.) %>%
  ggplot(aes(x = Components, y = Eigenvalues)) + 
  geom_point(size = 4) +
  geom_line(size = 0.8) + 
  geom_point(aes(x = Components, y = Par.Analysis, ),
             shape = 2,
             size = 4) +
  geom_line(aes(x = Components, y = Par.Analysis), 
            color = "red", 
            size = 0.7) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 18,face = "bold"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 18, face = "bold"))


prepareMplusData(dat2_Mplus, "scripts/Mplus/efa_split_sub.dat", inpfile = TRUE)

#EFA run in MPlus
# Reading in MPlus Results 


write_list_excel <- function(sheet, file) {
  sheet_name <- deparse(substitute(sheet))
  write.xlsx(sheet, file, sheetName = sheet_name, append = TRUE ) 
}

keepHighest <- function(df, bifactor = FALSE){
  
  max_fun <- function(x, max){
    ifelse(x %in% max, x, NA)}
  
  if (bifactor == TRUE) {
    max_list <- suppressWarnings(apply(X=df[6:length(df)], MARGIN=1, FUN=max, na.rm =TRUE))
    max_list <- max_list[max_list != -Inf]
    df[6:length(df)] <- df[6:length(df)] %>%
      mutate_if(is.numeric, max_fun, max = max_list)
  } else {
    max_list <- suppressWarnings(apply(X=df[5:length(df)], MARGIN=1, FUN=max, na.rm =TRUE))
    max_list <- max_list[max_list != -Inf]
    df[5:length(df)] <- df[5:length(df)] %>%
      mutate_if(is.numeric, max_fun, max = max_list)
  }
  
  return(df)
}

Mplus_table <- function(df, threshold){

  not_any_na <- function(x) all(!is.na(x))
  
  out.1 <- df[1:length(df)] %>%
    filter(!is.na(scale)) %>%
    select(where(not_any_na))
  
  col_names <- out.1 %>%
  select(contains("...")) %>%
  colnames() %>%
  as.vector()
  
  
  if(length(col_names) == 1){ 
out <- out.1 %>%
  as_tibble %>%
  separate(col_names[1], sep = " +", into = c("item", paste("factor", 1:5, sep = "_"))) %>%
  select(where(not_any_na)) %>%
  mutate(across(starts_with("factor"), ~str_remove(.x, "[*]")),
         across(starts_with("factor"), as.numeric)) %>%
  relocate(c("item", "scale", "domain", "Abv. Question"), .before = "factor_1")
}
  if(length(col_names) == 2){
    out <- out.1 %>%
      as_tibble %>%
      separate(col_names[1], sep = " +", into = c("item", paste("factor", 1:5, sep = "_"))) %>%
      separate(col_names[2], sep = " +", into = c("item", paste("factor", 6:10, sep = "_"))) %>%
      select(where(not_any_na)) %>%
      mutate(across(starts_with("factor"), ~str_remove(.x, "[*]")),
             across(starts_with("factor"), as.numeric)) %>%
      relocate(c("item", "scale", "domain", "Abv. Question"), .before = "factor_1")
  }
  if(length(col_names) == 3){ 
    out <- out.1 %>%
      as_tibble %>%
      separate(col_names[1], sep = " +", into = c("item", paste("factor", 1:5, sep = "_"))) %>%
      separate(col_names[2], sep = " +", into = c("item", paste("factor", 6:10, sep = "_"))) %>%
      separate(col_names[3], sep = " +", into = c("item", paste("factor", 11:15, sep = "_"))) %>%
      select(where(not_any_na)) %>%
      mutate(across(starts_with("factor"), ~str_remove(.x, "[*]")),
             across(starts_with("factor"), as.numeric)) %>%
      relocate(c("item", "scale", "domain", "Abv. Question"), .before = "factor_1") %>%
      relocate(c(paste("factor", 6:10, sep = "_")), .after = "factor_5")
  }
  
  less_than <- function(x){
    ifelse(x < threshold, NA, x)
  }
  
  out <- out %>%
    mutate_if(is.double, less_than)
  return(out)
}



#subset items - removing GG271, GG259, GG278

path_2 <- "output/EFA_sub1_raw.xlsx"

sheet_names_2 <- excel_sheets(path_2)

Mplus_raw_2 <- lapply(sheet_names_2, function(x) read_excel(path_2 , sheet = x))

names(Mplus_raw_2) <- sheet_names_2

Mplus_raw_2 <- Mplus_raw_2[2:15]

Mplus_full_2 <- Mplus_raw_2 %>%
  map(., ~Mplus_table(.,-1))

Mplus_filtered_2 <- Mplus_full_2 %>%
  map(., keepHighest)

Mplus_threshold_2 <- Mplus_raw_2 %>%
  map(., ~Mplus_table(.,0.3))




# saving edited tables


write.xlsx(Mplus_full_2, file = "results/Mplus_0_full.xlsx")
write.xlsx(Mplus_threshold_2, file = "results/Mplus_0_threshold.xlsx")
write.xlsx(Mplus_filtered_2, file = "results/Mplus_0_filtered.xlsx")

# reading xlsx files back in
tables_names <- paste0("results/", list.files("results", pattern = "Mplus.*\\.xlsx")) 

read_data <- function(path) { 
  names <- readxl::excel_sheets(path)
  all_sheets <- lapply(names, function(x) read_excel(path, sheet = x))
  names(all_sheets) <- names
  return(all_sheets)
}

tables_list <- lapply(tables_names, read_data)
names(tables_list) <- tables_names %>%
  str_remove(pattern = "results/Mplus_")


  