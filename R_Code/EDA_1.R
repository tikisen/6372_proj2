# DEPENDENCIES
library(tidyverse)

# INGEST DATA ####
path_file <- "../data/train.csv"

as_train <- read.csv(path_file, sep = ",")

rm(list = c("path_file"))

# CATEGORICAL AND CONTINIOUS ATTRIBUTES #####

as_train_cat <- as_train %>% select(contains("cat"),id)
as_train_cont <- as_train %>% select(contains("cont"),id)

# MISSING VALUES ####
as_train_cont_na <- data.frame(colSums(sapply(as_train_cont, is.na)))
names(as_train_cont_na) <- "IsNA"

if(nrow(as_train_cont_na %>% filter(IsNA>0))==0){
  rm(as_train_cont_na)
} else{
  print("Review as_train_cont_na for missing data.")
}

as_train_cat_na <- data.frame(colSums(sapply(as_train_cat, is.na)))
names(as_train_cat_na) <- "IsNA"

if(nrow(as_train_cat_na %>% filter(IsNA>0))==0){
  rm(as_train_cat_na)
} else{
  print("Review as_train_catt_na for missing data.")
}

# GATHER DATA ####

as_train_cont_tall <- as_train_cont %>% 
  gather(cont1:cont14, key = "Variable", value = "Value") %>%
  arrange(Variable)

as_train_cont_tall$Variable <- as.factor(as_train_cont_tall$Variable)


as_train_cont_tall$Variable <- factor(as_train_cont_tall$Variable, 
                                      levels = c("cont1", "cont2", "cont3", "cont4", "cont5",
                                                 "cont6", "cont7", "cont8", "cont9", "cont10",
                                                 "cont11", "cont12", "cont13", "cont14"))
# PLOTTING ####
filepath = "/Users/bgranger/Documents/SMU/6372_Applied Statistics/MSDS6372/Applied_Stats_Project_2/Proj2_Doc/6372_proj2/R_Output"

ggplot(data = as_train_cont_tall, aes(x=Variable, y=Value)) + 
  geom_boxplot() +
  ggtitle("BP1: Continuous Variables 1 through 14") 
ggsave("BP1.png", plot = last_plot(), path = filepath, width = 50, height = 20,
       units = "cm")
