#load the libraries
library(haven)   # laod sav data set
library(mlr)     # use removeConstantFeatures function 

#load the dataset
df <- read_sav("Data/Third preprocess/df_mean.sav")

##  remove constant, quasi-constant features
df_rm_con <- removeConstantFeatures(df,perc = 0.01)

# Remove duplicates 
df_rm_dup <- df[!duplicated(df), ]
