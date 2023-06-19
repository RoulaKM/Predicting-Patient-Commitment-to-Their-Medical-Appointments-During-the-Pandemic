#load libraries 
library(haven)

#read the dataset
df <- read_sav("Data/Q_13/df_Q13cat.sav")

# create a dataset that does not have negative values by
#changing all negative values to 0
df_positive <- df

df_positive[df_positive<0] <- 0

# save the data
write_sav(df_positive, "Data/Forth preprocess/df_positive.sav")
