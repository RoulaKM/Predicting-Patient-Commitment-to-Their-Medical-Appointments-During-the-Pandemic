#load libraries
library(haven)      # load the SAV data set

# load the positive and the netative datasets
df_positive <- read_sav("Data/Fifth preprocess/df_positive.sav")
df_negative <- read_sav("Data/Forth_Q_13/df_Q13cat.sav")

# drop Qs 13 and 14 from both negative and positive datasets
drop <- c("Q13cat","Q14_1","Q14_2","Q14_3","Q14_4","Q14_5")

df_negative <- df_negative[,!(names(df_negative) %in% drop)]

df_positive <- df_positive[,!(names(df_positive) %in% drop)]

#save the datasets
write_sav(df_negative, "Data/Sisth_ without 13&14/df_neg_without.sav")
write_sav(df_positive, "Data/Sisth_ without 13&14/df_pos_without.sav")
