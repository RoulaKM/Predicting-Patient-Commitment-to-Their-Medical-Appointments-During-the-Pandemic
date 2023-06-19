# load library
library(haven)
library(mice)
library(VIM)

# load data
df_drop <- read_sav("Data/Third preprocess/df_drop.sav")

# Viewing NA values
aggr_plot <- aggr(df_drop, col=c('blue','orange'),
                  numbers= TRUE,
                  sortVars=TRUE,
                  labels=names(df_drop),
                  cex.axis=.7,
                  gap=3,
                  ylab=c('Histogram of Missing Data','Pattern'))

##########################################################################
#using random forest from mice to replace NA values
df_impute <- mice(df_drop, m=5, method = "rf")

summary(df_impute)

df_finish_impute <- complete(df_impute,1)


##########################################################################
# Viewing NA values
aggr_plot <- aggr(df_finish_impute, col=c('blue','orange'),
                  numbers= TRUE,
                  sortVars=TRUE,
                  labels=names(df_finish_impute),
                  cex.axis=.7,
                  gap=3,
                  ylab=c('Histogram of Missing Data','Pattern'))


# save dataset
write_sav(df_finish_impute,"Data/Third preprocess/df_impute.sav")
