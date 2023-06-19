#Load libraries
library(haven)        #load the SAV data set
library(tidyverse)   #mutate and replace functions
library(labelled)    #check and change questions' labels 
library(VIM)         # plotting NA values

###########################

# Load filtered Data set
df_filtered <- read_sav("Data/Third preprocess/df_filtered.sav")

###########################
##### Cleaning Data #####
##########################

## identify variables with more than 50% missing data
df_na50 <- df_filtered[, which(colMeans(is.na(df_filtered)) > 0.5)]
names(df_na50)

###########################

# Q7- Has the number of cigarettes you smoke per day increased or
#decreased during the past few months?
val_labels(df_filtered$Q7)
#I want to keep this question, so I am changing refused (-1) and NAs to 0
df_filtered$Q7[is.na(df_filtered$Q7)] <- 0


# Q11: 1-5 - How did you receive the recommended cancer screening?
val_labels(df_filtered$Q11_1)
#I want to keep this question as it might matter the type of appt
df_filtered <- df_filtered %>%
  mutate_at(vars(Q11_1,Q11_2,Q11_3,Q11_4,Q11_5), ~replace_na(., -1))

#Q13- What are the services you missed?
# was done in a separate notebook (8_Q13_cat)
#drop Q13_Refused


#Q16- 1:8- Why have you been tested for COVID-19?
# I don't want to remove the variable, so I will change NA to -1
# we don't have anyone who refused to answer, so no -1 values in the
# original dataset.
df_filtered <-  df_filtered %>%
  mutate_at(vars(Q16_1,Q16_2,Q16_3,Q16_4,Q16_5,
                 Q16_6,Q16_7,Q16_8), ~replace_na(., -1))


# Q17- Have you been diagnosed with COVID-19 (you tested positive for COVID-19)?
# I don't to remove this question. So I am changing all NAs to 0
val_labels(df_filtered$Q17)
df_filtered$Q17[is.na(df_filtered$Q17)] <- -1
df_filtered$Q17[df_filtered$Q17==2] <- 0


#Q44- Why did you avoid information about COVID-19?
val_labels(df_filtered$Q44_1)
# I wanted to keep this question, so I changed nA to -1
df_filtered <-  df_filtered %>%
  mutate_at(vars(Q44_1,Q44_2,Q44_3,Q44_4,Q44_5,Q44_6), ~replace_na(., -1))



#drop the following variables that has 50% or more NA values 
# "Q13_Refused" "Q50"  "T_Q7"  "T_Q11"  "T_Q13"      
# "T_Q16" "T_Q17"  "T_Q44"  "T_Q50"  
df_filtered <-  df_filtered[, which(colMeans(!is.na(df_filtered)) > 0.5)]

###########################

# Viewing NA values
aggr_plot <- aggr(df_filtered, col=c('blue','orange'),
                  numbers= TRUE,
                  sortVars=TRUE,
                  labels=names(df_filtered),
                  cex.axis=.7,
                  gap=3,
                  ylab=c('Histogram of Missing Data','Pattern'))

###########################

#Questions Q51, Q52, Q14 1~5 have more that 10% NAs

#Q51- If you are not working now, were you working before the COVID-19 outbreak?
val_labels(df_filtered$Q51)
df_filtered$Q51[is.na(df_filtered$Q51)] <- -1
df_filtered$Q51[df_filtered$Q51==2] <- 0

#Q52- If you are not working after the COVID-19 outbreak,
#do you now receive unemployment benefits?
val_labels(df_filtered$Q52)
df_filtered$Q52[is.na(df_filtered$Q52)] <- -1
df_filtered$Q52[df_filtered$Q52==2] <- 0

#Q14- (1:5)- How did you receive the healthcare services
#for you or someone under your care?
val_labels(df_filtered$Q14_1)
#I want to keep this question as it might matter the type of appt
df_filtered <- df_filtered %>%
  mutate_at(vars(Q14_1,Q14_2,Q14_3,Q14_4,Q14_5), ~replace_na(., -1))

###########################

## Remove time variables for each question
df_filtered <- df_filtered[,1:268]

## remove meaningless variables
#should I? ,"tm_start","tm_finish"
drop <- c("CaseID","Q9_TEXT","Q11_TEXT","Q14_TEXT","Q16_TEXT","Q44_TEXT","Q61_TEXT")
df_drop <- df_filtered[,!(names(df_filtered) %in% drop)]

###########################

# check for NA values 
aggr_plot <- aggr(df_drop, col=c('blue','orange'),
                  numbers= TRUE,
                  sortVars=TRUE,
                  labels=names(df_drop),
                  cex.axis=.7,
                  gap=3,
                  ylab=c('Histogram of Missing Data','Pattern'))

###########################

# save the new dataset
write_sav(df_drop,"Data/Third preprocess/df_drop.sav")
