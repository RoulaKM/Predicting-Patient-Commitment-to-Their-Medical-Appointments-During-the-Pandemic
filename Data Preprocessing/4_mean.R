#load libraries
library(haven)      # load the SAV data set
library(labelled)   # check and change the questions' labels 
library(dplyr)     # use apply function 

#########################

# Load filtered Data set
df_update <- read_sav("Data/Third preprocess/df_impute.sav")

#########################
## Q19
val_labels(df_update$Q19a)
#create new col 
df_update$Q19 <- apply(df_update[,c("Q19a","Q19b","Q19c","Q19d","Q19e","Q19f",
                                    "Q19g","Q19h","Q19i","Q19j","Q19k",
                                    "Q19l","Q19m","Q19n")],1,mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q19, .after=Q18)
# add label to the col
var_label(df_update$Q19) <- "Psychological well being"
# drop col Q19a thro Q19n
drop_Q19 <- c("Q19a","Q19b","Q19c","Q19d","Q19e","Q19f","Q19g",
              "Q19h","Q19i","Q19j","Q19k","Q19l","Q19m","Q19n")
df_update <- df_update[,!(names(df_update) %in% drop_Q19)]


#########################
## Q20
val_labels(df_update$Q20a)
#create new col 
df_update$Q20 <- apply (df_update[,c("Q20a","Q20b","Q20c","Q20d","Q20e")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q20, .after=Q19)
# add label to the col
var_label(df_update$Q20) <- "Mindfulness"
# drop col Q20a thro Q20n
drop_Q20 <- c("Q20a","Q20b","Q20c","Q20d","Q20e")
df_update <- df_update[,!(names(df_update) %in% drop_Q20)]


#########################
## Q21
val_labels(df_update$Q21a)
#create new col 
df_update$Q21 <- apply (df_update[,c("Q21a","Q21b","Q21c","Q21d")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q21, .after=Q20)
# add label to the col
var_label(df_update$Q21) <- "Neighborhood Social Cohesion"
# drop col Q20a thro Q20n
drop_Q21 <- c("Q21a","Q21b","Q21c","Q21d")
df_update <- df_update[,!(names(df_update) %in% drop_Q21)]



#########################
## Q22
val_labels(df_update$Q22a)
#create new col 
df_update$Q22 <- apply (df_update[,c("Q22a","Q22b","Q22c","Q22d","Q22e","Q22f",
                                     "Q22g","Q22h","Q22i","Q22j","Q22k")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q22, .after=Q21)
# add label to the col
var_label(df_update$Q22) <- "Support"
# drop col Q22a thro Q22k
drop_Q22 <- c("Q22a","Q22b","Q22c","Q22d","Q22e","Q22f","Q22g","Q22h","Q22i","Q22j","Q22k")
df_update <- df_update[,!(names(df_update) %in% drop_Q22)]


#########################
## Q24
val_labels(df_update$Q24a)
#create new col 
df_update$Q24 <- apply (df_update[,c("Q24a","Q24b","Q24c")],1, mean) 
# move question to its in-order place
df_update <- df_update %>% relocate(Q24, .after=Q23f)
# add label to the col
var_label(df_update$Q24) <- "Loneliness"
# drop col Q24a thro Q24c
drop_Q24 <- c("Q24a","Q24b","Q24c")
df_update <- df_update[,!(names(df_update) %in% drop_Q24)]


#########################
## Q25
val_labels(df_update$Q25a)
#create new col 
df_update$Q25 <- apply (df_update[,c("Q25a","Q25b","Q25c","Q25d",
                                     "Q25e","Q25f","Q25g")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q25, .after=Q24)
# add label to the col
var_label(df_update$Q25) <- "Fear of COVID"
# drop col Q19a thro Q19n
drop_Q25 <- c("Q25a","Q25b","Q25c","Q25d","Q25e","Q25f","Q25g")
df_update <- df_update[,!(names(df_update) %in% drop_Q25)]

#########################
## Q28
val_labels(df_update$Q28a)
#create new col 
df_update$Q28 <- apply (df_update[,c("Q28a","Q28b")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q28, .after=Q27o)
# add label to the col
var_label(df_update$Q28) <- "Mask Acceptance"
# drop col Q19a thro Q19n
drop_Q28 <- c("Q28a","Q28b")
df_update <- df_update[,!(names(df_update) %in% drop_Q28)]


#########################
## Q30
val_labels(df_update$Q30a)
#create new col 
df_update$Q30 <- apply (df_update[,c("Q30a","Q30b","Q30c")],1, mean) 
# move question to its in-order place
df_update <- df_update %>% relocate(Q30, .after=Q29k)
# add label to the col
var_label(df_update$Q30) <- "Suseptibility to COVID"
# drop col Q19a thro Q19n
drop_Q30 <- c("Q30a","Q30b","Q30c")
df_update <- df_update[,!(names(df_update) %in% drop_Q30)]


#########################
## Q31
val_labels(df_update$Q31a)
#create new col 
df_update$Q31 <- apply (df_update[,c("Q31a","Q31b","Q31c")],1, mean) 
# move question to its in-order place
df_update <- df_update %>% relocate(Q31, .after=Q30)
# add label to the col
var_label(df_update$Q31) <- "Severity"
# drop col Q31a thro Q31c
drop_Q31 <- c("Q31a","Q31b","Q31c")
df_update <- df_update[,!(names(df_update) %in% drop_Q31)]


#########################
## Q33
val_labels(df_update$Q33a)
#create new col 
df_update$Q33 <- apply (df_update[,c("Q33a","Q33b","Q33c","Q33d","Q33e")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q33, .after=Q32b)
# add label to the col
var_label(df_update$Q33) <- "Self Efficacy"
# drop col Q33a thro Q33e
drop_Q33 <- c("Q33a","Q33b","Q33c","Q33d","Q33e")
df_update <- df_update[,!(names(df_update) %in% drop_Q33)]



#########################
## Q34
val_labels(df_update$Q34a)
#create new col 
df_update$Q34 <- apply (df_update[,c("Q34a","Q34b","Q34c","Q34d")],1, mean) 
# move question to its in-order place
df_update <- df_update %>% relocate(Q34, .after=Q33)
# add label to the col
var_label(df_update$Q34) <- "Trust in Government and Healthcare workers"
# drop col Q34a thro Q34d
drop_Q34 <- c("Q34a","Q34b","Q34c","Q34d")
df_update <- df_update[,!(names(df_update) %in% drop_Q34)]



#########################
## Q35
val_labels(df_update$Q35a)
#create new col 
df_update$Q35 <- apply (df_update[,c("Q35a","Q35b","Q35c","Q35d",
                                     "Q35e","Q35f","Q35g","Q35h","Q35i")],1, mean)  
# move question to its in-order place
df_update <- df_update %>% relocate(Q35, .after=Q34)
# add label to the col
var_label(df_update$Q35) <- "Behavioral Compliance"
# drop col Q35a thro Q35i
drop_Q35 <- c("Q35a","Q35b","Q35c","Q35d","Q35e","Q35f","Q35g","Q35h","Q35i")
df_update <- df_update[,!(names(df_update) %in% drop_Q35)]


#########################
## Q40
val_labels(df_update$Q40a)
#create new col 
df_update$Q40 <- apply (df_update[,c("Q40a","Q40b","Q40c","Q40d")],1, mean)   
# move question to its in-order place
df_update <- df_update %>% relocate(Q40, .after=Q39)
# add label to the col
var_label(df_update$Q40) <- "Disatisfaction with COVID info search"
# drop col Q40a thro Q40d
drop_Q40 <- c("Q40a","Q40b","Q40c","Q40d")
df_update <- df_update[,!(names(df_update) %in% drop_Q40)]


#########################
## Q47
val_labels(df_update$Q47a)
#create new col 
df_update$Q47 <- apply (df_update[,c("Q47a","Q47b","Q47c",
                                     "Q47d","Q47e")],1, mean) 
# move question to its in-order place
df_update <- df_update %>% relocate(Q47, .after=Q46)
# add label to the col
var_label(df_update$Q47) <- "Health Mavenism"
# drop col Q47a thro Q47e
drop_Q47 <- c("Q47a","Q47b","Q47c","Q47d","Q47e")
df_update <- df_update[,!(names(df_update) %in% drop_Q47)]


#########################
## Q48
val_labels(df_update$Q48a)
#create new col 
df_update$Q48 <- apply (df_update[,c("Q48a","Q48b","Q48c",
                                     "Q48d","Q48e")],1, mean)  
# move question to its in-order place
df_update <- df_update %>% relocate(Q48, .after=Q47)
# add label to the col
var_label(df_update$Q48) <- "Health Mavenism on Social Media"
# drop col Q48a thro Q48e
drop_Q48 <- c("Q48a","Q48b","Q48c","Q48d","Q48e")
df_update <- df_update[,!(names(df_update) %in% drop_Q48)]



##################################
## require complex process ##
##################################
## Q23
# half of the statements are positive attitude towards resilience
#and the other half is negative
# therefore, I am flipping here the scale of the negative statements (b,d,f)
# the higher the median the better is the resilience
# check proportions for Q23b
prop.table(table(df_update$Q23b))
# flip the scale for Q23b
df_update$Q23b.r <- ifelse(df_update$Q23b %in% c(1),5,
                    ifelse(df_update$Q23b %in% c(2),4,
                    ifelse(df_update$Q23b %in% c(3),3,
                    ifelse(df_update$Q23b %in% c(4),2,
                    ifelse(df_update$Q23b %in% c(5),1,-1)))))
#check the flipped scale
prop.table(table(df_update$Q23b.r))

#####

# check proportions for Q23d
prop.table(table(df_update$Q23d))
# filp the scale for Q23b
df_update$Q23d.r <- ifelse(df_update$Q23d %in% c(1),5,
                    ifelse(df_update$Q23d %in% c(2),4,
                    ifelse(df_update$Q23d %in% c(3),3,
                    ifelse(df_update$Q23d %in% c(4),2,
                    ifelse(df_update$Q23d %in% c(5),1,-1)))))
#check the flipped scale
prop.table(table(df_update$Q23d.r))

#####

# check proportions for Q23f
prop.table(table(df_update$Q23f))
# filp the scale for Q23b
df_update$Q23f.r <- ifelse(df_update$Q23f %in% c(1),5,
                    ifelse(df_update$Q23f %in% c(2),4,
                    ifelse(df_update$Q23f %in% c(3),3,
                    ifelse(df_update$Q23f %in% c(4),2,
                    ifelse(df_update$Q23f %in% c(5),1,-1)))))
#check the flipped scale
prop.table(table(df_update$Q23f.r))

#create new col by taking the median
df_update$Q23 <- apply (df_update[,c("Q23a","Q23b.r","Q23c","Q23d.r",
                                     "Q23e","Q23f.r")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q23, .after=Q22)
# add label to the col
var_label(df_update$Q23) <- "Resilience"
# drop col Q22a thro Q22k
drop_Q23 <- c("Q23a","Q23b.r","Q23b","Q23c","Q23d","Q23d.r","Q23e","Q23f","Q23f.r")
df_update <- df_update[,!(names(df_update) %in% drop_Q23)]

##################################
## Q27
# Except statments d and n, statments are all incorrect regaring COVID
# Therefore, I am filpping the scale for (d and n) 
# the higher the average, the misinformative the individuale is 
# check proportions for Q27d
prop.table(table(df_update$Q27d))
# flip the scale for Q27d
df_update$Q27d.r <- ifelse(df_update$Q27d %in% c(1),2,
                           ifelse(df_update$Q27d %in% c(2),1,
                                  ifelse(df_update$Q27d %in% c(3),3,-1)))
#check the flipped scale
prop.table(table(df_update$Q27d.r))

#####

# check proportions for Q27n
prop.table(table(df_update$Q27n))
# flip the scale for Q27d
df_update$Q27n.r <- ifelse(df_update$Q27n %in% c(1),2,
                           ifelse(df_update$Q27n %in% c(2),1,
                                  ifelse(df_update$Q27n %in% c(3),3,-1)))
#check the flipped scale
prop.table(table(df_update$Q27n.r))

###

#create new col by taking the median
df_update$Q27 <- apply (df_update[,c("Q27a","Q27b","Q27c","Q27d.r",
                                     "Q27e","Q27f","Q27g","Q27h","Q27i",
                                     "Q27j","Q27k","Q27l","Q27m","Q27n.r",
                                     "Q27o")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q27, .after=Q26_7)
# add label to the col
var_label(df_update$Q27) <- "Mis-info about COVID"
# drop col Q22a thro Q22k
drop_Q27 <- c("Q27a","Q27b","Q27c","Q27d.r","Q27d",
              "Q27e","Q27f","Q27g","Q27h","Q27i",
              "Q27j","Q27k","Q27l","Q27m","Q27n.r",
              "Q27n","Q27o")
df_update <- df_update[,!(names(df_update) %in% drop_Q27)]

##################################
## Q29
#create new col by taking the median
df_update$Q29 <- apply (df_update[,c("Q29a","Q29b","Q29c","Q29d","Q29e","Q29f",
                                     "Q29g","Q29h","Q29i","Q29j","Q29k")],1, mean)
# move question to its in-order place
df_update <- df_update %>% relocate(Q29, .after=Q28)
# add label to the col
var_label(df_update$Q29) <- "Symptoms of COVID"
# drop col Q22a thro Q22k
drop_Q29 <- c("Q29a","Q29b","Q29c","Q29d","Q29e","Q29f",
              "Q29g","Q29h","Q29i","Q29j","Q29k")
df_update <- df_update[,!(names(df_update) %in% drop_Q29)]

####################################

## save the dataset
write_sav(df_update, "Data/Third preprocess/df_mean.sav")

