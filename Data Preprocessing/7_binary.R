# Load libraries
library(haven)      # load the SAV data set
library(labelled)   # check and change the questions' labels


# Load the data
df <- read_sav("Data/Third preprocess/df_mean.sav")


# start and finish values are almost all unique 
length(unique(df$tm_start))
length(unique(df$tm_finish))
# drop Q13, start and finish time 
drop <- c("tm_finish","tm_start")
df_clean <- df[,!(names(df) %in% drop)]

# remove attributes from the dataset (spss format, labels, and display width)
df_clean[] <- lapply(df_clean, function(x) { attributes(x) <- NULL; x })

##########################################################################
########### changing all binary variables to no (0) and yes (1) ##########
##########################################################################
## Change Q12 from 1 (missed) and 2 (Did not miss) to 1 (yes, missed) and 0 (no, did not miss)
# 2 (no) -> 0
# 1 (yes) -> 1
df_clean$Q12bin <- ifelse(df_clean$Q12 %in% c(1),1,
                            ifelse(df_clean$Q12 %in% c(2),0,-1))

drop_Q12 <- c("Q12")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q12)]


#### Q5 ####
# 2 (no) -> 0
# 1 (yes) -> 1
df_clean$Q5bin <- ifelse(df_clean$Q5 %in% c(1),1,
                          ifelse(df_clean$Q5 %in% c(2),0,
                                 ifelse(df_clean$Q5 %in% c(3),2, -1)))

drop_Q5 <- c("Q5")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q5)]


#### Q10 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# 3 (did't have) -> 2
# -1 (refused) -> -1
df_clean$Q10bin <- ifelse(df_clean$Q10 %in% c(1),1,
                         ifelse(df_clean$Q10 %in% c(2),0,
                                ifelse(df_clean$Q10 %in% c(3),2, -1)))

drop_Q10 <- c("Q10")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q10)]



#### Q15 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q15bin <- ifelse(df_clean$Q15 %in% c(1),1,
                          ifelse(df_clean$Q15 %in% c(2),0,-1))

drop_Q15 <- c("Q15")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q15)]


#### Q18 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# 3 (dont' know) -> 2
# -1 (refused) -> -1
df_clean$Q18bin <- ifelse(df_clean$Q18 %in% c(1),1,
                          ifelse(df_clean$Q18 %in% c(2),0,
                                 ifelse(df_clean$Q18 %in% c(3),2, -1)))

drop_Q18 <- c("Q18")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q18)]


#### Q43 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q43bin <- ifelse(df_clean$Q43 %in% c(1),1,
                          ifelse(df_clean$Q43 %in% c(2),0,-1))

drop_Q43 <- c("Q43")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q43)]


#### Q49 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q49bin <- ifelse(df_clean$Q49 %in% c(1),1,
                          ifelse(df_clean$Q49 %in% c(2),0,-1))

drop_Q49 <- c("Q49")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q49)]



#### Q53 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q53bin <- ifelse(df_clean$Q53 %in% c(1),1,
                          ifelse(df_clean$Q53 %in% c(2),0,-1))

drop_Q53 <- c("Q53")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q53)]


#### Q55 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q55bin <- ifelse(df_clean$Q55 %in% c(1),1,
                          ifelse(df_clean$Q55 %in% c(2),0,
                                 ifelse(df_clean$Q55 %in% c(3),2, -1)))

drop_Q55 <- c("Q55")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q55)]


#### Q59 ####
# 2 (no) -> 0
# 1 (yes) -> 1
# -1 (refused) -> -1
df_clean$Q59bin <- ifelse(df_clean$Q59 %in% c(1),1,
                          ifelse(df_clean$Q59 %in% c(2),0,-1))

drop_Q59 <- c("Q59")
df_clean <- df_clean[,!(names(df_clean) %in% drop_Q59)]

#### ppgender ####
# 2 (female) -> 0
# 1 (male) -> 1

df_clean$ppgndrbin <- ifelse(df_clean$ppgender %in% c(1),1,
                          ifelse(df_clean$ppgender %in% c(2),0,-1))

drop_ppgender <- c("ppgender")
df_clean <- df_clean[,!(names(df_clean) %in% drop_ppgender)]

#### Q12 (outcome variable) ####
# Change Q12 from 1 (missed) and 2 (Did not miss) to 1 (missed) 
# and 0(did not miss)
# 2 (no) -> 0
# 1 (yes) -> 1
df_clean_2$Q12bin <- ifelse(df_clean_2$Q12 %in% c(1),1,
                            ifelse(df_clean_2$Q12 %in% c(2),0,-1))

drop_Q12 <- c("Q12")
df_clean_2 <- df_clean_2[,!(names(df_clean_2) %in% drop_Q12)]

#################################

# save the data (df_binary)
write_sav(df_clean,"Data/Q_13/df_binary.sav")

