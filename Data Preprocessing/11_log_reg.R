#load the libraries 
library(haven)       # load the data set
library(dplyr)      # use the select function
library(ltm)        # use forward selection and back elimination
library(ggplot2)    # plotting 
library(gmodels)    # for the cross table function
library(Greg)       # for the printCrudeAndAdjustedModel function


##########################################
# read the data
df <- read_sav("Data/Sisth_ without 13&14/df_pos_without.sav")


# remove attributes from the dataset (spss format, labels, and display width)
df[] <- lapply(df, function(x) { attributes(x) <- NULL; x })


# create a subset of the data
df_sub <- df %>%
  select('Q19','Q42e','Q42i','Q45i','Q59bin',
         'ppeducat','Q44_2','Q2','Q42g','Q3',
         'Q21','Q40d','ppmarit','Q38j','Q22',
         'duration','ppage', 'Q36e', 'Q9_9',
         'Q12bin','ppeduc','pphispan', 'Q58','Q46','Q63',
         'Q32b','Q26_7')


# attach dataset
attach(df_sub)

############################################
######## preparing for the model ##########
############################################

#convert categorical variables to factor
df_sub$Q12bin <- as.factor(df_sub$Q12bin)
df_sub$Q42e <- as.factor(df_sub$Q42e)
df_sub$Q42g <- as.factor(df_sub$Q42g)
df_sub$Q42i <- as.factor(df_sub$Q42i)
df_sub$Q45i <- as.factor(df_sub$Q45i)
df_sub$Q59bin <- as.factor(df_sub$Q59bin)
df_sub$ppeducat <- as.factor(df_sub$ppeducat)
df_sub$Q44_2 <- as.factor(df_sub$Q44_2)
df_sub$Q3 <- as.factor(df_sub$Q3)
df_sub$Q40d <- as.factor(df_sub$Q40d)
df_sub$ppmarit <- as.factor(df_sub$ppmarit)
df_sub$Q38j <- as.factor(df_sub$Q38j)
df_sub$Q36e <- as.factor(df_sub$Q36e)
df_sub$Q9_9 <- as.factor(df_sub$Q9_9)
df_sub$ppeduc <- as.factor(df_sub$ppeduc)
df_sub$pphispan <- as.factor(df_sub$pphispan)
df_sub$Q46 <- as.factor(df_sub$Q46)
df_sub$Q63 <- as.factor(df_sub$Q63)
df_sub$Q32b <- as.factor(df_sub$Q32b)
df_sub$Q26_7 <- as.factor(df_sub$Q26_7)

# label the factors
df_sub$Q9_9 <- factor(df_sub$Q9_9,labels = c("No", "Yes"))
df_sub$Q59bin <- factor(df_sub$Q59bin,labels = c("No", "Yes"))
df_sub$Q12bin <- factor(df_sub$Q12bin,labels = c("No", "Yes"))
df_sub$Q32b <- factor(df_sub$Q32b,labels = c("Refused", "Not at all likely",
                                                 "Unlikely", "Likely",
                                                 "Very likely"))

############################################
########## building the models ##########
###########################################

#build logistic regression using all variables (full model)
df_full_model <- glm(data=df_sub, Q12bin~ Q19+Q42e+Q42i+Q45i+Q59bin+
                     Q44_2+Q2+Q42g+Q42i+Q3+
                     Q21+Q40d+ppmarit+Q38j+Q22+
                     duration+ppage+Q36e+Q9_9+
                     ppeduc+pphispan+Q58+Q46+Q63+
                     Q32b+Q26_7, family="binomial")

#build logistic regression that includes only the intercept (intercept model)
df_intercept_model <- glm(data=df_sub, Q12bin ~1, family = "binomial")

#check the summary
summary(df_full_model)
summary(df_intercept_model)

############################################
##forward selection and back elimination ##
###########################################

#forward selection
df_forward_model <- stepAIC(df_intercept_model, direction= "forward", scope= formula(df_full_model), trace=FALSE)
df_forward_model$anova

#backward Elimination 
df_backward_model <- stepAIC(df_full_model, direction = "backward", scope= formula(df_full_model), trace = FALSE)
df_backward_model$anova

############################################
#### search for the parsimonious model ####
########################################### 

## model 2
df_full_model_2 <- glm(data=df_sub, Q12bin~ Q59bin + Q19 + Q2 + Q26_7 +
                         Q9_9 + Q44_2 + Q42g + duration + Q42e,
                       family="binomial")
summary(df_full_model_2)

## model 3
df_full_model_3 <- glm(data=df_sub, Q12bin~ Q59bin + Q19 + Q2 + Q26_7 +
                         Q9_9 + Q44_2 + Q42g + duration,
                       family="binomial")
summary(df_full_model_3)

## model 4
df_full_model_4 <- glm(data=df_sub, Q12bin~ Q59bin + Q19 + Q2 + Q26_7 +
                         Q9_9 + Q44_2 + Q42g + duration + Q44_2,
                       family="binomial")
summary(df_full_model_4)

## model 5
df_full_model_5 <- glm(data=df_sub, Q12bin~ Q59bin + Q19 + Q2 + Q26_7 +
                         Q9_9 + Q44_2 + Q42g + duration + Q44_2,
                       family="binomial")
summary(df_full_model_5)

## model 6
df_full_model_6 <- glm(data=df_sub, Q12bin~ Q32b+Q19+Q59bin+
                         Q2+Q9_9+Q44_2, family="binomial")
summary(df_full_model_6)

############################################
############### Plotting ##################
###########################################

# Q12 and Q19
ggplot(df_sub)+
  aes(x= Q12bin, y= Q19)+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="#3c7ade", fill="red") +
  labs(title="Relaionship Between Missing Appointments and Psychological Well-being")+
  labs(x = "Did you miss an appointment", y = "Psychological well-being scale")


# Q12 and Q59
ggplot(df_sub, aes(Q59bin, fill= Q12bin))+
  geom_bar(width = 0.5)+
  scale_fill_manual(values=c("#98bbf5","#3c7ade"))+
  labs(title="Reltionship Between Missing Appointments and Being a Caregiver")+
  labs(x = "Are you a caregiver", y = "Count", fill = "Missed Appointment")


# Q12 and Q32b
#ggplot(df_sub, aes(Q32b, fill= Q12bin))+
#  geom_bar(width = 0.5)+
#  scale_fill_manual(values=c("#98bbf5","#3c7ade"))+
#  labs(title="Reltionship Between Missing Appointments and Getting COVID-19 Vaccine
#for Someone Under Your Care")+
#  labs(x = "Scale", y = "Count", fill = "Missed Appointment")



# Q12 and Q59
#ggplot(df_sub, aes(Q9_9, fill= Q12bin))+
#  geom_bar(width = 0.5)+
#  scale_fill_manual(values=c("#98bbf5","#3c7ade"))+
#  labs(title="Do You Have any Medical Conditions?")+
#  labs(x = "Answer", y = "Count", fill = "Missed Appointment")

# Q12 and Q2
# remove the extreme points from Q2 for better visualization
df_sub_sub <- subset(df_sub, Q2 != 90)
df_sub_sub <- subset(df_sub_sub, Q2 != 45)
#plot
ggplot(df_sub_sub)+
  aes(x= Q12bin, y= Q2)+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="#3c7ade", fill="red") +
  labs(title="How Many Times per Week Did You Participate in Any Physical Activity")+
  labs(x = "Did you miss an appointment", y = "Physical Activity per Week")

############################################
############# Cross-tables ################
###########################################
# cross table for Q9_9 and Q12
CrossTable(df_sub$Q9_9,df_sub$Q12bin)

# cross table for Q32b and Q12
CrossTable(df_sub$Q32b,df_sub$Q12bin)

############################################
############### Analysis ##################
###########################################
# Test of Significance for Residual Deviance 
anova(df_full_model_6, df_forward_model, test = "Chisq")
# Crude and adjusted odd ratios 
printCrudeAndAdjustedModel(df_full_model_6)

