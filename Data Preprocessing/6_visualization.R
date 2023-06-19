## libraries
library(haven)       #load SAV data set
library(table1)      #change the labels and units, and create the tables
library(dplyr)       # uset the relocate function

#load the dataset
df <- read_sav("Data/Third preprocess/df_mean.sav")

# Change numeric columns to factors with labels for better visualization
df$ppgender<-factor(df$ppgender,labels = c("Male","Female"),ordered = T)

df$ppethm<-factor(df$ppethm,labels = c("White","Black","Other",
                                       "Hispanic","2+ Race"),ordered = T)

df$ppreg4<-factor(df$ppreg4,labels = c("Northeast","Midwest",
                                       "South","West"),ordered = T)

df$ppmsacat<-factor(df$ppmsacat,labels = c("Non-Metro","Metro"),ordered = T)

df$ppeducat<-factor(df$ppeducat,labels = c("Less than High School","High School",
                                           "Some College", "Bachelor's or higher"),ordered = T)

df$ppagect4<-factor(df$ppagect4,labels = c("18-29","30-44","45-59", " 60+"),ordered = T)

df$Q12<-factor(df$Q12,labels = c("Yes, I missed","No, I did not miss"),ordered = T)

df$Q9_1<-factor(df$Q9_1,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_2<-factor(df$Q9_2,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_3<-factor(df$Q9_3,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_4<-factor(df$Q9_4,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_5<-factor(df$Q9_5,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_6<-factor(df$Q9_6,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_7<-factor(df$Q9_7,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_8<-factor(df$Q9_8,labels = c("Refused","No","Yes"),ordered = T)

df$Q9_9<-factor(df$Q9_9,labels = c("Refused","No","Yes"),ordered = T)

# generate a new column for the income with less categories

df$ppincome6  <-  ifelse(df$ppincimp %in% c(1,2,3,4,5,6,7),1,
              ifelse(df$ppincimp %in% c(8,9,10,11),2,
              ifelse(df$ppincimp %in% c(12,13),3,
              ifelse(df$ppincimp %in% c(14,15),4,
              ifelse(df$ppincimp %in% c(16,17),5,
              ifelse(df$ppincimp %in% c(18,19,20,21),6,-1))))))

df <- df %>% relocate(ppincome6, .after=ppincimp)

df$ppincome6<-factor(df$ppincome6,labels = c("$5,000 to $24,999","$25,000 to $49,999",
                                             "$50,000 to $74,999", "$75,000 to $99,999",
                                             "$100,000 to $149,999", "$150,000 +"),ordered = T)


# change labels in the table
label(df$ppagect4) <- "Age"
label(df$ppgender) <- "Gender"
label(df$ppeducat) <- "Education"
label(df$ppreg4)   <- "Region"
label(df$ppmsacat) <- "Metropolitan Area"
label(df$ppincome6)<- "Income"
label(df$ppethm)<- "Ethnicity"
label(df$Q9_1)<- "Cancer"
label(df$Q9_2)<- "Diabetes or high blood sugar"
label(df$Q9_3)<- "High blood pressure or hypertension"
label(df$Q9_4)<- "Heart condition"
label(df$Q9_5)<- "Chronic lung disease"
label(df$Q9_6)<- "Arthritis or rheumatism"
label(df$Q9_7)<- "HIV"
label(df$Q9_8)<- "Other"
label(df$Q9_9)<- "None of the above"

# change age unit
units(df$ppagect4) <- "years"

# Create tables 1 (demographics)
table1(~ ppagect4 + ppgender + ppeducat + ppincome6 + ppreg4 + ppmsacat + ppethm|Q12 , data= df,na.rm=TRUE )

# create table 2 (medical conditions)
table1(~ Q9_1+ Q9_2+ Q9_3+ Q9_4+ Q9_5+ Q9_6+ Q9_7+ Q9_8+ Q9_9|Q12 , data= df,na.rm=TRUE )


