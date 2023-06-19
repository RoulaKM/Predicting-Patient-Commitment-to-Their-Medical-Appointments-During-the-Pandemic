# Load the dataset
library(haven)      #read the SAV dataset
library(ggplot2)    #create the plots
library(labelled)   #check and change questions' labels

# load the dataset
raw_data <- read_sav("Data/Raw Data/Communications During COVID_Main_Client.sav")

# create a new dataset without value 3 and -1 in Q12
df_filtered <- raw_data[raw_data$Q12==1 | raw_data$Q12==2,]

# save the new dataset (df_filtered)
write_sav(df_filtered, "Data/Third preprocess/df_filtered.sav") 


# visualizing Q14 in the raw data
ggplot(raw_data,aes(x=as.factor(Q12)))+
  geom_bar(fill="#6897BB") +
  labs(y="Number of Participants", x=("Respnse to Q12"),
       title = "Over the past months, did you miss any healthcare services for you or someone under your care due to the COVID-19 outbreak?")


#visualizing Q12 in the filtered data
# label yes as 1 and 2 as no for better visualisation
df_filtered$Q12<-factor(df_filtered$Q12,labels = c("Yes, I missed","No, I did not miss"),ordered = T)
# visualization 
ggplot(df_filtered,aes(x=as.factor(Q12)))+
  geom_bar(fill="#6897BB") +
  labs(y="Number of Participants", x=("Respnse to Q12"),
       title = "Over the past months, did you miss any healthcare services for you or someone under your care
due to the COVID-19 outbreak?")


