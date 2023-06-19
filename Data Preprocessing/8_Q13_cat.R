# Load libraries
library(haven)      # load the SAV data set
library(labelled)   # check and change the questions' labels


# Load the data
df <- read_sav("Data/Forth_Q_13/df_binary.sav")

df_Q13 <- df

# create categories (buckets) for the values in Q13

general <- c ("Physical check up", "Follow up appointment", "follow up appointments","Regular. Checkup. And labs",
              "Yearly physical; dental 6 month checkup", "Six month check up from the doctor","Routine check up",
              "follow up appt", "Routine checkup","visiting doctor for routine issues", "doctors appointment","Physical",
              "general physical, possible pap smear, possible bone density test","Check up", "6 month routine checkup", "Check ups.",
              "Chequeos rutinarios", "Follow up appointments","Annual check up","routine check-up; other", "annual phyical",
              "Regular Checkup","Doctors appointment","Routine check-up","Annual physical","Annual check","Healthcare check with new primary care doctor.",
              "Reg. Check up","Dr Appt","Doctor appointments","routine check up.","Regular checkup and switching to a new doctor",
              "Basic Check-up","Scheduling a visit for routine health care","Regular Dr appointment","doctor check up appointment",
              "Kids physicals","Check up appointment", "routine physical", "Annual Checkup", "Checkups","consultas medicas chekeo de rutina",
              "Rountine check up","Routine follow-up","Self check up. Child welness visit","Doctors appointment & service",
              "Child Doctor visit","A routine check up", "Dr appointment")

specialty <- c("Dentist, Eye Doctor, General Health", "Therapy for PTSD","routine teeth cleaning",
               "An injection for the macular degeneration was scheduled.  Travel to a different city put me in a kind of quarantine for a 14 day period before I could get the injection.",
               "Cardiologist appt.","regular doctor appointment   eye appointment", "Back injections", "Bone density test",
               "quarterly visit with cystic fibrosis docs, and annual visit with gynocoligist","allergy shots",
               "Routine dental app." ,"routine eye care,","Dermatology screening", "dental, eye test","dematologist and dental",
               " driving spouse vision field check and dermatology check." ,  "yearly blood test","Eye exam, yearly.",
               "Regular medical, mammogram","dental cleaning","blood check", "Dental appiintment, vision appointment",
               "Annual eye care appointment","orthopedic appointment, massage therapy (at hospital, not a spa), mental health appointments",
               "urologist","dentist and ENT appt for my son","Ear,Nose and Throat", "Blood test","Blood test and dental care",
               "neurology", "Eye", "General check up, dentist, dermatologust", "Oftalmología","Skin doctor appointment, neurologist appointment",
               "Dentist appointment for tooth work","Treatment", "retinal tear yearly follow-up","Massage, chiropractic, dental",
               "Dermatologist, GI specialist","Sleep Apena testing" ,"teeth cleaning","dentist, eye doctor","Dental, Neurologist,",
               "Behavioral Health appointment","Oculista","Eye checkup","Kidneys stones - Follow-Up.   Ophthalmologist.",
               "Dentist appt for cleaning" ,"blood test","Dental cleanings","Dentist", "Medical, mental, and dental services",
               "Tooth extraction" ,"Gynecologist appointment and dental appointment", "Dental check-up",
               "Chiropractic care, dental & COVID test kit equipment shortages in different areas with the cost of having the tests done!",
               "dentist appointment","dental hygiene appts","General doctor’s visit, delayed dentist cleaning" ,"Schedule dr and dentist" ,
               "phys. therapy, ophtholmology appt., tetanus shot,","Mental Sessions","My mental illness groups", "Foot care",
               "Dermatology","my husband missed an Iron injection that is injected into an IV", "dental check up",
               "General check ups and tests for problems with my digestive system","my daughter needs her Prolia injection",
               "Orthopaedist - shoulder. Dermatologist - eye. Physical therapy", "Follow ups after a surgery",
               "ROUTINE CHECK UP WITH RETINA SPECIALIST","Thrombolytic consultation from a hematologist", "Dental",
               "Dr. Visits and procedures","i missed knee and arthritis checkup", "ent appt", "Drs appointments, dental appointments",
               "Para mi mama, oculista","Needed to be seen for a on going kidney problem","Regular glaucoma follow up","Chiropractor",
               "My wife missed treatment for a recently developed chronic cough (Nov. 2019)","foot doctor,", "gyn, dont recall",
               "Blood draw, dental cleaning","Dental visit","Quarterly dental cleaning and exam","Dental appointment",
               "Fillings in my teeth. Physical therapy. Meeting in person with counselor and psych dr","Heart specialist appointment" )

scan <- c("Cta chest scan, echo on my leg and heart.","Colonoscopy", "Mri","EKG exam", "CT scan","El oculista", "Colonoscopy and Endoscopy")

surgery <- c("Hernia surgery", "Eye Procedure","My wife had scheduled surgery that had to be pushed back")

cancer <- c("skin cancer screening", "mammogram", "Pap smear, colonoscopy and mammogram.", "Routine mammogram",
            "Pap smear","Mammogram", "Lab work, mammogram, DEXA scan, dental appt","Breast exam","Annual pap smear",
            "Mamagram","cita para el pap, mamografia etc.", "check ups, mammography","Follow-ups, dentist, mammogram",
            "Mammogram.  It was re scheduled and completed two months late.")

virtual <- c("Did a doctors appointment on the computer", "going in to get a lump checked out...did telehealth instead. was told to wait til pandemic ended to get it thoroughly examined")

# replace each values in Q13 with its category

df_Q13$Q13cat <- ifelse(df_Q13$Q13 %in% c(general),1,
                          ifelse(df_Q13$Q13 %in% c(specialty),2,
                                 ifelse(df_Q13$Q13 %in% c(scan),3,
                                        ifelse(df_Q13$Q13 %in% c(surgery),4,
                                               ifelse(df_Q13$Q13 %in% c(cancer),5,
                                                      ifelse(df_Q13$Q13 %in% c(virtual),6,-1))))))


## drop Q13
drop_Q13 <- c("Q13")
df_Q13 <- df_Q13[,!(names(df_Q13) %in% drop_Q13)]


#save the dataset
write_sav(df_Q13,"Data/Q_13/df_Q13cat.sav")
