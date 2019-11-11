library(dplyr)
head(sasdata)
dim(sasdata)
hintsdata<-subset(sasdata, select=c(1,2,3))
head(hintsdata)
str(sasdata)
hintsfinal<-hintsdatasc
final<-hintsdatasc
final_withheight<-hintsdatasc
hintsdata<-subset(hints_sas, select=c("SeekHealthInfo", "WhereSeekHealthInfo", "UseInternet", "Electronic_SelfHealthInfo", "Electronic_HealthInfoSE", "Electronic_BuyMedicine", "Electronic_LookedAssistance", "Electronic_TalkDoctor",
                                      "Electronic_TrackedHealthCosts", "Electronic_TestResults","SharedHealthDeviceInfo","TextFromDoctor", "RegularProvider", "MostRecentCheckup2", "FreqGoProvider",
                                      "ChanceAskQuestions", "FeelingsAddressed", "InvolvedDecisions", "UnderstoodNextSteps", "ExplainedClearly", "SpentEnoughTime", "HelpUncertainty",
                                      "QualityCare", "ProbCare_BringTest", "ProbCare_WaitLong", "ProbCare_RedoTest", "ProbCare_ProvideHist", "HealthIns_InsuranceEmp","HealthIns_InsurancePriv",
                                      "HealthIns_Medicare", "HealthIns_Medicaid", "HealthIns_Tricare", "HealthIns_VA", "HealthIns_IHS", "HealthIns_Other",
                                      "ProviderMaintainEMR2", "ConfidentInfoSafe", "EverOfferedAccessRec", "AccessOnlineRecord", "NotAccessed_SpeakDirectly", "NotAccessed_NoInternet", "NotAccessed_NoNeed",
                                      "NotAccessed_ConcernedPrivacy", "NotAccessed_NoRecord", "NotAccessed_Other", "RecordsOnline_RefillMeds", "RecordsOnline_Paperwork", "RecordsOnline_RequestCorrection",
                                      "RecordsOnline_MessageHCP", "RecordsOnline_DownloadHealth", "RecordsOnline_AddHealthInfo", "RecordsOnline_MakeDecision", "RecordsOnline_HealthProbs", "RecordsOnline_Allergies", 
                                      "RecordsOnline_VisitSummary", "RecordsOnline_ClinNotes", "RecordsOnline_Immunizations", "UsefulOnlineMedRec", 
                                      "OwnAbilityTakeCareHealth", "MedConditions_Diabetes", "MedConditions_HighBP", "MedConditions_HeartCondition", "MedConditions_LungDisease", "MedConditions_Arthritis", "MedConditions_Depression","Weight", "TimesModerateExercise",
                                      "HowLongModerateExerciseMinutes", "Age", "OccupationStatus", "Education","SpeakEnglish",
                                      "NotHisp","Mexican", "PuertoRican", "Cuban","MailSurveyTimeMin", "MailSurveyTimeHrs","Height_Feet","Height_Inches")) 


#selecting columns for transforming datatypes


cols <- c("SeekHealthInfo", "WhereSeekHealthInfo", "UseInternet", "Electronic_SelfHealthInfo", "Electronic_HealthInfoSE", "Electronic_BuyMedicine", "Electronic_LookedAssistance", "Electronic_TalkDoctor",
          "Electronic_TrackedHealthCosts", "Electronic_TestResults","SharedHealthDeviceInfo","TextFromDoctor", "RegularProvider", "MostRecentCheckup2", "FreqGoProvider",
          "ChanceAskQuestions", "FeelingsAddressed", "InvolvedDecisions", "UnderstoodNextSteps", "ExplainedClearly", "SpentEnoughTime", "HelpUncertainty",
          "QualityCare", "ProbCare_BringTest", "ProbCare_WaitLong", "ProbCare_RedoTest", "ProbCare_ProvideHist", "HealthIns_InsuranceEmp","HealthIns_InsurancePriv",
          "HealthIns_Medicare", "HealthIns_Medicaid", "HealthIns_Tricare", "HealthIns_VA", "HealthIns_IHS", "HealthIns_Other",
          "ProviderMaintainEMR2", "ConfidentInfoSafe", "EverOfferedAccessRec", "AccessOnlineRecord", "NotAccessed_SpeakDirectly", "NotAccessed_NoInternet", "NotAccessed_NoNeed",
          "NotAccessed_ConcernedPrivacy", "NotAccessed_NoRecord", "NotAccessed_Other", "RecordsOnline_RefillMeds", "RecordsOnline_Paperwork", "RecordsOnline_RequestCorrection",
          "RecordsOnline_MessageHCP", "RecordsOnline_DownloadHealth", "RecordsOnline_AddHealthInfo", "RecordsOnline_MakeDecision", "RecordsOnline_HealthProbs", "RecordsOnline_Allergies", 
          "RecordsOnline_VisitSummary", "RecordsOnline_ClinNotes", "RecordsOnline_Immunizations", "UsefulOnlineMedRec", 
          "OwnAbilityTakeCareHealth", "MedConditions_Diabetes", "MedConditions_HighBP", "MedConditions_HeartCondition", "MedConditions_LungDisease", "MedConditions_Arthritis", "MedConditions_Depression", "TimesModerateExercise",
          "OccupationStatus", "Education","SpeakEnglish",
          "NotHisp","Mexican", "PuertoRican", "Cuban")


#These are already Numeric
#HowLongModerateExerciseMinutes", "Age","MailSurveyTimeMin", "MailSurveyTimeHrs", "Weight",

#####C4 section is doubtful so we might not require those 7 variables at all#####

# MyData[cols] <- sapply(MyData[cols],as.factor()))
hintsdata[,cols] <-  data.frame(apply(hintsdata[cols],2, as.factor))
str(hintsdata)
dim(hintsdata)
test<-hintsdata[hintsdata$SeekHealthInfo==1,]

#START SCREENING VARIABLES- A1,B1, D1, D4, 
#A1 -SeekHealthInfo Only keep Values with 1 in option
hintsdataA1<- hintsdata[hintsdata$SeekHealthInfo==" 1",]
str(hintsdata$SeekHealthInfo)
#B1 - UseInternet Only keep values with 1 in option
hintsdataA1B1<- hintsdataA1[hintsdataA1$UseInternet==" 1",]

#D1 - ProviderMaintainEMR2 - Only keep values with 1 in option
hintsdataA1B1D1<- hintsdataA1B1[hintsdataA1B1$ProviderMaintainEMR2==" 1",]

# D4 - EverOfferedAccessRec where value is only= 1 which will remove -1 values in D6
hintsdataA1B1D1D4<- hintsdataA1B1D1[hintsdataA1B1D1$EverOfferedAccessRec==" 1",]




#Now that screening has been done, we're left with 1418 observations.
hintsdatasc<- hintsdataA1B1D1D4
table(hintsdatasc$AccessOnlineRecord)
#Remove -9, -2,-1 columns from Access online records since there are only 3 values left in -9 after screening

hintsdatasc<- hintsdatasc[!hintsdatasc$AccessOnlineRecord==-9, ]
hintsdatasc$AccessOnlineRecord <- droplevels(hintsdatasc$AccessOnlineRecord)
table(hintsdatasc$AccessOnlineRecord)

#Condensing the dependent variable into categories
#Category 0 - NEVER - values 0 
#Category 1 - LESS FREQUENT- Merge values 1 and 2
#Category 2 - REGULAR- Merge values 3 and 4

hintsdatasc$AccessOnlineRecord <- ifelse(hintsdatasc$AccessOnlineRecord == 0, "NEVER", ifelse(hintsdatasc$AccessOnlineRecord == 1 | hintsdatasc$AccessOnlineRecord == 2 , "LESSFREQUENT", "FREQUENT" ))

table(hintsdatasc$AccessOnlineRecord)

#Condesing the variable WhereSeekHealthInfo in A2
#Category 1- INTERNET 
#Category 2- OTHERS
#There is only -5 values left which will be put together with Error
hintsdatasc$WhereSeekHealthInfo <- ifelse(hintsdatasc$WhereSeekHealthInfo ==" 7", "INTERNET", "OTHERS")
table(hintsdatasc$WhereSeekHealthInfo)
hintsdatasc$WhereSeekHealthInfo<-as.factor(hintsdatasc$WhereSeekHealthInfo)

###Condense B12- Dont know into NO category
table(hintsdatasc$TextFromDoctor)
#First lets remove -5 and -9 values
hintsdatasc<- hintsdatasc[!hintsdatasc$TextFromDoctor==-5,]
hintsdatasc<- hintsdatasc[!hintsdatasc$TextFromDoctor==-9,]
hintsdatasc$TextFromDoctor<- droplevels(hintsdatasc$TextFromDoctor)
#Now, we condense Dont know category into NO
hintsdatasc$TextFromDoctor<- ifelse(hintsdatasc$TextFromDoctor==" 1","1","2")

#Condense C3 
#Category 1- NEVER = 0 
#Category 2- LESS FREQUENT - 1,2,3,4
#Category 3- FREQUENT - 5,6
#Category 4- -9 values
table(hintsdatasc$FreqGoProvider)
hintsdatasc$FreqGoProvider <- ifelse(hintsdatasc$FreqGoProvider ==" 0", "NEVER" , ifelse(hintsdatasc$FreqGoProvider ==" 1" | hintsdatasc$FreqGoProvider == " 2"
                                                                                         |hintsdatasc$FreqGoProvider == " 3" | hintsdatasc$FreqGoProvider == " 4", "LESSFREQUENT",
                                                                                         ifelse(hintsdatasc$FreqGoProvider == " 5" | hintsdatasc$FreqGoProvider == " 6", "FREQUENT", " -9")))
hintsdatasc<- hintsdatasc[!hintsdatasc$FreqGoProvider==" -9",]
#Removing the -9 values as they are 1% only

#C5 Quality Care
##Scaling and reverse coding done from 1-5##
#CATEGORY 0- NA AND ERROR VALUES
#CATEGORY 1- POOR VALUES
#CATEGORY 2- FAIR VALUES
#CATEGORY 3- GOOD VALUES
#CATEGORY 4- VERY GOOD VALUES
#CATEGORY 5- EXCELLENT VALUES

table(hintsdatasc$QualityCare)
hintsdatasc$QualityCare <- ifelse(hintsdatasc$QualityCare == " 5", 1, ifelse(hintsdatasc$QualityCare == " 4", 2, ifelse(hintsdatasc$QualityCare == " 2", 4, 
                                                                                                                        ifelse(hintsdatasc$QualityCare == " 1", 5,ifelse(hintsdatasc$QualityCare == " 3",3, 0))) ))



###D2-  Scaling and reverse coding done from 1-3  ### confidentinfosafe
table(hintsdatasc$ConfidentInfoSafe)
#Drop the -9 level as it has 0 values and -5 level as it as only 1 value
hintsdatasc<- hintsdatasc[!hintsdatasc$ConfidentInfoSafe==-5,]
hintsdatasc$ConfidentInfoSafe<- droplevels(hintsdatasc$ConfidentInfoSafe)

#CATEGORY 1- NOT CONFIDENT VALUES
#CATEGORY 2- SOMEWHAT CONFIDENT VALUES
#CATEGORY 3 VERY CONFIDENT VALUES
hintsdatasc$ConfidentInfoSafe <- ifelse(hintsdatasc$ConfidentInfoSafe == " 3", 1, ifelse(hintsdatasc$ConfidentInfoSafe == " 2", 2 , ifelse(hintsdatasc$ConfidentInfoSafe ==" 1", 3, 0)))



table(hintsdatasc$EverOfferedAccessRec)
#All are one in this

###G2 Scaling and reverse coding### - OwnAbilityTakeCareHealth

table(hintsdatasc$OwnAbilityTakeCareHealth)
#CATEGORY 0- NA AND ERROR VALUES
#CATEGORY 1- NOT CONFIDENT AT ALL
#CATEGORY 2- A LITTLE CONFIDENT
#CATEGORY 3- SOMEWHAT CONFIDENT
#CATEGORY 4- VERY CONFIDENT
#CATEGORY 5- COMPLETELY CONFIDENT

hintsdatasc$OwnAbilityTakeCareHealth <- ifelse(hintsdatasc$OwnAbilityTakeCareHealth == " 5", 1, ifelse(hintsdatasc$OwnAbilityTakeCareHealth==" 4", 2,
                                                                                                       ifelse(hintsdatasc$OwnAbilityTakeCareHealth==" 3", 3, ifelse(hintsdatasc$OwnAbilityTakeCareHealth==" 2",4, ifelse(hintsdatasc$OwnAbilityTakeCareHealth==" 1",5,0)))))


###O2 create categories for Employed and Non-Employed - OccupationStatus
table(hintsdatasc$OccupationStatus)
#First lets drop -9 and -5 values since there are only 21 values in total and removing them won't have a big affect.
hintsdatasc<- hintsdatasc[!hintsdatasc$OccupationStatus==-5,]
hintsdatasc<- hintsdatasc[!hintsdatasc$OccupationStatus==-9,]
hintsdatasc$OccupationStatus<- droplevels(hintsdatasc$OccupationStatus)

#CATEGORY 1- EMPLOYED
#CATEGORY 2- NON EMPLOYED
hintsdatasc$OccupationStatus<- ifelse(hintsdatasc$OccupationStatus==" 1", 1, 2)


### I1 can be broken into different categories
table(hintsdatasc$TimesModerateExercise)
################Lets first remove the -9 values from the data OR WE CAN REPLACE THOSE VALUES WITH THE MEDIAN############
hintsdatasc<- hintsdatasc[!hintsdatasc$TimesModerateExercise]


## I1 can be broken into categories
#CATEGORY 1- None : 0 days
#CATEGORY 2- Average workout: 1-5 days
#CATEGORY 3 - Daily : 6 and 7 days
hintsdatasc$TimesModerateExercise<- ifelse(hintsdatasc$TimesModerateExercise==" 0",1, ifelse(hintsdatasc$TimesModerateExercise==" 1" |hintsdatasc$TimesModerateExercise==" 2" |
                                                                                               hintsdatasc$TimesModerateExercise==" 3" | hintsdatasc$TimesModerateExercise==" 4" | hintsdatasc$TimesModerateExercise==" 5", 2 , 
                                                                                             ifelse(hintsdatasc$TimesModerateExercise==" 6" | hintsdatasc$TimesModerateExercise==" 7", 3, -9)))





###Height and weight can be used to calculate BMI of the patients
### O9 Scaling and reverse coding - SpeakEnglish
table(hintsdatasc$SpeakEnglish)
########  Remove the -5 level and keeping -9 values for now as there are 25 vales      ########
hintsdatasc$SpeakEnglish<- droplevels(hintsdatasc$SpeakEnglish)
#CATEGORY 1- NOT  AT ALL
#CATEGORY 2- NOT WELL
#CATEGORY 3- WELL
#CATEGORY 4- VERY WELL
hintsdatasc$SpeakEnglish<- ifelse(hintsdatasc$SpeakEnglish==" 4",1, ifelse(hintsdatasc$SpeakEnglish==" 3",2, ifelse(hintsdatasc$SpeakEnglish==" 2",3, ifelse(hintsdatasc$SpeakEnglish==" 1",4, -9))) )


###############TO BE REVIEWED ####################
### O18 MailsurveytimeMin and mailsurveytimeHrs can be combined into 1 ie. mailsurverytimeHrs can be converted into minutes instead
table(hintsdatasc$MailSurveyTimeHrs)
table(hintsdatasc$MailSurveyTimeMin)
#
hintsdatasc$MailSurveyTimeHrsToMins<- ifelse(hintsdatasc$MailSurveyTimeHrs==-4 | hintsdatasc$MailSurveyTimeHrs==-9, -9, ifelse(hintsdatasc$MailSurveyTimeHrs >=0,hintsdatasc$MailSurveyTimeHrs *60, -1  ))
table(hintsdatasc$MailSurveyTimeHrsToMins)

table(hintsdatasc$Electronic_SelfHealthInfo)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_SelfHealthInfo==-9,]
hintsdatasc$Electronic_SelfHealthInfo<- droplevels(hintsdatasc$Electronic_SelfHealthInfo)

table(hintsdatasc$Electronic_HealthInfoSE)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_HealthInfoSE ==-9,]
hintsdatasc$Electronic_HealthInfoSE <- droplevels(hintsdatasc$Electronic_HealthInfoSE)

table(hintsdatasc$Electronic_BuyMedicine)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_BuyMedicine ==-9,]
hintsdatasc$Electronic_BuyMedicine <- droplevels(hintsdatasc$Electronic_BuyMedicine)

table(hintsdatasc$Electronic_LookedAssistance)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_LookedAssistance ==-9,]
hintsdatasc$Electronic_LookedAssistance <- droplevels(hintsdatasc$Electronic_LookedAssistance)

table(hintsdatasc$Electronic_TalkDoctor)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_TalkDoctor ==-9,]
hintsdatasc$Electronic_TalkDoctor <- droplevels(hintsdatasc$Electronic_TalkDoctor)

table(hintsdatasc$Electronic_TrackedHealthCosts)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_TrackedHealthCosts ==-9,]
hintsdatasc$Electronic_TrackedHealthCosts <- droplevels(hintsdatasc$Electronic_TrackedHealthCosts)

table(hintsdatasc$Electronic_TestResults)
hintsdatasc<- hintsdatasc[!hintsdatasc$Electronic_TestResults ==-9,]
hintsdatasc$Electronic_TestResults <- droplevels(hintsdatasc$Electronic_TestResults)

table(hintsdatasc$SharedHealthDeviceInfo)
hintsdatasc<- hintsdatasc[!hintsdatasc$SharedHealthDeviceInfo ==-9,]
hintsdatasc<- hintsdatasc[!hintsdatasc$SharedHealthDeviceInfo ==-5,]
hintsdatasc$SharedHealthDeviceInfo <- droplevels(hintsdatasc$SharedHealthDeviceInfo)

table(hintsdatasc$TextFromDoctor)
hintsdatasc$TextFromDoctor<-as.factor(hintsdatasc$TextFromDoctor)

table(hintsdatasc$RegularProvider)
hintsdatasc<- hintsdatasc[!hintsdatasc$RegularProvider ==-9,]
hintsdatasc$RegularProvider <- droplevels(hintsdatasc$RegularProvider)

table(hintsdatasc$MostRecentCheckup2)
hintsdatasc$MostRecentCheckup2 <- droplevels(hintsdatasc$MostRecentCheckup2)

table(hintsdatasc$FreqGoProvider)
hintsdatasc$TextFromDoctor<-as.numeric(hintsdatasc$TextFromDoctor)

hintsdatasc$TextFromDoctor<-as.factor(hintsdatasc$TextFromDoctor)

table(hintsdatasc$ProviderMaintainEMR2)

hintsdatasc$ConfidentInfoSafe<-as.factor(hintsdatasc$ConfidentInfoSafe)
table(hintsdatasc$ConfidentInfoSafe)
data.frame(t(sapply(hintsdatasc$FreqGoProvider,c)))
hintsdatasc$FreqGoProvider <- as.factor(hintsdatasc$FreqGoProvider)

table(hintsdatasc$EverOfferedAccessRec)

table(hintsdatasc$AccessOnlineRecord)
data.frame(t(sapply(hintsdatasc$AccessOnlineRecord,c)))
hintsdatasc$AccessOnlineRecord <- as.factor(hintsdatasc$AccessOnlineRecord)
str(hintsdatasc)

table(hintsdatasc$OccupationStatus)
hintsdatasc$OccupationStatus <- as.factor(hintsdatasc$OccupationStatus)


table(hintsdatasc$SpeakEnglish)
hintsdatasc$SpeakEnglish <- as.factor(hintsdatasc$SpeakEnglish)
######SPLITTING DATA INTO TRAINING AND TEST######
#################################
#RANDOM FOREST##########################
####################################

nr=nrow(hintsdatasc)
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 70%sample of row-indices
mdTrn=hintsdatasc[trnIndex,]   #training data with the randomly selected row-indices
mdTst = hintsdatasc[-trnIndex,]

rfModel = randomForest(mdTrn$AccessOnlineRecord ~ ., data=mdTrn, ntree=100, importance=TRUE )

#check accuracy
mean(predTrn==mdTrn$AccessOnlineRecord)
mean(predict(rfModel,mdTrn, type="class") == mdTrn$AccessOnlineRecord)


fit=randomForest(churn~., data=data_churn[3:17], ntree=1,
                 importance=TRUE, proximity=TRUE)
conf <- rfModel$confusion
conf
accuracy<-sum(diag(conf))/sum(conf)
accuracy
#0.832
#Variable importance
importance(rfModel)
varImpPlot(rfModel)

