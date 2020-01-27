#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
#Install Tidyverse to ease removing duplicates and other features
install.packages("tidyverse")
#Add Library that enable removing duplicates
library(tidyverse)
library(magrittr)
#Set the working Directory to RVeraAuto folder on G:Drive
setwd("C:\Users\travi\OneDrive\Documents\Data Science\My R Workspace\R Projects\RVera Automation")

#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
#print(now)

# Tell R to read the main services data report as well as other more 
# focused report files from the directory.
original_main_df <- read.csv("VeraMonthlyTest.csv")
# alternatively 
#original_main_df <- read.csv("VeraMonthlyTest.csv", na.strings=c(""," ","NA") )
activities_df <- read.csv("VeraActivitiesTest.csv")
HasAttny_df <- read.csv("VeraAlertHasAttorneyTest.csv")
Litig_df <- read.csv("LitigationTest.csv")
main_df <- original_main_df
main_df2 <- main_df
#View(main_df)
#str(main_df2)

#Remove duplicates from MainReport and store the value as 'main_df2'
dim(main_df2)
main_df2 <- main_df2[!duplicated(main_df$Matter.Case.ID), ]
dim(main_df2)
# View(MainUniq)

#Remove duplicates from Activities Report and store the value as 'ActUniq'
dim(activities_df)
activities_uniq_df <- activities_df[!duplicated(activities_df$Case.ID, activities_df$Item ), ]
dim(activities_uniq_df)
#View(ActUniq)

# STORE DATES AS DATES
#Store activity and followup dates as actual dates 
activities_uniq_df$Activity.Date <-as.Date(activities_uniq_df$Activity.Date, format = "%m/%d/%Y")
class(activities_uniq_df$Activity.Date)

main_df2$date_first_followup <-as.Date(main_df2$date_first_followup, format = "%m/%d/%Y")
class(main_df2$date_first_followup)

#Not necessary to remove duplicates from 1) HasAttny data set NOR 2) litigations

#GETTING TO KNOW THE DATA
#List all existing variables to see what they are and if any are unnecessary
ls()

# Class of each variable to see if they are the same type that could help identify 
# issues. One I noticed was the a_num variable was a list as opposed to the ra_num  
#which was a dataframe so I converted it. IDK if that did anything b.c same results.
class(main_df2$a_number)
class(activities_uniq_df)
class(main_df2)
class(now)


#USING REPORTING A# WHENEVER IT IS PRESENT AND A# WHEN THERE ISN'T A REPORTING A#
# First Remove hyphens from a number fields and store the column as integers

main_df2$Reporting.A.Number <- as.integer( gsub("-", "", main_df2$Reporting.A.Number))
main_df2$a_number <- as.integer( gsub("-", "", main_df2$a_number))

#Do same hyphen process for the activities dataset and the case alerts dataset!
activities_uniq_df$Reporting.A.Number <- as.integer( gsub("-", "", activities_uniq_df$Reporting.A.Number))
activities_uniq_df$a_number <- as.integer( gsub("-", "", activities_uniq_df$a_number))

HasAttny_df$Reporting.A.Number <- as.integer( gsub("-", "", HasAttny_df$Reporting.A.Number))
HasAttny_df$a_number <- as.integer( gsub("-", "", HasAttny_df$a_number))

#Use Coalesce to create a new column that contains the NEW A NUMBERS 
main_df2$new_a_number <- dplyr::coalesce(main_df2$Reporting.A.Number, main_df2$a_number)
#view(head(main_df2$new_a_number, 30))

activities_uniq_df$new_a_number <- dplyr::coalesce(activities_uniq_df$Reporting.A.Number, activities_uniq_df$a_number)
#view(head(activities_uniq_df$new_a_number, 30))

HasAttny_df$new_a_number <- dplyr::coalesce(HasAttny_df$Reporting.A.Number, HasAttny_df$a_number)
#view(head(HasAttny_df$new_a_number, 30))


#Step 5 h-j.  Rename "Sponsor Intake (FollowUp)" as "FollowUp"; Rename "Released Referral- Letter to child" as "Gave NGO Tel #s"
activities_uniq_df$Item <- as.character( gsub("Sponsor Intake (Follow Up)", "Follow Up", activities_uniq_df$Item))
activities_uniq_df$Item <- as.character( gsub("Released Referral- Letter to child", "Gave NGO Tel #s", activities_uniq_df$Item))

#Re-Sort data by Activity Item, Activity Date oldest on top, CaseID lowest on top, then A# lowest 
#on top as well.
# Re-Sort dataframe by Activity Item (ascending), Activity Date (ascending), CaseID(ascending), and A# (ascending).
# if you wanted to sort with descending just add a "-" (minus) in front of the data frame name no space
#view(head(activities_uniq_df, 50))
activities_uniq_df <- activities_uniq_df[order(activities_uniq_df$Item, activities_uniq_df$Activity.Date,
                                            activities_uniq_df$Case.ID, activities_uniq_df$new_a_number),]
#view(head(activities_uniq_df, 50))

# FORMAT THE DATE to American style baby! 
# ***Important for this Formatting to happen AFTER its already been sorted***
# because the sort order for dates depends on which format you chose, doesn't default to chronological sorting #DUMB
activities_uniq_df$Activity.Date <- format(activities_uniq_df$Activity.Date, "%m/%d/%Y")
#view(head(activities_uniq_df, 50))

# SUBSETTING THE ACTIVITIES in order to import the dates into req'd fields.

#Create a subset of the main activities_uniq dataframe that only contains "Follow Up" activities. 
#Note this is intentionally happening after Sponsor Follow Ups were renamed as Follow Ups.
FollowUpSub <- activities_uniq_df[activities_uniq_df$Item == "Follow Up",]

#Create a subset of the main activities_uniq dataframe that only contains "COVCOA" activities. 
COVCOASub <- activities_uniq_df[activities_uniq_df$Item == "COV / COA Assistance",]

#Create a subset of the main activities_uniq dataframe that only contains "Gave NGO Tel #s" activities.
#Note this is intentionally happening after Letters to Child were renamed as Gave NGOs.
NGOListSub <- activities_uniq_df[activities_uniq_df$Item == "Gave NGO Tel #s",]

#Create a subset of the main activities_uniq dataframe that only contains "LOPC Referral" activities. 
LOPCSub <- activities_uniq_df[activities_uniq_df$Item == "LOPC Referral",]

#Create a subset of the main activities_uniq dataframe that only contains "Referral Other NGO" activities. 
OtherNGOSub <- activities_uniq_df[activities_uniq_df$Item == "Released Referral- Other NGO",]

#Create a subset of the main activities_uniq dataframe that only contains "Referral Private" activities. 
RefPrivAttySub <- activities_uniq_df[activities_uniq_df$Item == "Released Referral- Private Attorney",]

#Create a subset of the main activities_uniq dataframe that only contains "Referral UCORD" activities. 
RefUCORDSub <- activities_uniq_df[activities_uniq_df$Item == "Released Referral- UCORD",]

#Create a subset of the main activities_uniq dataframe that only contains "Young Center" activities. 
RefYoungCSub <- activities_uniq_df[activities_uniq_df$Item == "Young Center Referral",]

#view(tail(NGOListSub))

### PULL ACTIVITY DATES from Activities dataset into Main dataset based on Case ID#.

#FOLLOW UPS

#1st do a Precheck of what the values look like before importing
main_df2[c(35:39, 7001), c(27, 104)]
names(main_df2[c(27, 104)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, FollowUpSub[ , c("Item", "Activity.Date", "Case.ID")],
                        by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and followup dates because the merge causes date to go back to factor
main_df2$Activity.Date.x <-as.Date(main_df2$Activity.Date.x, format = "%m/%d/%Y")
class(main_df2$Activity.Date.x)

main_df2$date_first_followup <-as.Date(main_df2$date_first_followup, format = "%m/%d/%Y")
class(main_df2$date_first_followup)

#class(main_df2$date_first_followup) <- "Date"



#Finally do the actual replacing process
main_df2$date_first_followup <- ifelse(is.na(main_df2$date_first_followup), main_df2$Activity.Date.x, 
                                             ifelse(main_df2$date_first_followup > main_df2$Activity.Date.x, 
                                                    main_df2$Activity.Date.x, main_df2$date_first_followup )
)
#The above step messes with followup date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_first_followup) <- "Date"

#Confirm successful replacements
main_df2[c(35:39, 7001), c(27, 104)]
names(main_df2[c(27, 104)])

dim(main_df)
dim(main_df2)
names(main_df2[100:119])

# REMOVE additional columns created during the subset merge for FollowUps
main_df2 <- subset(main_df2, select = -c(Activity.Date.y, Activity.Date.x, Item))

#FOLLOWUPS Replacement Complete!! Now COV/COA's


# COV/COA's
#---

### PULL ACTIVITY DATES from Activities dataset into Main dataset based on Case ID#.

#COV/COA's
#1st do a Precheck of what the values look like before importing
which( colnames(main_df2) == "cov_coa_date")

main_df2[c(35:39, 7001), c(32, 33, 116)]
names(main_df2[c(32, 33, 116)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, COVCOASub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

names(main_df2[100:118])
#3rd Re-Format the activity and COV/COA dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$cov_coa_date <-as.Date(main_df2$cov_coa_date, format = "%m/%d/%Y")
class(main_df2$cov_coa_date)

#class(main_df2$date_first_followup) <- "Date"


#Finally do the actual replacing process
#It looks like NA values are white space, and there's a handful of "Facilitated Through Court Assistance" records. Not sure if either of those matter!

#  This version handles the white space and other records by preserving the Detained UC value and setting the other original values to NA:
##Tried it on 1.2.20 and it works with caveats. When I do it manually I get 742 COV/COA's total, including the 352 Formerly Detained.
##When I do it with R I get 376 COV/COA's total. No Facilitated through court assistances and 352 formerly detained.
##The Detained COV's still appear but the date get's wiped for most of them. Will rerun and investigate tomorrow.
main_df2 <- main_df2 %>%
  
  mutate(cov_coa = as.character(cov_coa)) %>%
  
  mutate(
    cov_coa_date = if_else(
      is.na(cov_coa_date),
      Activity.Date,
      
      if_else(
        !is.na(Activity.Date) & cov_coa_date > Activity.Date,
        Activity.Date,
        cov_coa_date
      )
    ),
    
    cov_coa = if_else(
      cov_coa == "Pro Se Assistance (Detained UC)",
      cov_coa,
      
      if_else(
        !is.na(cov_coa_date) &
          cov_coa != "Pro Se Assistance (Detained UC)",
        "Pro Se Assistance (Formerly Detained UC)",
        as.character(NA)
      )
    )
  )

#Confirm successful replacements
which( colnames(main_df2) == "cov_coa_date")
which( colnames(main_df2) == "cov_coa")
which( colnames(main_df2) == "Activity.Date")
main_df2[c(30:45, 7001), c(32:33, 117, 118)]
names(main_df2[c(31:33, 117:118)])

dim(main_df)
dim(main_df2)
names(main_df2[100:118])

# REMOVE additional columns created during the subset merge for COV/COA's
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))


#COV/COAs  Replacement Complete!! Now Gave NGO Lists.

#***NGO LISTS***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_list")
which(colnames(main_df2) == "client_fname")

main_df2[c(200:220, 7010), c(13, 103)]
names(main_df2[c(13, 103)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, NGOListSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and NGO List dates because the merge causes date to go back to factor
main_df2$Activity.Date.x <-as.Date(main_df2$Activity.Date.x, format = "%m/%d/%Y")
class(main_df2$Activity.Date.x)

main_df2$date_pr_referral_list <-as.Date(main_df2$date_pr_referral_list, format = "%m/%d/%Y")
class(main_df2$date_pr_referral_list)

#class(main_df2$date_pr_referral_list) <- "Date"



#Finally do the actual replacing process
main_df2$date_pr_referral_list <- main_df2$Activity.Date

#The above step messes with referral list date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_pr_referral_list) <- "Date"

#Confirm successful replacements
main_df2[c(200:220, 7010), c(13, 103)]
names(main_df2[c(13, 103)])

dim(main_df)
dim(main_df2)
names(main_df2[100:118])

# REMOVE additional columns created during the subset merge for NGO Lists
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('Gave NGO List Replacement Complete!! Now LOPC Referrals.')
#

#***LOPC Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_lopc")
which(colnames(main_df2) == "client_fname")

main_df2[c(150:180, 7010), c(13, 102)]
names(main_df2[c(13, 102)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, LOPCSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and LOPC Referral dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_pr_lopc <-as.Date(main_df2$date_pr_lopc, format = "%m/%d/%Y")
class(main_df2$date_pr_lopc)

#class(main_df2$date_pr_lopc) <- "Date"



#Finally do the actual replacing process
main_df2$date_pr_lopc <- main_df2$Activity.Date

#The above step messes with followup date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_pr_lopc) <- "Date"

#Confirm successful replacements
main_df2[c(150:180, 7010), c(13, 102)]
names(main_df2[c(13, 102)])

dim(main_df)
dim(main_df2)
names(main_df2[100:118])

# REMOVE additional columns created during the subset merge for LOPC
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('LOPC Referral Replacement Complete!! Now OtherNGO Referrals')


#***OtherNGO Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_other_ngo")
which(colnames(main_df2) == "client_fname")

main_df2[c(900:920, 7010), c(13, 100)]
names(main_df2[c(13, 100)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, OtherNGOSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and OtherNGO Referral dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_pr_referral_other_ngo <-as.Date(main_df2$date_pr_referral_other_ngo, format = "%m/%d/%Y")
class(main_df2$date_pr_referral_other_ngo)

#class(main_df2$date_pr_referral_other_ngo) <- "Date"



#Finally do the actual replacing process
main_df2$date_pr_referral_other_ngo <- main_df2$Activity.Date

#The above step messes with OtherNGO date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_pr_referral_other_ngo) <- "Date"

#Confirm successful replacements
which(main_df2$date_pr_referral_other_ngo == '2019-02-11')

main_df2[c(900:920, 7010), c(13, 100)]
names(main_df2[c(13, 100)])

dim(main_df)
dim(main_df2)
names(main_df2[99:118])

# REMOVE additional columns created during the subset merge for OtherNGOs
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('OtherNGO Referral Import Complete!! Now Private Referrals')


#***Private Atty Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_pvr")
which(colnames(main_df2) == "client_fname")

main_df2[c(1:12, 7010), c(13, 101)]
names(main_df2[c(13, 101)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, RefPrivAttySub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and Private Atty Referral dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_pr_referral_pvr <-as.Date(main_df2$date_pr_referral_pvr, format = "%m/%d/%Y")
class(main_df2$date_pr_referral_pvr)

#class(main_df2$date_pr_referral_pvr) <- "Date"


#Finally do the actual replacing process
main_df2$date_pr_referral_pvr <- main_df2$Activity.Date

#The above step messes with Private Referral date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_pr_referral_pvr) <- "Date"

#Confirm successful replacements
#which(main_df2$date_pr_referral_pvr ==!is.null(main_df2$date_pr_referral_pvr))

main_df2[c(1:12, 7010), c(13, 101)]
names(main_df2[c(13, 101)])

dim(main_df)
dim(main_df2)
names(main_df2[99:118])

# REMOVE additional columns created during the subset merge for Private Referrals
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_pr_referral_pvr)

print('Private Attorney Referral Import Complete!! Now UCORD Referrals')


#***UCORD Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_vera")
which(colnames(main_df2) == "client_fname")

main_df2[c(230:250, 7010), c(13, 99)]
names(main_df2[c(13, 99)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, RefUCORDSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and UCORD Referral dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_pr_referral_vera <-as.Date(main_df2$date_pr_referral_vera, format = "%m/%d/%Y")
class(main_df2$date_pr_referral_vera)

#class(main_df2$date_pr_referral_vera) <- "Date"


#Finally do the actual replacing process
main_df2$date_pr_referral_vera <- main_df2$Activity.Date

#The above step messes with UCORD Referral date format again so may have to re-class it. 
class(main_df2$date_pr_referral_vera) <- "Date"

#Confirm successful replacements

main_df2[c(230:250, 7010), c(13, 99)]
names(main_df2[c(13, 99)])

dim(main_df)
dim(main_df2)
names(main_df2[99:118])

# REMOVE additional columns created during the subset merge for UCORD
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_pr_referral_vera)

print('UCORD Referral Import Complete!! Last but not least... Child Advocate (Young Center) Referrals')


#***Young Center Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_childadvocate")
which(colnames(main_df2) == "client_fname")

main_df2[c(230:250, 7010), c(13, 98)]
names(main_df2[c(13, 98)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, RefYoungCSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and Young Center Referral dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_childadvocate <-as.Date(main_df2$date_childadvocate, format = "%m/%d/%Y")
class(main_df2$date_childadvocate)

#class(main_df2$date_childadvocate) <- "Date"


#Finally do the actual replacing process
main_df2$date_childadvocate <- main_df2$Activity.Date

#The above step messes with Young Center Referral date format again so may have to re-class it. 
class(main_df2$date_childadvocate) <- "Date"

#Confirm successful replacements

main_df2[c(230:250, 7010), c(13, 98)]
names(main_df2[c(13, 98)])

dim(main_df)
dim(main_df2)
names(main_df2[97:118])

# REMOVE additional columns created during the subset merge for Young Center
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_childadvocate)

print('Young Center Referral Import Complete!! ALL DATE IMPORTS COMPLETE!!!!!!!')

#### Correct the Shelter names. Remove Deactivated phrase, remove the star from Harlingen ####
main_df2$ult_facility <- as.factor( gsub(" (deactivated TS 1.4.17)", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub(" TS 1.4.17", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("*", "", main_df2$ult_facility))
class(main_df2$ult_facility)
summary(main_df2$ult_facility)

#Insert today's date into "record modified" field"
class(main_df2$record_modified)
summary(main_df2$record_modified)
main_df2$record_modified <- Sys.Date()

#Remove the 5 prefixe-labels text strings from aka names. "Alias: ", "Former Name: ", "Incorrect NTA Name: " "Real Name: ", "Maiden Name: "
main_df2$aka1 <- as.factor( gsub("Alias: ", "", main_df2$aka1))
main_df2$aka1 <- as.factor( gsub("Former Name: ", "", main_df2$aka1))
main_df2$aka1 <- as.factor( gsub("Incorrect NTA Name: ", "", main_df2$aka1))
main_df2$aka1 <- as.factor( gsub("Real Name: ", "", main_df2$aka1))
main_df2$aka1 <- as.factor( gsub("Maiden Name: ", "", main_df2$aka1))
main_df2$aka1 <- as.factor( gsub(" ,", ",", main_df2$aka1))
summary(main_df2$aka1)
#The are some "(other)"s that I don't know what they mean or where they came from.

#Gender Syntax is only M or F or OTHER
main_df2$gender <- as.factor( gsub("emale", "", main_df2$gender))
main_df2$gender <- as.factor( gsub("ale", "", main_df2$gender))
main_df2$gender <- as.factor( gsub("Not Listed", "OTHER", main_df2$gender))
class(main_df2$gender) 
summary(main_df2$gender)
# Abandoning trying to change blank gender values to NA since not important

#Code to erase est_age whenever DOB is NOT NULL
main_df2$dob <- as.Date(main_df2$dob, format = "%m/%d/%Y")
class(main_df2$dob)
summary(main_df2$dob)
summary(main_df2$est_age)

main_df2 <- main_df2 %>%
  
  mutate(est_age = as.integer(est_age)) %>%
  
  mutate(
    est_age = ifelse(
      is.na(dob) == TRUE, 
      est_age,
      NA)
  )

summary(main_df2$est_age)
#Erasing irrelevant est_age's complete

#Erase second language if it's the same as the first
class(main_df2$language1)
class(main_df2$language2)
summary(main_df2$language2)

main_df2 <- main_df2 %>%
  
  mutate(language2 = as.character(language2)) %>%
  
  mutate(
    language2 = ifelse(
      !is.na(language1) & !is.na(language2) & language1 == language2, 
      "", 
      language2)
  )

str(main_df2$language2)
# Done! 1st and 2nd language are not the same

#Store delinquency_system values as true or false
class(main_df2$delinquency_system)
summary(main_df2$delinquency_system)


#Store delinquency_system values as true or false
main_df2$delinquency_system <- as.character( gsub("Yes","TRUE", main_df2$delinquency_system))
main_df2$delinquency_system <- as.character( gsub("No","FALSE", main_df2$delinquency_system))
main_df2$delinquency_system <- as.character( gsub(" ","FALSE", main_df2$delinquency_system))

summary(as.factor(main_df2$delinquency_system))

#Delinquiency field updated

#### CREATE FIELDS TO MEASURE TIME BETWEEN FACILITY ENTRY AND SERVICE PROVIDED #####

#Create a field that measures days between Arrival and KYR
main_df2$daysTilKYR <- ifelse(!is.na(main_df2$date_first_KYR) & !is.na(main_df2$Facility.DOE...For.data.integrity.only),
as.integer(as.Date(main_df2$date_first_KYR, format = "%m/%d/%Y") - as.Date(main_df2$Facility.DOE...For.data.integrity.only, format = "%m/%d/%Y")),
NA)

class(main_df2$daysTilKYR)
summary(main_df2$daysTilKYR)

#Create a field that measures days between Arrival and SCREENINGS!
class(main_df2$daysTilScreen)
summary(main_df2$daysTilScreen)

main_df2$daysTilScreen <-
  ifelse(
    !is.na(main_df2$date_first_indiv) &
      !is.na(main_df2$Facility.DOE...For.data.integrity.only),
    as.integer(
      as.Date(main_df2$date_first_indiv, format = "%m/%d/%Y") - as.Date(main_df2$Facility.DOE...For.data.integrity.only, format = "%m/%d/%Y")
    ),
    NA
  )

# Whenever daysTilKYR or daysTilScreen are greater than 10 AAAAANNNDDD the corresponding note 
# field is empty insert string 'Limited resources'
#Starting with daysTilKYR
summary(as.factor(main_df2$NoKYR_10days))

main_df2 <- main_df2 %>%
  
  mutate(NoKYR_10days = as.character(NoKYR_10days)) %>%
  
  mutate(
    NoKYR_10days = ifelse(
      NoKYR_10days == "" & daysTilKYR > 10, 
      "Limited resources", 
      NoKYR_10days)
  )
summary(as.factor(main_df2$NoKYR_10days))

#Erase all NA's from NoKYR_10Days

main_df2$NoKYR_10days <- ifelse( is.na(main_df2$NoKYR_10days),
                                 "",
                                 main_df2$NoKYR_10days)
#Now handling with daysTilScreen
summary(as.factor(main_df2$NoScreening_10days))

main_df2 <- main_df2 %>%
  
  mutate(NoScreening_10days = as.character(NoScreening_10days)) %>%
  
  mutate(
    NoScreening_10days = ifelse(
      NoScreening_10days == "" & daysTilScreen > 10, 
      'Limited resources', 
      NoScreening_10days)
  )
summary(as.factor(main_df2$NoScreening_10days))

#Erase all NA's from NoScreening_10Days

main_df2$NoScreening_10days <- ifelse( is.na(main_df2$NoScreening_10days),
                                 "",
                                 main_df2$NoScreening_10days)
# not sure if this is actually working
#Erase Follow Up Date if no Intake Date OR if Intake Date is after Follow Up Date 
summary(main_df2$date_first_followup)
class(main_df2$date_first_followup)
class(main_df2$date_first_followup) <- "Date"

main_df2 <- main_df2 %>%
  
  mutate(date_first_followup = as.Date(date_first_followup, format = "%m/%d/%Y")) %>%
  
  mutate(
    date_first_followup = ifelse(
      date_first_indiv == "" | as.numeric(date_first_followup) < as.numeric(date_first_indiv), 
      NA, 
      date_first_followup)
  )
class(main_df2$date_first_followup)
class(main_df2$date_first_followup) <- "Date"
summary(main_df2$date_first_followup)

# It worked! Erased all non-logical followups 


# If screening date is null, potential relief should also be null.
# 
class(main_df2$pot_rel1)
summary(main_df2$pot_rel1)

main_df2 <- main_df2 %>%
  
  mutate(pot_rel1 = as.character(pot_rel1)) %>%
  
  mutate(
    pot_rel1 = ifelse(
      date_first_indiv == "", 
      "", 
      pot_rel1)
  )

class(main_df2$pot_rel1) <- "Factor"
summary(main_df2$pot_rel1)
plot(main_df2$pot_rel1)


# Change all "Awaiting Attorney Review"s to "PENDING"
main_df2$pot_rel1 <- as.character(gsub("Awaiting Attorney Review", "PENDING", main_df2$pot_rel1))  


summary(main_df2$pot_rel1)

# If screening date is NOT null, but potential relief is, store pot_rel as PENDING
main_df2 <- main_df2 %>%
  
  mutate(pot_rel1 = as.character(pot_rel1)) %>%
  
  mutate(
    pot_rel1 = ifelse(
      date_first_indiv != "" & pot_rel1 == "", 
      "PENDING", 
      pot_rel1)
  )

#str(main_df2)
length(main_df2)

#### Change Checkbox values to True/False ####
#Change Values to FALSE/TRUE for in_house	
summary(main_df2$in_house)
class(main_df2$in_house)

main_df2 <- main_df2 %>%
  
  mutate(in_house = as.logical(in_house)) %>%
  
  mutate(
    in_house = ifelse(
      is.na(in_house), 
      FALSE, 
      in_house)
  )
class(main_df2$in_house)
summary(main_df2$in_house)

#Change Values to FALSE/TRUE for withdraw_in_house OR whatever the neew field is called
#
#	
#Change Values to FALSE/TRUE for rep_vol_ngo
summary(main_df2$rep_vol_ngo)
class(main_df2$rep_vol_ngo)

main_df2 <- main_df2 %>%
  
  mutate(rep_vol_ngo = as.logical(rep_vol_ngo)) %>%
  
  mutate(
    rep_vol_ngo = ifelse(
      is.na(rep_vol_ngo), 
      FALSE, 
      rep_vol_ngo)
  )
class(main_df2$rep_vol_ngo)
summary(main_df2$rep_vol_ngo)

#Change Values to FALSE/TRUE for mentor_vol_rep
summary(main_df2$mentor_vol_rep)
class(main_df2$mentor_vol_rep)

main_df2 <- main_df2 %>%
  
  mutate(mentor_vol_rep = as.logical(mentor_vol_rep)) %>%
  
  mutate(
    mentor_vol_rep = ifelse(
      is.na(mentor_vol_rep), 
      FALSE, 
      mentor_vol_rep)
  )
class(main_df2$mentor_vol_rep)
summary(main_df2$mentor_vol_rep)

#Change Values to FALSE/TRUE for withdraw_vol_ngo	
summary(main_df2$withdraw_vol_ngo)
class(main_df2$withdraw_vol_ngo)

main_df2 <- main_df2 %>%
  
  mutate(withdraw_vol_ngo = as.logical(withdraw_vol_ngo)) %>%
  
  mutate(
    withdraw_vol_ngo = ifelse(
      is.na(withdraw_vol_ngo), 
      FALSE, 
      withdraw_vol_ngo)
  )
class(main_df2$withdraw_vol_ngo)
summary(main_df2$withdraw_vol_ngo)


#Change Values to FALSE/TRUE for rep_vol_priv
summary(main_df2$rep_vol_priv)
class(main_df2$rep_vol_priv)

main_df2 <- main_df2 %>%
  
  mutate(rep_vol_priv = as.logical(rep_vol_priv)) %>%
  
  mutate(
    rep_vol_priv = ifelse(
      is.na(rep_vol_priv), 
      FALSE, 
      rep_vol_priv)
  )
class(main_df2$rep_vol_priv)
summary(main_df2$rep_vol_priv)

#Change Values to FALSE/TRUE for mentor_vol_priv
summary(main_df2$mentor_vol_priv)
class(main_df2$mentor_vol_priv)

main_df2 <- main_df2 %>%
  
  mutate(mentor_vol_priv = as.logical(mentor_vol_priv)) %>%
  
  mutate(
    mentor_vol_priv = ifelse(
      is.na(mentor_vol_priv), 
      FALSE, 
      mentor_vol_priv)
  )
class(main_df2$mentor_vol_priv)
summary(main_df2$mentor_vol_priv)


#Change Values to FALSE/TRUE for withdraw_vol_priv
summary(main_df2$withdraw_vol_priv)
class(main_df2$withdraw_vol_priv)

main_df2 <- main_df2 %>%
  
  mutate(withdraw_vol_priv = as.logical(withdraw_vol_priv)) %>%
  
  mutate(
    withdraw_vol_priv = ifelse(
      is.na(withdraw_vol_priv), 
      FALSE, 
      withdraw_vol_priv)
  )
class(main_df2$withdraw_vol_priv)
summary(main_df2$withdraw_vol_priv)

#8 More Fields!! counsel_acpt_date_VTC	date_vol_ngo_VTC	date_vol_priv_VTC	VTC_courtass_1	VTC_courtass_2	VTC_courtass_3	VTC_courtass_4	VTC_courtass_5
#counsel_acpt_date_VTC
main_df2 <- main_df2 %>%
  
  mutate(counsel_acpt_date_VTC = as.logical(counsel_acpt_date_VTC)) %>%
  
  mutate(counsel_acpt_date_VTC = ifelse(is.na(counsel_acpt_date_VTC),
                                        FALSE,
                                        counsel_acpt_date_VTC))
summary(as.factor(main_df2$counsel_acpt_date_VTC))


#date_vol_ngo_VTC
main_df2 <- main_df2 %>%
  
  mutate(date_vol_ngo_VTC = as.logical(date_vol_ngo_VTC)) %>%
  
  mutate(date_vol_ngo_VTC = ifelse(is.na(date_vol_ngo_VTC),
                                   FALSE,
                                   date_vol_ngo_VTC))

summary(as.factor(main_df2$date_vol_ngo_VTC))

#date_vol_priv_VTC
main_df2 <- main_df2 %>%
  
  mutate(date_vol_priv_VTC = as.logical(date_vol_priv_VTC)) %>%
  
  mutate(date_vol_priv_VTC = ifelse(is.na(date_vol_priv_VTC),
                                        FALSE,
                                        date_vol_priv_VTC))
summary(as.factor(main_df2$date_vol_priv_VTC))

#VTC_courtass_1
main_df2 <- main_df2 %>%
  
  mutate(VTC_courtass_1 = as.logical(VTC_courtass_1)) %>%
  
  mutate(VTC_courtass_1 = ifelse(is.na(VTC_courtass_1),
                                        FALSE,
                                        VTC_courtass_1))
#class(main_df2$VTC_courtass_1)
summary(as.factor(main_df2$VTC_courtass_1))

#VTC_courtass_2
main_df2 <- main_df2 %>%
  
  mutate(VTC_courtass_2 = as.logical(VTC_courtass_2)) %>%
  
  mutate(VTC_courtass_2 = ifelse(is.na(VTC_courtass_2),
                                        FALSE,
                                        VTC_courtass_2))
summary(as.factor(main_df2$VTC_courtass_2))


#VTC_courtass_3
main_df2 <- main_df2 %>%
  
  mutate(VTC_courtass_3 = as.logical(VTC_courtass_3)) %>%
  
  mutate(VTC_courtass_3 = ifelse(is.na(VTC_courtass_3),
                                        FALSE,
                                        VTC_courtass_3))
summary(as.factor(main_df2$VTC_courtass_3))


#VTC_courtass_4
main_df2 <- main_df2 %>%
  
  mutate(VTC_courtass_4 = as.logical(VTC_courtass_4)) %>%
  
  mutate(VTC_courtass_4 = ifelse(is.na(VTC_courtass_4),
                                        FALSE,
                                        VTC_courtass_4))
summary(as.factor(main_df2$VTC_courtass_4))


#VTC_courtass_5

main_df2 <- main_df2 %>%
  
  mutate(VTC_courtass_5 = as.logical(VTC_courtass_5)) %>%
  
  mutate(VTC_courtass_5 = ifelse(is.na(VTC_courtass_5),
                                        FALSE,
                                        VTC_courtass_5))
summary(as.factor(main_df2$VTC_courtass_5))








#### Erase All Fields that DON'T get reported ####
length(main_df2)
main_df2 <- subset(main_df2, select = -c(Matter.Case.ID.,	Active,	Disposition, a_number,	Reporting.A.Number))

length(main_df2)
main_df2 <- subset(main_df2, select = -c(Facility.DOE...For.data.integrity.only, Potential.relief..kids.attny.assessment.review.,
                                         Activity.Item, Activity.Creator,	Prescreen.User,	Intake.User,
                                         daysTilKYR,	daysTilScreen))
length(main_df2)


# Change all "Harlingen*" values to just "Harlingen
summary(main_df2$jurisdiction)
main_df2$jurisdiction <- as.character(gsub("[*]", "", main_df2$jurisdiction))  
summary(as.factor(main_df2$jurisdiction))

#### Get rid of alll of the inappropriate "none" values ####
## pos_KYR_presenter, pos_screener, venue_in_house, venue_vol_ngo, withdrawrsn_vol_ngo, venue_vol_priv,
## withdrawrsn_vol_priv,  ##

#pos_KYR_presenter
class(main_df2$pos_KYR_presenter)
summary(main_df2$pos_KYR_presenter)
main_df2$pos_KYR_presenter <- gsub("none", "", main_df2$pos_KYR_presenter)
main_df2$pos_KYR_presenter <- gsub("Paralegal", "GENERAL OFFICE CLERK", main_df2$pos_KYR_presenter)
class(main_df2$pos_KYR_presenter)
summary(as.factor(main_df2$pos_KYR_presenter))


#pos_screener
class(main_df2$pos_screener)
summary(main_df2$pos_screener)
main_df2$pos_screener <- gsub("none", "", main_df2$pos_screener)
class(main_df2$pos_screener)
summary(as.factor(main_df2$pos_screener))

#venue_in_house
class(main_df2$venue_in_house)
summary(main_df2$venue_in_house)
main_df2$venue_in_house <- gsub("none", "", main_df2$venue_in_house)
class(main_df2$venue_in_house)
summary(as.factor(main_df2$venue_in_house))

#venue_vol_ngo
class(main_df2$venue_vol_ngo)
summary(main_df2$venue_vol_ngo)
main_df2$venue_vol_ngo <- gsub("none", "", main_df2$venue_vol_ngo)
class(main_df2$venue_vol_ngo)
summary(as.factor(main_df2$venue_vol_ngo))

#withdrawrsn_vol_ngo
class(main_df2$withdrawrsn_vol_ngo)
summary(main_df2$withdrawrsn_vol_ngo)
main_df2$withdrawrsn_vol_ngo <- gsub("none", "", main_df2$withdrawrsn_vol_ngo)
class(main_df2$withdrawrsn_vol_ngo)
summary(as.factor(main_df2$withdrawrsn_vol_ngo))

#venue_vol_priv
class(main_df2$venue_vol_priv)
summary(main_df2$venue_vol_priv)
main_df2$venue_vol_priv <- gsub("none", "", main_df2$venue_vol_priv)
class(main_df2$venue_vol_priv)
summary(as.factor(main_df2$venue_vol_priv))

#withdrawrsn_vol_priv
class(main_df2$withdrawrsn_vol_priv)
summary(main_df2$withdrawrsn_vol_priv)
main_df2$withdrawrsn_vol_priv <- gsub("none", "", main_df2$withdrawrsn_vol_priv)
class(main_df2$withdrawrsn_vol_priv)
summary(as.factor(main_df2$withdrawrsn_vol_priv))

#### Get rid of alll of the inappropriate "n/a" values ####
## date_first_KYR, date_indiv_court_prep, date_hearing1, and date_vol_ngo

#date_first_KYR
class(main_df2$date_first_KYR)
#summary(main_df2$date_first_KYR)
main_df2$date_first_KYR <- gsub("n/a", "", main_df2$date_first_KYR)
main_df2$date_first_KYR <- as.Date(main_df2$date_first_KYR, format = "%m/%d/%Y")
class(main_df2$date_first_KYR)
summary(main_df2$date_first_KYR)

#date_indiv_court_prep
class(main_df2$date_indiv_court_prep)
#summary(main_df2$date_indiv_court_prep)
main_df2$date_indiv_court_prep <- gsub("n/a", "", main_df2$date_indiv_court_prep)
main_df2$date_indiv_court_prep <- as.Date(main_df2$date_indiv_court_prep, format = "%m/%d/%Y")
class(main_df2$date_indiv_court_prep)
summary(main_df2$date_indiv_court_prep)

#date_hearing1
class(main_df2$date_hearing1)
#summary(main_df2$date_hearing1)
main_df2$date_hearing1 <- gsub("n/a", "", main_df2$date_hearing1)
main_df2$date_hearing1 <- as.Date(main_df2$date_hearing1, format = "%m/%d/%Y")
class(main_df2$date_hearing1)
summary(main_df2$date_hearing1)

#date_vol_ngo
class(main_df2$date_vol_ngo)
#summary(main_df2$date_vol_ngo)
main_df2$date_vol_ngo <- gsub("n/a", "", main_df2$date_vol_ngo)
main_df2$date_vol_ngo <- as.Date(main_df2$date_vol_ngo, format = "%m/%d/%Y")
class(main_df2$date_vol_ngo)
summary(main_df2$date_vol_ngo)

#Done taking out 'n/a's!!!


#### Only remaining step is take out NAs and leave blank across multiple fields (dates, chracter, integer, etc.)

#### Testing Zone #Testing Zone #Testing Zone #Testing Zone #Testing Zone #Testing Zone ####

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone

#---------------------------------------------------------------------------------------------------------
which( !is.na(main_df2$date_childadvocate), arr.ind=TRUE)
class(main_df3$filing1_date)
class(main_df3$filing1_date) <- "Date"
class(main_df3$filing2_date) <- "Date"

#df <- df %>% mutate_all(funs(type.convert(as.character(.))))
main_df3 <- main_df3 %>% mutate_all(funs(type.convert(as.character(main_df3$filing1_date))))
str(main_df3)
#Internet solution for writing all NAs as blank mat[is.na(mat)]   <- " "
main_df3 <- main_df2
main_df3[is.na(main_df3)] <- as.character(gsub(NA,"FALSE", main_df3))

summary(main_df2$language1)

#This version preserves every value of cov_coa that doesn't fall within the if_else() parameter:

main_df2 %>%
  
  mutate(cov_coa = as.character(cov_coa)) %>%
  
  mutate(
    cov_coa_date = if_else(
      is.na(date_first_followup),
      Activity.Date,
      
      if_else(
        date_first_followup > Activity.Date,
        Activity.Date,
        date_first_followup
      )
    ),
    
    cov_coa = if_else(
      !is.na(cov_coa_date) &
        cov_coa != "Pro Se Assistance (Detained UC)",
      "Pro Se Assistance (Formerly Detained UC)",
      cov_coa
    )
  )

#It looks like NA values are white space, and there's a handful of "Facilitated Through Court Assistance" records. Not sure if either of those matter!

#  This version handles the white space and other records by preserving the Detained UC value and setting the other original values to NA:

main_df2 <- main_df2 %>%
  
  mutate(cov_coa = as.character(cov_coa)) %>%
  
  mutate(
    cov_coa_date = if_else(
      is.na(date_first_followup),
      Activity.Date,
      
      if_else(
        date_first_followup > Activity.Date,
        Activity.Date,
        date_first_followup
      )
    ),
    
    cov_coa = if_else(
      cov_coa == "Pro Se Assistance (Detained UC)",
      cov_coa,
      
      if_else(
        !is.na(cov_coa_date) &
          cov_coa != "Pro Se Assistance (Detained UC)",
        "Pro Se Assistance (Formerly Detained UC)",
        as.character(NA)
      )
    )
  )




#Reorganizing the if statement to change 2 columns?

if (is.na(main_df2$cov_coa_date)) {
  and(main_df2$cov_coa_date <-  main_df2$Activity.Date, main_df2$cov_coa <-  "Pro Se Assistance (Formerly Detained UC)")
  }else{ 
  ifelse(main_df2$cov_coa_date > main_df2$Activity.Date, 
                                       main_df2$Activity.Date, main_df2$cov_coa_date)
}
#
#
#
#I think next step is delete the Activity.Date.x column and any other additonal column then REPEAT for all 
#data subsets.
#
#12.20 different might still try Celia's other suggestion if time allows
testVlookup_df <- testVlookup_df %>%
mutate(date_first_followup = casewhen(is.na(date_first_followup) ~ Activity.Date,
                                      date_first_followup > Activity.Date ~ Activity.Date)

#12.10.19 attempting to ONLY pull activity dates in when the column is NULL OR when the column has a value that has a later date.
testVlookup_df$date_first_followup <- ifelse(is.null(testVlookup_df$date_first_followup) == TRUE,
                                             as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y"),
                                             ifelse(isTRUE(as.Date(testVlookup_df$date_first_followup, format = "%m/%d/%Y") > as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y")) == TRUE,
 
#
#---------------------------------------------------------------------------------------------------------
# FIN

#FINISHING THE DATA AND SPITTING OUT A CSV WITH A TIMESTAMPED TITLE
#View(main_df2)
#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
file_name <- paste0("Vera_Automation_Test", now, ".csv" )
file_name_act <- paste0("Vera_Activities_Test", now, ".csv" )
file_name_vlook <- paste0("Vlook_Test", now, ".csv" )

#print(file_name)

######## Export Data as a csv file #######
write.table(main_df2, sep=",", file_name, row.names = FALSE, )

write.csv(activities_uniq_df, 'activities_file.csv', row.names = FALSE, )
write.csv(testVlookup_df, 'v-lookie-here3.csv', row.names = FALSE, )
