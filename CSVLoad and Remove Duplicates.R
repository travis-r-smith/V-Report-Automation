#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
#Install Tidyverse to ease removing duplicates and other features
install.packages("tidyverse")
#Add Library that enable removing duplicates
library(tidyverse)
library(magrittr)
library(ggplot2)
#Set the working Directory to RVeraAuto folder on G:Drive
#setwd("C:/Users/travi/OneDrive/Documents/Data Science/My R Workspace/R Projects/RVera Automation")
setwd("G:/My Drive/RVeraAuto")

#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
#print(now)

# Tell R TO LOAD THE 3 FILES 
part1_df <- read.csv("serv1VeraTest.csv", na.strings=c("",".","NA"))
#part2_df <- read.csv("serv2VeraTest.csv", na.strings=c("",".","NA"))
part3_df <- read.csv("ActvVeraCumTest.csv", na.strings=c("",".","NA"))
dim(part1_df)
#dim(part2_df)
dim(part3_df)
names(part3_df)
names(part1_df)

# THEN REMOVE ADDITIONAL columns FROM ACTIVITIES FILE. 
part3_df <- subset(part3_df, select = -c(Active, Activity.Creator, Activity.Date))
dim(part3_df)
# THEN COMBINE THE 3 DATASETS with rbind?
#?rbind
megafile_df <- rbind(part1_df, part3_df, make.row.names = TRUE)
class(megafile_df)

# Tell R to read the main services data report as well as other more 
# focused report files from the directory.
#### original_main_df <- read.csv("VeraMonthlyTest.csv") ####
original_main_df <- megafile_df

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

#Remove updated A-Number-duplicates from MainReport and store the value as 'main_df2'
dim(main_df2)
main_df2 <- main_df2[!duplicated(main_df$a_number), ]
dim(main_df2)

#Do same hyphen process for the activities dataset and the case alerts dataset!
names(activities_uniq_df)
activities_uniq_df$Reporting.A.Number <- as.integer( gsub("-", "", activities_uniq_df$Reporting.A.Number))
activities_uniq_df$A.Number <- as.integer( gsub("-", "", activities_uniq_df$A.Number))

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
activities_uniq_df$Activity.Date <-as.Date(activities_uniq_df$Activity.Date, format = "%m/%d/%Y")
#view(head(activities_uniq_df, 50))
class(activities_uniq_df$Activity.Date)
head(activities_uniq_df$Activity.Date)
summary(activities_uniq_df$Activity.Date)
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
which( colnames(main_df2) == "date_first_followup")
which( colnames(main_df2) == "date_first_indiv")
main_df2[c(1556:1561, 1588, 22004:22010, 60000:60010), c(24, 26)]
#names(main_df2[c(26, 100:102)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, FollowUpSub[ , c("Item", "Activity.Date", "Case.ID")],
                        by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

which( colnames(main_df2) == "Activity.Date")
main_df2[c(1556:1561, 1588, 52004:52010, 53035:53040), c(24, 26, 141)]

#3rd Re-Format the activity and followup dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_first_followup <-as.Date(main_df2$date_first_followup, format = "%m/%d/%Y")
class(main_df2$date_first_followup)



#class(main_df2$date_first_followup) <- "Date"



#Finally do the actual replacing process
main_df2$date_first_followup <-
  ifelse(
    is.na(main_df2$date_first_followup) &
      !is.na(main_df2$date_first_indiv),
    main_df2$Activity.Date,
    ifelse(
      main_df2$date_first_followup > main_df2$Activity.Date,
      main_df2$Activity.Date,
      main_df2$date_first_followup
    )
  )

#The above step messes with followup date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_first_followup) <- "Date"

#Confirm successful replacements
main_df2[c(1556:1561, 1588, 52004:52010, 53035:53040), c(24, 26, 141)]

dim(main_df)
dim(main_df2)
names(main_df2[138:141])

# REMOVE additional columns created during the subset merge for FollowUps
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))

#FOLLOWUPS Replacement Complete!! Now COV/COA's


# COV/COA's
#---

### PULL ACTIVITY DATES from Activities dataset into Main dataset based on Case ID#.

#COV/COA's
#1st do a Precheck of what the values look like before importing
which( colnames(main_df2) == "cov_coa_date")

main_df2[c(61130:61145, 62001), c(32:33, 117, 118)]
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
main_df2[c(48130:48145, 62001), c(32:33, 140, 141)]
names(main_df2[c(31:33, 140:141)])

dim(main_df)
dim(main_df2)
names(main_df2[100:118])

# REMOVE additional columns created during the subset merge for COV/COA's
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)


#COV/COAs  Replacement Complete!! Now Gave NGO Lists.

#***NGO LISTS***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_list")
which(colnames(main_df2) == "client_fname")

main_df2[c(61200:61220, 63777), c(12, 105)]
names(main_df2[c(12, 105)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, NGOListSub[ , c("Item", "Activity.Date", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd Re-Format the activity and NGO List dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_pr_referral_list <-as.Date(main_df2$date_pr_referral_list, format = "%m/%d/%Y")
class(main_df2$date_pr_referral_list)

#class(main_df2$date_pr_referral_list) <- "Date"



#Finally do the actual replacing process
main_df2$date_pr_referral_list <- main_df2$Activity.Date

#The above step messes with referral list date format again so may have to re-class it. Question for Celia, does
#the format of dates affect what date is considered less than or greatear than another?
class(main_df2$date_pr_referral_list) <- "Date"

#Confirm successful replacements
main_df2[c(59200:59220, 62777), c(12, 105)]
names(main_df2[c(12, 105)])

dim(main_df)
dim(main_df2)
names(main_df2[130:141])

# REMOVE additional columns created during the subset merge for NGO Lists
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('Gave NGO List Replacement Complete!! Now LOPC Referrals.')
#

#***LOPC Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_lopc")
which(colnames(main_df2) == "client_fname")

main_df2[c(10150:10180, 17010), c(12, 104)]
names(main_df2[c(12, 104)])

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
main_df2[c(10150:10180, 17010), c(12, 104)]
names(main_df2[c(12, 104)])

dim(main_df)
dim(main_df2)
names(main_df2[135:141])

# REMOVE additional columns created during the subset merge for LOPC
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('LOPC Referral Replacement Complete!! Now OtherNGO Referrals')


#***OtherNGO Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_other_ngo")
which(colnames(main_df2) == "client_fname")

main_df2[c(11900:11920, 17010), c(12, 101)]
names(main_df2[c(12, 101)])

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

main_df2[c(11900:11920, 17010), c(12, 101)]
names(main_df2[c(12, 101)])

dim(main_df)
dim(main_df2)
names(main_df2[130:141])

# REMOVE additional columns created during the subset merge for OtherNGOs
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

print('OtherNGO Referral Import Complete!! Now Private Referrals')


#***Private Atty Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_pvr")
which(colnames(main_df2) == "client_fname")

main_df2[c(10001:10012, 57010), c(12, 103)]
names(main_df2[c(12, 103)])

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

main_df2[c(10001:10012, 57010), c(12, 103)]
names(main_df2[c(12, 103)])

dim(main_df)
dim(main_df2)
names(main_df2[135:141])

# REMOVE additional columns created during the subset merge for Private Referrals
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_pr_referral_pvr)

print('Private Attorney Referral Import Complete!! Now UCORD Referrals')


#***UCORD Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_pr_referral_vera")
which(colnames(main_df2) == "client_fname")

main_df2[c(9230:9250, 57010), c(12, 99)]
names(main_df2[c(12, 99)])

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

main_df2[c(9230:9250, 57010), c(12, 99)]
names(main_df2[c(12, 99)])

dim(main_df)
dim(main_df2)

# REMOVE additional columns created during the subset merge for UCORD
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_pr_referral_vera)

print('UCORD Referral Import Complete!! Last but not least... Child Advocate (Young Center) Referrals')


#***Young Center Referrals***

#1st do a Precheck of what the values look like before importing
which(colnames(main_df2) == "date_childadvocate")
which(colnames(main_df2) == "client_fname")

main_df2[c(77230:77250, 79010), c(12, 97)]
names(main_df2[c(12, 97)])

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

main_df2[c(77230:77250, 79010), c(12, 97)]
names(main_df2[c(12, 97)])

dim(main_df)
dim(main_df2)


# REMOVE additional columns created during the subset merge for Young Center
main_df2 <- subset(main_df2, select = -c(Activity.Date, Item))
dim(main_df2)

summary(main_df2$date_childadvocate)

print('Young Center Referral Import Complete!! ALL DATE IMPORTS COMPLETE!!!!!!!')



#### has attorney alerts ####


#1st do a Precheck of what the values look like before importing
#which(colnames(main_df2) == "date_childadvocate")
#which(colnames(main_df2) == "client_fname")

main_df2[c(77230:77250, 79010), c(12, 97)]
names(main_df2[c(12, 97)])

#2nd pull all of the desired fields in
main_df2 <- merge(main_df2, HasAttny_df[ , c("HasAttorney", "Case.ID")],
                  by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)

#3rd ifelse statement for already_represented field via HasAtty Alerts. If NA put as FALSE, if not keep value
main_df2$already_represented <-
  ifelse(is.na(main_df2$HasAttorney),
         FALSE,
         ifelse(isTRUE(main_df2$HasAttorney), main_df2$HasAttorney, FALSE))

class(main_df2$already_represented)


dim(main_df)
dim(main_df2)
summary(main_df2$already_represented)

# REMOVE additional columns created during the subset merge for Young Center
main_df2 <- subset(main_df2, select = -c(HasAttorney))
dim(main_df2)


#Declined Rep ifelse statement. If NA put as FALSE, if not keep value
summary(main_df2$declined_rep)
main_df2$declined_rep <-ifelse(is.na(main_df2$declined_rep), FALSE, main_df2$declined_rep)
class(main_df2$declined_rep)
summary(main_df2$declined_rep)

#### Correct the Shelter names. Remove Deactivated phrase, remove the star from Harlingen ####
main_df2$ult_facility <- as.factor( gsub(" (deactivated TS 1.4.17)", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("(deactivated)", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub(" Shelter", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub(" TS 1.4.17", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("Soutwest", "Southwest", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("BCFS Harlingen ", "BCFS - Harlingen", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("LSSS - New Hope Renamed to Upbring New Hope", "Upbring New Hope", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("\\(", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("\\)", "", main_df2$ult_facility))
main_df2$ult_facility <- as.factor( gsub("*", "", main_df2$ult_facility))
class(main_df2$ult_facility)
summary(main_df2$ult_facility)

#Insert today's date into "record modified" field"
class(main_df2$record_modified)
summary(main_df2$record_modified)
main_df2$record_modified <- Sys.Date()
summary(main_df2$record_modified)

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
main_df2$gender <- as.factor( gsub("Transgender", "OTHER", main_df2$gender))

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

summary(as.factor(main_df2$language2))
# Done! 1st and 2nd language are not the same

#Erase second language if first language is null
main_df2$language2 <-
  ifelse(is.na(main_df2$language1), NA, main_df2$language2)


#Store delinquency_system values as true or false
class(main_df2$delinquency_system)
summary(main_df2$delinquency_system)


#Store delinquency_system values as true or false.
main_df2$delinquency_system <- as.character( gsub("Yes","TRUE", main_df2$delinquency_system))
main_df2$delinquency_system <- as.character( gsub("No","FALSE", main_df2$delinquency_system))
main_df2$delinquency_system <- ifelse(is.na(main_df2$delinquency_system), FALSE, main_df2$delinquency_system)
main_df2$delinquency_system <- na_if(main_df2$delinquency_system, " ")
summary(as.factor(main_df2$delinquency_system))

#Delinquiency field updated

#### Remove pos_screener and pos_kyr_presenter values whenever date is NOT present ####
main_df2$pos_KYR_presenter <-
  ifelse(is.na(main_df2$date_first_KYR),
         NA,
         main_df2$pos_KYR_presenter)


main_df2$pos_screener <-
  ifelse(is.na(main_df2$date_first_indiv),
         NA,
         main_df2$pos_screener)

#### CREATE FIELDS TO MEASURE TIME BETWEEN FACILITY ENTRY AND SERVICE PROVIDED #####

#Create a field that measures days between Arrival and KYR
main_df2$daysTilKYR <-
  ifelse(
    !is.na(main_df2$date_first_KYR) &
      !is.na(main_df2$Facility.DOE...For.data.integrity.only),
    as.integer(
      as.Date(main_df2$date_first_KYR, format = "%m/%d/%Y") - as.Date(main_df2$Facility.DOE...For.data.integrity.only, format = "%m/%d/%Y")
    ),
    NA
  )

class(main_df2$daysTilKYR)
summary(main_df2$daysTilKYR)

#Create a field that measures days between Arrival and SCREENINGS!


main_df2$daysTilScreen <-
  ifelse(
    !is.na(main_df2$date_first_indiv) &
      !is.na(main_df2$Facility.DOE...For.data.integrity.only),
    as.integer(
      as.Date(main_df2$date_first_indiv, format = "%m/%d/%Y") - as.Date(main_df2$Facility.DOE...For.data.integrity.only, format = "%m/%d/%Y")
    ),
    NA
  )
class(main_df2$daysTilScreen)
summary(main_df2$daysTilScreen)
# Whenever daysTilKYR or daysTilScreen are greater than 10 AAAAANNNDDD the corresponding note 
# field is empty insert string 'Limited resources'
#Starting with daysTilKYR
main_df2[c(45013:45030, 61016), c(12, 27)]

main_df2 <- main_df2 %>%
  
  mutate(NoKYR_10days = as.character(NoKYR_10days)) %>%
  
  mutate(
    NoKYR_10days = ifelse(
      NoKYR_10days == "" & daysTilKYR > 10, 
      "Limited resources", 
      NoKYR_10days)
  )

which(colnames(main_df2) == "NoKYR_10days")
main_df2[c(45013:45030, 61016), c(12, 27)]

##### Erase all NA's from NoKYR_10Days; commenting this section out because Vera prefers NA's everywhere ####

# main_df2$NoKYR_10days <- ifelse( is.na(main_df2$NoKYR_10days),
#                                  "",
#                                  main_df2$NoKYR_10days)
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
# 
# main_df2$NoScreening_10days <- ifelse( is.na(main_df2$NoScreening_10days),
#                                  "",
#                                  main_df2$NoScreening_10days)
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
summary(as.factor(main_df2$pot_rel1))
#plot(main_df2$pot_rel1)


# Change all "Awaiting Attorney Review"s to "PENDING"
main_df2$pot_rel1 <- as.character(gsub("Awaiting Attorney Review", "PENDING", main_df2$pot_rel1))  


summary(as.factor(main_df2$pot_rel1))

# If screening date is NOT null, but potential relief is, store pot_rel as PENDING
main_df2 <- main_df2 %>%
  
  mutate(pot_rel1 = as.character(pot_rel1)) %>%
  
  mutate(
    pot_rel1 = ifelse(
      !is.na(date_first_indiv) & is.na(pot_rel1), 
      "PENDING", 
      pot_rel1)
  )
summary(as.factor(main_df2$pot_rel1))
#str(main_df2)
#
# If screening date is NOT null, but potential relief is, store pot_rel as PENDING
main_df2 <- main_df2 %>%
  
  mutate(pot_rel1 = as.character(pot_rel1)) %>%
  
  mutate(
    pot_rel1 = ifelse(
      date_first_indiv != "" & pot_rel1 == "", 
      "PENDING", 
      pot_rel1)
  )

summary(as.factor(main_df2$pot_rel1))
#str(main_df2)



#
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
summary(as.logical(main_df2$in_house))


main_df2$conflicted_rep <- ifelse(is.na(main_df2$conflicted_rep), FALSE, 
                                  main_df2$conflicted_rep)

summary(as.logical(main_df2$conflicted_rep))

#Change Values to FALSE/TRUE for withdraw_in_house OR whatever the neew field is called
main_df2$withdraw_counsel <- ifelse(is.na(main_df2$withdraw_counsel), FALSE, 
                                  main_df2$withdraw_counsel)
class(main_df2$withdraw_counsel)
summary(as.logical(main_df2$withdraw_counsel))
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

#8 More Fields!! counsel_acpt_date_VTC	date_vol_ngo_VTC	date_vol_privq_VTC	VTC_courtass_1	VTC_courtass_2	VTC_courtass_3	VTC_courtass_4	VTC_courtass_5
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

#date_vol_privq_VTC
main_df2 <- main_df2 %>%
  
  mutate(date_vol_privq_VTC = as.logical(date_vol_privq_VTC)) %>%
  
  mutate(date_vol_privq_VTC = ifelse(is.na(date_vol_privq_VTC),
                                        FALSE,
                                        date_vol_privq_VTC))
summary(as.factor(main_df2$date_vol_privq_VTC))

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
main_df2$a_number  <-main_df2$new_a_number
main_df2 <- subset(main_df2, select = -c(Disposition, record_created, A.Number,	Reporting.A.Number))
names(main_df2)[names(main_df2) == 'Matter.Case.ID.'] <- 'db_id'

length(main_df2)

main_df2 <- subset(main_df2, select = -c(Facility.DOE...For.data.integrity.only, Potential.relief..kids.attny.assessment.review.,
                                         Prescreen.User,	Intake.User, Historical.KYR.Date, new_a_number,
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
main_df2$pos_KYR_presenter <- gsub("none", NA, main_df2$pos_KYR_presenter)
main_df2$pos_KYR_presenter <- gsub("Paralegal", "GENERAL OFFICE CLERK", main_df2$pos_KYR_presenter)
class(main_df2$pos_KYR_presenter)
summary(as.factor(main_df2$pos_KYR_presenter))


#pos_screener
class(main_df2$pos_screener)
summary(main_df2$pos_screener)
main_df2$pos_screener <- gsub("none", NA, main_df2$pos_screener)
class(main_df2$pos_screener)
summary(as.factor(main_df2$pos_screener))

#venue_in_house
class(main_df2$venue_in_house)
summary(main_df2$venue_in_house)
main_df2$venue_in_house <- gsub("none", NA, main_df2$venue_in_house)
class(main_df2$venue_in_house)
summary(as.factor(main_df2$venue_in_house))

#venue_vol_ngo
class(main_df2$venue_vol_ngo)
summary(main_df2$venue_vol_ngo)
main_df2$venue_vol_ngo <- gsub("none", NA, main_df2$venue_vol_ngo)
class(main_df2$venue_vol_ngo)
summary(as.factor(main_df2$venue_vol_ngo))

#withdrawrsn_vol_ngo
class(main_df2$withdrawrsn_vol_ngo)
summary(main_df2$withdrawrsn_vol_ngo)
main_df2$withdrawrsn_vol_ngo <- gsub("none", NA, main_df2$withdrawrsn_vol_ngo)
class(main_df2$withdrawrsn_vol_ngo)
summary(as.factor(main_df2$withdrawrsn_vol_ngo))

#venue_vol_priv
class(main_df2$venue_vol_priv)
summary(main_df2$venue_vol_priv)
main_df2$venue_vol_priv <- gsub("none", NA, main_df2$venue_vol_priv)
class(main_df2$venue_vol_priv)
summary(as.factor(main_df2$venue_vol_priv))

#withdrawrsn_vol_priv
class(main_df2$withdrawrsn_vol_priv)
summary(main_df2$withdrawrsn_vol_priv)
main_df2$withdrawrsn_vol_priv <- gsub("none", NA, main_df2$withdrawrsn_vol_priv)
class(main_df2$withdrawrsn_vol_priv)
summary(as.factor(main_df2$withdrawrsn_vol_priv))

#Replace LMTD APP values WITH FOC
main_df2$court_assist_role <- as.character( gsub("LMTD APP","FOC", main_df2$court_assist_role))

#Replace none or no relief values WITH NA
main_df2$pot_rel1_other <- as.character( gsub("none", NA, main_df2$pot_rel1_other))
main_df2$pot_rel1_other <- as.character( gsub("no relief", NA, main_df2$pot_rel1_other))

#### Get rid of alll of the inappropriate "n/a" values ####
## date_first_KYR, date_indiv_court_prep, date_hearing1, and date_vol_ngo

#date_first_KYR
class(main_df2$date_first_KYR)
#summary(main_df2$date_first_KYR)
main_df2$date_first_KYR <- gsub("n/a", NA, main_df2$date_first_KYR)
main_df2$date_first_KYR <- as.Date(main_df2$date_first_KYR, format = "%m/%d/%Y")
class(main_df2$date_first_KYR)
summary(main_df2$date_first_KYR)

#date_indiv_court_prep
class(main_df2$date_indiv_court_prep)
#summary(main_df2$date_indiv_court_prep)
main_df2$date_indiv_court_prep <- gsub("n/a", NA, main_df2$date_indiv_court_prep)
main_df2$date_indiv_court_prep <- as.Date(main_df2$date_indiv_court_prep, format = "%m/%d/%Y")
class(main_df2$date_indiv_court_prep)
summary(main_df2$date_indiv_court_prep)

#date_hearing1
class(main_df2$date_hearing1)
#summary(main_df2$date_hearing1)
main_df2$date_hearing1 <- gsub("n/a", NA, main_df2$date_hearing1)
main_df2$date_hearing1 <- as.Date(main_df2$date_hearing1, format = "%m/%d/%Y")
class(main_df2$date_hearing1)
summary(main_df2$date_hearing1)

#date_vol_ngo
class(main_df2$date_vol_ngo)
#summary(main_df2$date_vol_ngo)
main_df2$date_vol_ngo <- gsub("n/a", NA, main_df2$date_vol_ngo)
main_df2$date_vol_ngo <- as.Date(main_df2$date_vol_ngo, format = "%m/%d/%Y")
class(main_df2$date_vol_ngo)
summary(main_df2$date_vol_ngo)

#Done taking out 'n/a's!!!

#Store dates as dates so chart runs correctly
main_df2$date_first_indiv <-as.Date(main_df2$date_first_indiv, format = "%m/%d/%Y")
class(main_df2$date_first_indiv)

main_df2$date_first_KYR <-as.Date(main_df2$date_first_KYR, format = "%m/%d/%Y")
class(main_df2$date_first_KYR)

#### Charts & Summaries ####

# Store first and last days of previous month to easily refer to in pot_rel range

library(lubridate)

currentDate <- Sys.Date()

end_last_month <- rollback(currentDate)

begin_last_month <- rollback(end_last_month, roll_to_first = TRUE)

current_month_name <- format(Sys.Date(), format = "%B")
current_month_name

previous_month_name <- format(begin_last_month, format = "%B")
previous_month_name

#Create a small data frame of pot_rel1 and counts, order descending by frequency of values

# https://stackoverflow.com/questions/34984132/plot-non-numeric-data
df_pot_rels_monthly <-
  main_df2[!is.na(main_df2$date_first_indiv) & (main_df2$date_first_indiv >= begin_last_month & main_df2$date_first_indiv <= end_last_month),]   %>% # Pipe df into group_by
  group_by(pot_rel1)              %>% # grouping by 'pot_rel' column
  summarise(relief_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
#reorder the data so that it's descending.
df_pot_rels_monthly <- df_pot_rels_monthly[order(-df_pot_rels_monthly$relief_count),]
df_pot_rels_monthly


#random color index generator, so we don't have to think about a new color each time
randomcolor <- runif(3, max = 4)/4
p_monthly_pot_rel <- ggplot(df_pot_rels_monthly, aes(x = reorder(pot_rel1, -relief_count), y = relief_count)) +  # 
  geom_bar(stat = 'identity', fill = do.call(rgb, as.list(randomcolor)), color = 'darkred')       +       # stat='identity' is used for summarized data.
  geom_text(aes(label = relief_count), vjust = -1) +
  labs(title = 'Potential Reliefs', subtitle = previous_month_name )

p_monthly_pot_rel

#### Unnecessary plot of cumulative reliefs ####
df_pot_rels_cum <-
  main_df2[!is.na(main_df2$date_first_indiv), ]           %>% # Pipe df into group_by
  group_by(pot_rel1)              %>% # grouping by 'pot_rel' column
  summarise(name_count = n())     # calculate the name count for each group
## 'df_summary' now contains the summary data for each 'type'
#reorder the data so that it's descending.
df_pot_rels_cum <- df_pot_rels_cum[order(-df_pot_rels_cum$name_count),]
df_pot_rels_cum

#random color index generator, so we don't have to think about a new color each time
randomcolor <- runif(3, max = 4)/4
p_cumulative_pot_rel <- ggplot(df_pot_rels_cum, aes(x = reorder(pot_rel1, -name_count), y = name_count)) +  # 
  geom_bar(stat = 'identity', fill = do.call(rgb, as.list(randomcolor)), color = 'darkred')       +       # stat='identity' is used for summarized data.
  geom_text(aes(label = name_count), vjust = -1) +
  labs(title = 'Potential Reliefs', subtitle = 'Cumulative' )

p_cumulative_pot_rel



#### Testing Zone #Testing Zone #Testing Zone #Testing Zone #Testing Zone #Testing Zone ####

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone

#Testing Zone
#---------------------------------------------------------------------------------------------------------
# test code goes here.


class(main_df2$date_first_indiv)
class(main_df2$pot_rel1)
class(main_df2$pot_rel1) <- "Factor"
class(main_df2$pot_rel1)
# # Format 8 digit A#s as characters with the leading 0. This  didn't work. Nor did an as.character  paste0 approach work.
#summary(main_df2$a_number)
#class(main_df2$a_number)
#main_df2$a_number <- formatC(main_df2$a_number, width = 9, format = "d", flag = "0")
# 



# A tibble: 15 x 2
# pot_rel1                 name_count
# <chr>                         <int>
# 1 ASYLUM                         8154
# 2 ASYLUM WITHHOLDING               30
# 3 CITIZENSHIP                       2
# 4 DACA                              3
# 5 Family Petition                  22
# 6 OTHER                           143
# 7 PENDING                         545
# 8 Prosecutorial Discretion          2
# 9 SIJ                            7196
# 10 T VISA                           43
# 11 TPS                               2
# 12 U VISA                           32
# 13 UNKNOWN                       19655
# 14 VAWA                              1
# 15 NA                            30738 #actually took these out with the is.na... something

df_sumsum <-
  df_summary[!is.na(main_df2$pot_rel1),]

### Two ways to plot using ggplot

## (1) Plot pre summarized data: 'df_summary'.
p1 <- ggplot(df_summary, aes(pot_rel1, name_count)) +  # 
  geom_bar(stat = 'identity')           # stat='identity' is used for summarized data.
  geom_text(aes(label = name_count), vjust = -1) +
  ggtitle('Cumulative Potential Reliefs')

#random color index generator
randomcolor <- runif(3, max = 4)/4
p2.0 <- ggplot(df_summary, aes(x = reorder(pot_rel1, -name_count), y = name_count)) +  # 
  geom_bar(stat = 'identity', fill = do.call(rgb, as.list(randomcolor)), color = 'darkred')       +    # stat='identity' is used for summarized data.
  geom_text(aes(label = name_count), vjust = -1) +
  ggtitle('Cumulative Potential Reliefs')

p2.0
  
  
p2.3 <- ggplot(df_summary, aes(x = reorder(pot_rel1, -name_count), y = name_count)) +  # 
  geom_bar(stat = 'identity')       +       # stat='identity' is used for summarized data.
  geom_text(aes(label = name_count), vjust = -1) +
  labs(title = 'Potential Reliefs', subtitle = previous_month_name )

p2.3




## (2) Bar plot on original data frame (not summarised)
p1 <- ggplot(main_df2[!is.na(main_df2$pot_rel1),], aes(main_df2$pot_rel1[!is.na(main_df2$pot_rel1)]), )      +
  geom_bar()             + # 'stat' isn't needed here.
  labs(x = 'Potential Relief', 'count of children' , y = 'count of children') +
  geom_text( stat = 'count', aes(label = ..count..), vjust = -1) +
  ggtitle('Cumulative Pot_Rels')

p2 <- ggplot(main_df2[!is.na(main_df2$pot_rel1),], aes(main_df2$pot_rel1[!is.na(main_df2$pot_rel1)]), )      +
  geom_bar()             + # 'stat' isn't needed here.
  labs(x = 'Potential Relief' , y = 'count of children', ordered('count of children')) +
  geom_text( stat = 'count', aes(label = ..count..), vjust = -1) +
  ggtitle('Cumulative Pot_Rels')



help(geom_bar)



# # Three years ago, we launched the great American comeback. Tonight, I stand before you to share the incredible results. 
# Jobs are booming, incomes are soaring, poverty is plummeting, crime is falling, confidence is surging, and our country is thriving 
# and highly respected again! America's enemies are on the run, America's fortunes are on the rise, and America's future is blazing bright.
#
# The years of economic decay are over. The days of our country being used, taken advantage of, and even scorned by other nations are long 
# behind us. Gone too are the broken promises, jobless recoveries, tired platitudes, and constant excuses for the depletion of American wealth, 
# power, and prestige.
#
# In just 3 short years, we have shattered the mentality of American decline, and we have rejected the downsizing of America's destiny. 
# We are moving forward at a pace that was unimaginable just a short time ago, and we are never going back! I am thrilled to report to you 
# tonight that our economy is the best it has ever been. Our military is completely rebuilt, with its power being unmatched anywhere in the 
# world - and it is not even close. Our borders are secure. Our families are flourishing. Our values are renewed. Our pride is restored. And 
# for all these reasons, I say to the people of our great country, and to the Members of Congress before me: The State of our Union is stronger 
# than ever before!
# 
# 
# 
# 
#---------------------------------------------------------------------------------------------------------
# FIN
# FIN# FIN# # FIN
# # FIN
# # FIN
# FIN
# FIN
# FIN

#### FINISHING THE DATA AND SPITTING OUT A CSV WITH A TIMESTAMPED TITLE ####




#View(main_df2)
#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
file_name <- paste0("Vera_Automation_Test", now, ".csv" )
file_name_act <- paste0("Vera_Activities_Test", now, ".csv" )
file_name_vlook <- paste0("Vlook_Test", now, ".csv" )

#print(file_name)

######## Export Data as a csv file #######
write.table(main_df2, sep=",", file_name, row.names = FALSE, )

#Names for other report exports if you want
write.csv(activities_uniq_df, 'activities_file.csv', row.names = FALSE, )
write.csv(testVlookup_df, 'v-lookie-here3.csv', row.names = FALSE, )
