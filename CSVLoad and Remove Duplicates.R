  #READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
  #Install Tidyverse to ease removing duplicates and other features
  install.packages("tidyverse")
#Add Library that enable removing duplicates
library(tidyverse)
library(magrittr)
#Set the working Directory to RVeraAuto folder on G:Drive
setwd("G:/My Drive/RVeraAuto")

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
view(head(activities_uniq_df, 50))
activities_uniq_df <- activities_uniq_df[order(activities_uniq_df$Item, activities_uniq_df$Activity.Date,
                                            activities_uniq_df$Case.ID, activities_uniq_df$new_a_number),]
view(head(activities_uniq_df, 50))

# FORMAT THE DATE to American style baby! 
# ***Important for this Formatting to happen AFTER its already been sorted***
# because the sort order for dates depends on which format you chose, doesn't default to chronological sorting #DUMB
activities_uniq_df$Activity.Date <- format(activities_uniq_df$Activity.Date, "%m/%d/%Y")
view(head(activities_uniq_df, 50))

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
#3rd Re-Format the activity and followup dates because the merge causes date to go back to factor
main_df2$Activity.Date <-as.Date(main_df2$Activity.Date, format = "%m/%d/%Y")
class(main_df2$Activity.Date)

main_df2$date_first_followup <-as.Date(main_df2$date_first_followup, format = "%m/%d/%Y")
class(main_df2$date_first_followup)

#class(main_df2$date_first_followup) <- "Date"


#THIS IS ALSO UNDER TESTING ******************************
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

#Confirm successful replacements
which( colnames(main_df2) == "cov_coa_date")
which( colnames(main_df2) == "cov_coa")
which( colnames(main_df2) == "Activity.Date")
main_df2[c(30:45, 7001), c(32:33, 117, 118)]
names(main_df2[c(31:33, 117:118)])




#Testing Zone
#---------------------------------------------------------------------------------------------------------

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

file_name <- paste0("Vera_Automation_Test", now, ".csv" )
file_name_act <- paste0("Vera_Activities_Test", now, ".csv" )
file_name_vlook <- paste0("Vlook_Test", now, ".csv" )

#print(file_name)

######## Export Data as a csv file #######
write.table(main_df2, sep=",", file_name, row.names = FALSE, )

write.csv(activities_uniq_df, 'activities_file.csv', row.names = FALSE, )
write.csv(testVlookup_df, 'v-lookie-here3.csv', row.names = FALSE, )
