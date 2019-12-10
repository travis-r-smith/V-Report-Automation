  #READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
  #Install Tidyverse to ease removing duplicates and other features
  install.packages("tidyverse")
#Add Library that enable removing duplicates
library(tidyverse)
#Add Library "dplyr" but I'm confused because it says dplyr is a part of tidyverse, whatevs.
library(dplyr)
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

str(main_df2)
#Remove duplicates from MainReport and store the value as 'MainUniq'
dim(main_df2)
main_df2 <- main_df2[!duplicated(main_df$Matter.Case.ID), ]
dim(main_df2)
# View(MainUniq)

#Remove duplicates from Activities Report and store the value as 'ActUniq'
dim(activities_df)
activities_uniq_df <- activities_df[!duplicated(activities_df$Case.ID, activities_df$Item ), ]
dim(activities_uniq_df)
#View(ActUniq)

#Store activity dates as actual dates 
activities_uniq_df$Activity.Date <-as.Date(activities_uniq_df$Activity.Date, format = "%m/%d/%Y")


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
#Remove hyphens from a number fields and store the column as integers

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

#Re-sort data by Activity Item, Activity Date oldest on top, CaseID lowest on top, then A# lowest 
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

#Testing Zone
#---------------------------------------------------------------------------------------------------------
#12.3.19 Attempt to pull Activity date from activities dataset into main dataset based on Case ID#.
#
testVlookup_df <- merge(main_df2, FollowUpSub[ , c("new_a_number", "Item", "Activity.Date", "Case.ID")],
                          by.x = "Matter.Case.ID.", by.y = "Case.ID", all.x = TRUE)
view(head(testVlookup_df))

testVlookup_df$date_first_followup <- as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y")


#12.10.19 attempting to ONLY pull activity dates in when the column is NULL OR when the column has a value that has a later date.
testVlookup_df$date_first_followup <- ifelse(is.null(testVlookup_df$date_first_followup) = TRUE,
                                             as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y"),
                                             ifelse(as.integer(testVlookup_df$date_first_followup) > as.integer(testVlookup_df$Activity.Date.x),
                                                    as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y"),)
                                             )
# testVlookup_df$date_first_followup <- if(is.null(testVlookup_df$date_first_followup) = TRUE),
#                                              as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y"),
#                                              ifelse(as.integer(testVlookup_df$date_first_followup) > as.integer(testVlookup_df$Activity.Date.x),
#                                                     as.Date(testVlookup_df$Activity.Date.x, format = "%m/%d/%Y"),
#                                              ))

?if
#?is.null
#?grepl
#?ifelse
#?'>'
#stop()
#Insert date values from Follow Ups if A# matches ???????????????
#main_df2$date_first_followup <- format()
#Not 100% sure 11.30.19
#merge(main_df2, unique(activities_uniq_df)[, c("a_number", "Activity.Date")], by="a_number")
#head(main_df2$Activity.Date)
#names(main_df2)
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
