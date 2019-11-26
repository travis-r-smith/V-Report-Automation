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

#Not necessary to remove duplicates from 1) HasAttny data set NOR 2) litigations

#GETTING TO KNOW THE DATA
#List all existing variables to see what they are and if any are unnecessary
ls()

# Class of each variable to see if they are the same type that could help identify 
# issues. One I noticed was the a_num variable was a list as opposed to the ra_num  
#which was a dataframe so I converted it. IDK if that did anything b.c same results.
class(main_df2$a_number)
class(activities_df)
class(main_df)
class(now)


#USING REPORTING A# WHENEVER IT IS PRESENT AND A# WHEN THERE ISN'T A REPORTING A#
#Remove hyphens from a number fields and store the column as integers

main_df2$Reporting.A.Number <- as.integer( gsub("-", "", main_df2$Reporting.A.Number))
main_df2$a_number <- as.integer( gsub("-", "", main_df2$a_number))

#Do same process for the activities dataset and the case alerts dataset!
activities_uniq_df$Reporting.A.Number <- as.integer( gsub("-", "", activities_uniq_df$Reporting.A.Number))
activities_uniq_df$a_number <- as.integer( gsub("-", "", activities_uniq_df$a_number))

HasAttny_df$Reporting.A.Number <- as.integer( gsub("-", "", HasAttny_df$Reporting.A.Number))
HasAttny_df$a_number <- as.integer( gsub("-", "", HasAttny_df$a_number))

#Use Coalesce to create a new column that contains the NEW A NUMBERS 
main_df2$new_a_number <- dplyr::coalesce(main_df2$Reporting.A.Number, main_df2$a_number)
view(head(main_df2$new_a_number, 30))

activities_uniq_df$new_a_number <- dplyr::coalesce(activities_uniq_df$Reporting.A.Number, activities_uniq_df$a_number)
view(head(activities_uniq_df$new_a_number, 30))

HasAttny_df$new_a_number <- dplyr::coalesce(HasAttny_df$Reporting.A.Number, HasAttny_df$a_number)
view(head(HasAttny_df$new_a_number, 30))
# FIN

#FINISHING THE DATA AND SPITTING OUT A CSV WITH A TIMESTAMPED TITLE
#View(main_df2)

file_name <- paste0("Vera_Automation_Test", now, ".csv" )
#print(file_name)

######## Export Data as a csv file #######
write.table(main_df2, sep=",", file_name, row.names = FALSE, )
