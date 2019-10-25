#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE
#Install Tidyverse to ease removing duplicates and other features
install.packages("tidyverse")
#Add Library that enable removing duplicates
library(tidyverse)
#Add Library "dplyr" but I'm confused because it says dplyr is a part of tidyverse, whatevs.
library(dplyr)

#Set the working Directory to RVeraAuto folder on G:Drive
setwd("G:/My Drive/RVeraAuto")

#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
#print(now)

# Tell R to read the main services data report as well as other more 
# focused report files from the directory.
original_main_df <- read.csv("VeraMonthlyTest.csv")
activitites_df <- read.csv("VeraActivitiesTest.csv")
HasAttny_df <- read.csv("VeraAlertHasAttorneyTest.csv")
Litig_df <- read.csv("LitigationTest.csv")
main_df <- original_main_df
#View(MRload)

str(main_df)
#Remove duplicates from MainReport and store the value as 'MainUniq'
main_df <- main_df[!duplicated(main_df$Matter.Case.ID), ]
# View(MainUniq)

#Remove duplicates from Activities Report and store the value as 'ActUniq'
activities_uniq_df <- main_df[!duplicated(activities_df$Case.ID, activities_df$Item ), ]
#View(ActUniq)




# I don't believe removing Duplicates from case alerts nor from Litigations is helpful nor desired but if so I can repeat the code later


# -------------------------------------------------------------------
# 8.21.19 attempt based on youtube video Version #2

# If the Reporting A# is not Null then the real A# should equal the Reporting A#
#a_num <- MainReportload$Matter.a_number
a_num <- main_df["a_number"]

#ra_num <- MainReportload$Matter.Reporting.A.Number
ra_num <- main_df['Reporting.A.Number']
str(a_num)

#Create the condition if the A# is greater than the reporting A#
cond <- is.null(main_df$Reporting.A.Number)

#Set up a statement: If the condition is true the A# value replaces the Reporting A# value
main_df$Reporting.A.Number <- ifelse(cond, 
                                         main_df$Reporting.A.Number <- main_df$a_number, 
                                         main_df$Reporting.A.Number <- main_df$Reporting.A.Number)

cond <- is.null(main_df[, "a_number"])
#Set up another statement (?unnecessary?) where if the reporting A# is  greater the Reporting A# value is what get's stored
main_df[,"Reporting.A.Number"] <- ifelse(cond, main_df[,"Reporting.A.Number"], 
                                         main_df[,"a_number"])



tail(main_df$Reporting.A.Number)
head(main_df$Reporting.A.Number)
summary(main_df$Reporting.A.Number)
View(main_df)



# 8.21.19 attempt based on youtube video

# If the Reporting A# is not Null then the real A# should equal the Reporting A#
#a_num <- MainReportload$Matter.a_number
a_num <- main_df["a_number"]

#ra_num <- MainReportload$Matter.Reporting.A.Number
ra_num <- main_df['Reporting.A.Number']
str(a_num)

#Create the condition if the A# is greater than the reporting A#
cond <- main_df[, "a_number"] != ""

#Set up a statement: If the condition is true the A# value replaces the Reporting A# value
main_df[,"Reporting.A.Number"] <- ifelse(cond, main_df[,"a_number"], 
                                         main_df[,"Reporting.A.Number"])

cond <- main_df[, "a_number"] = ""
#Set up another statement (?unnecessary?) where if the reporting A# is  greater the Reporting A# value is what get's stored
main_df[,"Reporting.A.Number"] <- ifelse(cond, main_df[,"Reporting.A.Number"], 
                                         main_df[,"a_number"])



tail(main_df$Reporting.A.Number)
head(main_df$Reporting.A.Number)
summary(main_df$Reporting.A.Number)
View(main_df)





# ---------------------------------------------------------------------------------------------

#8.6.19 Attempts based on youtube video https://youtu.be/CsFRM1_heM4 approach this time is to just create a new column for the anumber that we plan to use. Possible progress but here the issue
# is that the normal A# is not populating. Wondering if it's not treating the empty Reporting A# values as "null" only thing I can think of at the moment.
if(is.null(main_df$Reporting.A.Number)) {
  main_df$NEW.A.Number <- main_df$a_number
} else {
  main_df$NEW.A.Number <- main_df$Reporting.A.Number
  }
main_df$NEW.A.Number
#View(MainReportload)

# Class of each variable to see if they are the same type that could help identify 
# issues. One I noticed was the a_num variable was a list as opposed to the ra_num  
#which was a dataframe so I converted it. IDK if that did anything b.c same results.
class(a_num)
class(activities_df)
class(main_df)
class(now)
a_num <- as.data.frame(a_num)
class(a_num)

# Check the Class, Summary and Structure of Reporting A number
class(ra_num)
summary(ra_num)
str(ra_num)


#8.13.19 cell reference idea
while(ra_num > 1){
 main_df$NEWerer.A.Number <- ra_num
}

while(is.null(ra_num )){
 main_df$NEWerer.A.Number <- a_num
}
View(main_df)

#Internet example of for (changed it to why later)
#for(i in 2:nrow(df)){
# df$B[i] <- df$A[i] + df$B[i-1]
# }


#8.13.19 after looking at structure I'm wondering if R is interpreting the null values as "" instead of as "NULL" Going to try another idea
#I used the example and I think it could work with additional tweaks
main_df$NEWerer.A.Number <- ifelse(ra_num == "", a_num$x, ra_num$x)


summary(main_df$NEWerer.A.Number)
str(main_df$NEWerer.A.Number)

#View(MainReportload)


#8.6.19 Alternative based on brain and minimal SQL knowledge, just do a coalesce, coalesce is available in the dplyr library. Same issue as the above attempt will contine with research.
main_df$NEWer.A.Number <- coalesce(main_df$Reporting.A.Number, main_df$a_number)
main_df$NEWer.A.Number
# MainReportload$Matter.NEW.A.Number <- if(!is.null(MainReportload$Matter.Reporting.A.Number)) {MainReportload$Matter.NEW.A.Number <- MainReportload$Matter.Reporting.A.Number}

#Failed attempt below
#if(is.null(MainReportload$Matter.Reporting.A.Number)=FALSE) a_num <- MainReportload$Matter.Reporting.A.Number
# a_num <- if(!is.null(MainReportload$Matter.Reporting.A.Number)) {a_num <- MainReportload$Matter.Reporting.A.Number}

#Another failed attempt below
# a_num <- ifelse(is.null(MainReportload$Reporting.A.Number), MainReportload$Matter.Reporting.A.Number, MainReportload$Matter.a.number)

#Another failed attempt below
#select <- a_num < ra_num
#MRload[select,"a_number"] <- MRload[select,"Reporting.A.Number"]

# #Another failed attempt below
# if (!is.null(ra_num)) 
#   { a_num <- ra_num }

#Another failed attempt below
# Example ifelse(test_expression, x, y); Attempt:
a_num <- ifelse(is.null(ra_num), a_num <- ra_num, a_num)



#List all existing variables to see what they are and if any are necessary
ls()

# Class of each variable to see if they are the same type that could help identify 
# issues. One I noticed was the a_num variable was a list as opposed to the ra_num  
#which was a dataframe so I converted it. IDK if that did anything b.c same results.
class(a_num)
class(activities_df)
class(main_df)
class(now)
a_num <- as.data.frame(a_num)
class(a_num)

# Check the Class, Summary and Structure of Reporting A number
class(ra_num)
summary(ra_num)
str(ra_num)

# Example of Loop from internet
# x <- c(2,5,3,9,8,11,6)
# count <- 0
# for (val in x) {
#   if(val %% 2 == 0)  count = count+1
# }
# print(count)




for (val in ra_num)
  {
  if(val !is.null(ra_num))  a_num = ra_num
    }
View(a_num)

main_df$Matter.a_number<-a_num

#This might also work IDK

for (val in ra_num)
{
  if (!is.null(ra_num)) a_num <- ra_num
}
View(a_num)

main_df$Matter.a_number<-a_num



#I used the example and I think it could work with additional tweaks

for (val in ra_num)
  {
  if(val !is.null(ra_num))  a_num = ra_num
    }
View(a_num)

main_df$Matter.a_number<-a_num

#This might also work IDK

for (val in ra_num)
{
  if (!is.null(ra_num)) a_num <- ra_num
}
View(a_num)

main_df$Matter.a_number<-a_num




#the above code does not work for replacing


 
View(main_df
)

file_name <- paste0("Vera_Automation_Test", now, ".csv" )
#print(file_name)

######## Export Data as a csv file #######
write.table(main_df, sep=",", file_name, row.names = FALSE, )
