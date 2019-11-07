##### Load Libraries #####
library(readxl)
library(tibble)
library(readr)

##### Set Path #####
# Set the working directory to the one that contains the census .xlsx file below
setwd("~")

##### Column Names #####
identifiers <- c(
  "ResponseID", 
  "ClassID", 
  "ClassSize", 
  "ClassGrade", 
  "DataYear", 
  "Country", 
  "Region"
)

correct_names <- c(
  "Gender",
  "Ageyears",
  "Handed",
  "Height_cm",
  "Footlength_cm",
  "Armspan_cm",
  "Languages_spoken",
  "Travel_to_School",
  "Travel_time_to_School",
  "Reaction_time",
  "Score_in_memory_game",
  "Favourite_physical_activity",
  "Importance_reducing_pollution",
  "Importance_recycling_rubbish",
  "Importance_conserving_water",
  "Importance_saving_energy",
  "Importance_owning_computer",
  "Importance_Internet_access",
  "Left_Footlength_cm",
  "Longer_foot",
  "Index_Fingerlength_mm",
  "Ring_Fingerlength_mm",
  "Longer_Finger_Lefthand",
  "Birth_month",
  "Favorite_Season",
  "Allergies",
  "Vegetarian",
  "Favorite_Food",
  "Beverage",
  "Favorite_School_Subject",
  "Sleep_Hours_Schoolnight",
  "Sleep_Hours_Non_Schoolnight",
  "Home_Occupants",
  "Home_Internet_Access",
  "Communication_With_Friends",
  "Text_Messages_Sent_Yesterday",
  "Text_Messages_Received_Yesterday",
  "Hanging_Out_With_Friends_Hours",
  "Talking_On_Phone_Hours",
  "Doing_Homework_Hours",
  "Doing_Things_With_Family_Hours",
  "Outdoor_Activities_Hours",
  "Video_Games_Hours",
  "Social_Websites_Hours",
  "Texting_Messaging_Hours",
  "Computer_Use_Hours",
  "Watching_TV_Hours",
  "Paid_Work_Hours",
  "Work_At_Home_Hours",
  "Schoolwork_Pressure",
  "Planned_Education_Level",
  "Favorite_Music",
  "Superpower",
  "Preferred_Status",
  "Role_Model_Type",
  "Charity_Donation"
)


##### Read Data #####
census <- read_xlsx(
  "CensusAtSchoolDatabase2018asof12-8-18.xlsx",
  col_names = c(identifiers, correct_names),
  col_types = "text"
)

##### Write to CSV #####
write_csv(census, path = "~/.csv")

#### Cleaning Variables ####

## Travel_to_school - Change the made-up choices to NA
unique(census$Travel_to_School)
census$Travel_to_School[census$Travel_to_School == "Travel_to_school"] = NA
census$Travel_to_School[census$Travel_to_School == "Dinosaur"] = NA

## Travel_time_to_school
census$Travel_time_to_School = as.numeric(census$Travel_time_to_School)
copy = census$Travel_time_to_School

under1 = copy[copy<1]
under1 = na.omit(under1)
sort(unique(under1))

high = copy[copy>180]
high = na.omit(high)
sort(unique(high))

# For inputs under 1, I assume that student meant decimal of the hour. To fix this, multiply them by 60. 
# Anything under 0.1 is equivalent to less than 1 minute, so these will also be labelled as NA.
# inputs above 180 (3 hours) is unlikely, we will label those as NA.
census$Travel_time_to_School[is.na(census$Travel_time_to_School)] = 0
for(i in 1:nrow(census)){
  if ((census$Travel_time_to_School[i] < 1) && (census$Travel_time_to_School[i] >= 0.1)){
    census$Travel_time_to_School[i] = census$Travel_time_to_School[i] * 60 
  }else if((census$Travel_time_to_School[i] > 180) || (census$Travel_time_to_School[i] < 0.1)){
    census$Travel_time_to_School[i] = NA
  }
}

summary(census$Travel_time_to_School)

