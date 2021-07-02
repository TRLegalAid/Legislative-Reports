library(dplyr)
library(readxl)
library(rgeocodio)
library(data.table)
library(tidyverse)

##----Read Data----##

## Note for 2021: All cases should be geocoded in Legal Server! And may even include legislative districts.

prob_codes <- read_excel("Data/problem_codes.xlsx") %>%
  select(-2)

trla_SA <- read.csv("Data/SACount.csv")
addl <- read_excel("Data/addl.xlsx")
trla_dists <- read_excel("Data/trla_dists.xlsx")

case_data <- read_excel("Data/case_data_2020.xlsx") %>%
  filter(!grepl("PD", `Legal Problem Code`) & (`County of Residence` %in% trla_SA$County_Name) & State == "TX") %>%
  inner_join(prob_codes) %>%
  mutate(fulladdress = paste(`Street Address`, City, State, `Zip Code`, sep = ", ")) 

case_data$Category[is.na(case_data$Category)] = "Other"

##----Geocode Data with senate/house districts appended----##

#Geocode the data in batches -- this could definitely be a function

# case_data_part1 <- case_data[1:1000,]
# 
# data <- gio_batch_geocode(case_data_part1$fulladdress, fields = c("stateleg"))
# df <- unnest(data)
# results1 <- df %>%
#   distinct(query, .keep_all = TRUE)
# 
# 
# case_data_part2 <- case_data[1001:11000,]
# 
# data2 <- gio_batch_geocode(case_data_part2$fulladdress, fields = c("stateleg"))
# df2 <- unnest(data2)
# results2 <- df2 %>%
#   distinct(query, .keep_all = TRUE)
# 
# 
# case_data_part3 <- case_data[11001:21000,]
# 
# data3 <- gio_batch_geocode(case_data_part3$fulladdress, fields = c("stateleg"))
# df3 <- unnest(data3)
# results3 <- df3 %>%
#   distinct(query, .keep_all = TRUE)
# 
# 
# case_data_part4 <- case_data[21001:30125,]
# 
# data4 <- gio_batch_geocode(case_data_part4$fulladdress, fields = c("stateleg"))
# df4 <- unnest(data4)
# results4 <- df4 %>%
#   distinct(query, .keep_all = TRUE)


#Bind the batched geocoded data together and write to csv
# results <- bind_rows(results1, results2, results3, results4) %>%
# write.csv("fully_geocoded_1.4.20.csv")

##----If you skipped geocoding, read geocoded data in here----##

# results <- read.csv("Data/fully_geocoded_1.4.20.csv")

##----Join geocoding back to the case data----##

#Join the queries back with the case data -- could be one query to many addresses

results_join <- case_data %>%
  inner_join(results, by = c("fulladdress" = "query")) %>%
  select(-c(starts_with("address_components"), "source",
            "fields.state_legislative_districts.house.is_upcoming_state_legislative_district",
            "fields.state_legislative_districts.senate.is_upcoming_state_legislative_district")) %>%
  distinct(`Matter/Case ID#`, .keep_all = TRUE) %>%
  inner_join(addl) 

#write.csv(results_join, "joined_geocoded.csv")
#results_join <- read.csv("joined_geocoded.csv")

  results_join$`Percentage of Poverty` <- as.numeric(results_join$`Percentage of Poverty`)
  results_join$`Number of People under 18` <- as.numeric(results_join$`Number of People under 18`)
  results_join$`Age at Intake` <- as.numeric(results_join$`Age at Intake`)
  results_join$`Number of People 18 and Over` <- as.numeric(results_join$`Number of People 18 and Over`)
  
  results_join <- results_join %>%
    mutate(Children = `Number of People under 18` > 1)

##----Create Data Tables----##
  
# Stats for house districts  
  results_house <- results_join %>%
    group_by(fields.state_legislative_districts.house.name) %>%
    mutate("Intakes" = n()) %>%
    ungroup() %>%
    filter(`Case Disposition` != "Rejected") %>%
    group_by(fields.state_legislative_districts.house.name) %>%
    mutate("Not Rejected" = n(),
              "Closed" = sum(`Case Disposition`=="Closed"),
              "Avg_Poverty" = mean(`Percentage of Poverty`),
              "Vets" = sum(Veteran=="Yes"),
              "COVID" = length(grep("Coronavirus", Disasters)),
              "DV" = length(grep("Domestic violence", Category)),
              "Avg Age" = mean(`Age at Intake`),
              "Clients with Children" = sum(Children),
              "Total Children" = sum(`Number of People under 18`),
              "Total HH size" = sum(`Number of People under 18`, `Number of People 18 and Over`),
              .groups = "rowwise"
    ) %>%
    distinct(fields.state_legislative_districts.house.name, .keep_all = TRUE) %>%
    select(c(fields.state_legislative_districts.house.name, Intakes, `Not Rejected`, Avg_Poverty, Vets, COVID, DV, `Avg Age`, `Clients with Children`, `Total Children`,
             `Total HH size`)) %>%
    rename("Dist" = fields.state_legislative_districts.house.name)
  
  
# Stats for senate districts, then merge
  results_both <- results_join %>%
    group_by(fields.state_legislative_districts.senate.name) %>%
    mutate("Intakes" = n()) %>%
    filter(`Case Disposition` != "Rejected") %>%
    group_by(fields.state_legislative_districts.senate.name) %>%
    mutate("Not Rejected" = n(),
              "Closed" = sum(`Case Disposition`=="Closed"),
              "Avg_Poverty" = mean(`Percentage of Poverty`),
              "Vets" = sum(Veteran=="Yes"),
              "COVID" = length(grep("Coronavirus", Disasters)),
              "DV" = length(grep("Domestic violence", Category)),
              "Avg Age" = mean(`Age at Intake`),
              "Clients with Children" = sum(Children),
              "Total Children" = sum(`Number of People under 18`),
              "Total HH size" = sum(`Number of People under 18`, `Number of People 18 and Over`),
              .groups = "rowwise"
    ) %>%
    distinct(fields.state_legislative_districts.senate.name, .keep_all = TRUE) %>%
    select(c(fields.state_legislative_districts.senate.name, Intakes, `Not Rejected`, Avg_Poverty, Vets, COVID, DV, `Avg Age`, `Clients with Children`, `Total Children`,
             `Total HH size`)) %>%
    rename("Dist" = fields.state_legislative_districts.senate.name) %>%
    rbind(results_house)


# Get case types and counts for house and senate districts, then merge  
  case_types_house <- results_join %>%
    group_by(fields.state_legislative_districts.house.name, Category) %>%
    summarise("Intakes" = n()) %>%
    rename("Dist" = fields.state_legislative_districts.house.name)
  
  case_types_both <- results_join %>%
    group_by(fields.state_legislative_districts.senate.name, Category) %>%
    summarise("Intakes" = n()) %>%
    rename("Dist" = fields.state_legislative_districts.senate.name) %>%
    rbind(case_types_house) %>%
    group_by(Dist) %>%
    nest() %>%
    rename("Intake Case Types" = "data")

  
# Get NON-REJECTED case types and counts for house and senate districts, then merge
  case_types_house_nrj <- results_join %>%
    filter(`Case Disposition` != "Rejected") %>%
    group_by(fields.state_legislative_districts.house.name, Category) %>%
    summarise("Intakes" = n()) %>%
    rename("Dist" = fields.state_legislative_districts.house.name)
  
  case_types_both_nrj <- results_join %>%
    filter(`Case Disposition` != "Rejected") %>%
    group_by(fields.state_legislative_districts.senate.name, Category) %>%
    summarise("Intakes" = n()) %>%
    rename("Dist" = fields.state_legislative_districts.senate.name) %>%
    rbind(case_types_house_nrj) %>%
    group_by(Dist) %>%
    nest() %>%
    rename("Non-rejected Case Types" = "data")
  
  
# Join results into one big dataframe  
  results_both <- results_both %>% 
    right_join(case_types_both) %>%
    right_join(case_types_both_nrj) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    filter(!grepl("Assembly District", Dist))
  
  results_both <- results_both %>%
    filter(Dist %in% trla_dists$`District formatted`)

  rm(case_types_house_nrj, case_types_house, case_types_both)

  
# Repeat for entire service area
  case_data_SA <- case_data %>%
    filter(`County of Residence` %in% trla_SA$County_Name) %>%
    inner_join(addl)
  
  case_data_SA$`Percentage of Poverty` <- as.numeric(case_data_SA$`Percentage of Poverty`)
  case_data_SA$`Number of People under 18` <- as.numeric(case_data_SA$`Number of People under 18`)
  case_data_SA$`Age at Intake` <- as.numeric(case_data_SA$`Age at Intake`)
  case_data_SA$`Number of People 18 and Over` <- as.numeric(case_data_SA$`Number of People 18 and Over`)
  
  case_data_SA <- case_data_SA %>%
    mutate(Children = `Number of People under 18` > 1)
  
  SA_results1 <- case_data_SA %>%
    filter(`Case Disposition` != "Rejected") %>%
    summarise("Not Rejected" = n(),
              "Closed" = sum(`Case Disposition`=="Closed"),
              "Avg_Poverty" = mean(`Percentage of Poverty`),
              "Vets" = sum(Veteran=="Yes"),
              "COVID" = length(grep("Coronavirus", Disasters)),
              "DV" = length(grep("Domestic violence", Category)),
              "Avg Age" = mean(`Age at Intake`),
              "Clients with Children" = sum(Children),
              "Total Children" = sum(`Number of People under 18`),
              "Total HH size" = sum(`Number of People under 18`, `Number of People 18 and Over`),
              .groups = "rowwise"
    )
    
  SA_results2 <- case_data_SA %>%  
    filter(`Case Disposition` != "Rejected") %>%
    group_by(Category) %>%
    summarise("Intakes" = n()) %>%
    nest()
  
  SA_results <- cbind(SA_intakes,SA_results1,SA_results2)

# results_both %>%
#   select(-c(`Intake Case Types`, `Non-rejected Case Types`)) %>%
#   write.csv("Report 2020 All Districts.csv")