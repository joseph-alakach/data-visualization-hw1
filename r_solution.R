library(dplyr)
library(lubridate)

# **PART1**
# --1
crime_data <- read.csv("crime_data.csv")
head(crime_data, 5)

# --2
crime_data_cleaned <- crime_data %>%
  select(which(  colSums(is.na(.)) / nrow(.) <= 0.5))

# --3
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(DATE.OCC=as.Date(DATE.OCC, format = '%m/%d/%Y %I:%M:%S %p') )

crime_data_cleaned <- crime_data_cleaned %>%
  mutate(Year = lubridate::year(DATE.OCC))
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(Month = lubridate::month(DATE.OCC))
crime_data_cleaned <- crime_data_cleaned %>%
  mutate(Day = lubridate::day(DATE.OCC))

crime_data_cleaned <- crime_data_cleaned %>%
  mutate(TIME.OCC = sprintf("%04d", TIME.OCC))  # Ensure 4-digit format

crime_data_cleaned <- crime_data_cleaned %>%
  mutate(HOUR = paste0(substr(TIME.OCC, 1, 2),':',substr(TIME.OCC, 3, 4))) # get the first 2 digits (Hour), get the last 2 digits (Minutes)

# --4
c_2023_burg <- crime_data_cleaned %>%
  filter(Year == 2023) %>%
    filter(Crm.Cd.Desc == 'BURGLARY')

# --5
group_area_name <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
    summarise(count= n(),  averge= mean(Vict.Age))



# **PART3**
# --1
group_month <- crime_data_cleaned %>%
  group_by(Month) %>%
    summarise(count= n())

# --2
crime_weapon_count <- length(crime_data_cleaned$Weapon.Desc[!is.na(crime_data_cleaned$Weapon.Desc) & crime_data_cleaned$Weapon.Desc != ""])

# --3
group_premis_desc <- crime_data_cleaned %>%
  group_by(Premis.Desc) %>%
    summarise(count= n())



# **PART4**
group_severity_score <- crime_data_cleaned %>%
  group_by(AREA.NAME) %>%
    mutate(Severity.Score = 0 + (if_else(Weapon.Desc!= "", 5,0))+(if_else(Crm.Cd.Desc == "BURGLARY",3,0))+ (if_else(Weapon.Desc == "" & Crm.Cd.Desc != "BURGLARY", 1, 0)) ) %>%
      summarise(count= sum(Severity.Score))





