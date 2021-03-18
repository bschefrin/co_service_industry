

# Loading libraries

library("pacman")

p_load("tidyverse", "knitr", "lubridate", "janitor", "zoo", "viridis", "scales")

# Data & Data Cleaning-------------------------------------------------------------------------------------


# Gathering CO employment data from 2018-2020 and binding data frames into single data frame
# All data can be found from the CO Department of Labor at the website below.
# https://www.colmigateway.com/vosnet/lmi/default.aspx

co_ces_2018 <- read_csv("C:/Users/Jake/Desktop/r_projects/co_labor_data/co_ces_data/co_ces_2018.csv")

co_ces_2018_1<- co_ces_2018 %>% 
  select(area, areaname, seriescode, seriestitle, Timeperiod, CURempces, PMempces)

co_ces_2019 <- read_csv("C:/Users/Jake/Desktop/r_projects/co_labor_data/co_ces_data/co_ces_2019.csv")

co_ces_2019_1<- co_ces_2019 %>% 
  select(area, areaname, seriescode, seriestitle, Timeperiod, CURempces, PMempces)

co_ces_2020 <- read_csv("C:/Users/Jake/Desktop/r_projects/co_labor_data/co_ces_data/co_ces_2020.csv")

co_ces_2020_1<- co_ces_2020 %>% 
  select(area, areaname, seriescode, seriestitle, Timeperiod, CURempces, PMempces)

# Binding CO employment data from 2018-2020 into one data frame 

bind_one <- rbind(co_ces_2018_1, co_ces_2019_1)

co_ces_2018_2020 <- rbind(bind_one, co_ces_2020_1)


# Renaming column titles and cleaning them. 
co_ces_2018_2020_1 <- clean_names(co_ces_2018_2020) %>% 
  rename("location" = "areaname",
         "industry" = "seriestitle",
         "time" = "timeperiod",
         "current_emp" = "cu_rempces",
         "previous_emp" = "p_mempces")

# Using lubridate to switch time column from character to time

co_ces_2018_2020_2 <- co_ces_2018_2020_1 %>% 
  mutate(time =   mdy(time) - days(19))

# Removing unnecessary data frames from environment

co_ces_data <- co_ces_2018_2020_2  

rm(list = setdiff(ls(),"co_ces_data"))

write_csv(co_ces_data, "C:/Users/Jake/Desktop/r_projects/co_labor_data/co_ces_data/co_ces_data.csv")

# Leisure & Hospitality industries all MSAs------------------------------------------------------------------


# Data frame that has all hospitality service data 
hl_target <- c("Leisure and Hospitality", "Arts, Entertainment, and Recreation", 
               "Amusement, Gambling, and Recreation Indu", "Other Amusement and Recreation Industrie",
               "Accommodation", "Food Services and Drinking Places", "Full-Service Restaurants",
               "Limited-Service Eating Places")


co_ces_service_industry_data <- co_ces_data %>% 
  filter(industry %in% hl_target)

# Data frame with Leisure and hospitality data from all CO MSAs and corresponding graph 
# Removing state-wide data and inserting data from all non MSA regions
co_ces_hospitality <- co_ces_data %>% 
  select(location, industry, time, current_emp) %>% 
  filter(industry == "Leisure and Hospitality") %>% 
  pivot_wider(names_from = location, values_from = current_emp) 

co_ces_hospitality_2 <- clean_names(co_ces_hospitality) %>% 
  mutate(rest_of_state = colorado - greeley_msa - fort_collins_loveland_msa - denver_aurora_msa -
           colorado_springs_msa - boulder_longmont_msa - pueblo_msa - grand_junction_msa) %>% 
  select(time, rest_of_state) %>% 
  pivot_longer(rest_of_state, names_to = "location", values_to = "current_emp")
  
co_ces_hospitality_bind <- co_ces_data %>% 
  filter(industry == "Leisure and Hospitality") %>% 
  select(time, location, current_emp)

co_ces_hospitality_msa_data <- rbind(co_ces_hospitality_bind, co_ces_hospitality_2) %>% 
  filter(location != "Colorado") %>% 
  mutate(location = if_else(location == "rest_of_state", "Rest of State", location))
  
# Leisure and Hospitality Graph all state locations

lh_msa <- ggplot(data = co_ces_hospitality_msa_data) +
  geom_line(mapping = aes(x = time, y = current_emp, group = location, color = location), size = 1) +
  labs(title = "CO Leisure & Hospitality Employment 2018-2020", subtitle = "Note: Values for Dec 2020 are predicted values and subject to change",
       x = "Date", y = "# of Persons Employed (Thousands)") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), color = "darkorange1", size = 1, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2019-12-01"), y = 190000, label = "COVID closures ordered 3/16", size = 3) +
  annotate(geom = "text", x = as.Date("2020-09-01"), y = 190000, label = "Limited in person \ndining resumes 5/25", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), color = "purple4", size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = c(25000, 50000, 75000, 100000, 125000, 150000, 175000),labels = unit_format(unit = NULL, scale = 1e-3)) +
  scale_color_viridis(name = "Locations", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# I want a close up that doesn't include Denver-Aurora MSA and Rest of state to get a
# better visual of other MSA areas in CO

co_ces_hospitality_msa_data_2 <- co_ces_hospitality_msa_data %>% 
  filter(location != "Denver - Aurora MSA") %>% 
  filter(location != "Rest of State")

lh_msa_2 <- ggplot(data = co_ces_hospitality_msa_data_2) +
  geom_line(mapping = aes(x = time, y = current_emp, group = location, color = location), size = 1) +
  labs(title = "CO Leisure & Hospitality Employment 2018-2020 (Denver - Aurora MSA & Rest of State omitted)", subtitle = "Note: Values for Dec 2020 are predicted values and subject to change",
       x = "Date", y = "# of Persons Employed (Thousands)") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), color = "darkorange1", size = 1, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2019-12-01"), y = 47000, label = "COVID closures ordered 3/16", size = 3) +
  annotate(geom = "text", x = as.Date("2020-09-01"), y = 47000, label = "Limited in person \ndining resumes 5/25", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), color = "purple4", size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = c(10000, 20000, 30000, 40000),labels = unit_format(unit = NULL, scale = 1e-3)) +
  scale_color_viridis(name = "Locations", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 


# Clean-up of environment

rm(co_ces_hospitality, co_ces_hospitality_2, co_ces_hospitality_bind)

# Bars & Restaurant data -----------------------------------------------------------------------------

# Data frame with all restaurant related data Also seperating full service restaurant category by series code
# because it is named twice but series code is different. Series code that I am switching is
# 70722500

hl_restaurant <- c("Food Services and Drinking Places", "Full-Service Restaurants",
                   "Limited-Service Eating Places")

co_ces_restaurant <- co_ces_data %>% 
  filter(industry %in% hl_restaurant & location == "Colorado") %>%
  mutate(industry = if_else(seriescode == "70722500", "Restaurants", industry))

# Isolating drinking establishments from restaurants and putting it back into restaurant dataframe
# for more detailed picture of CO restaurant industry labor

co_ces_drinking_establishments <- co_ces_restaurant %>% 
  select(seriescode, industry, time, current_emp) %>% 
  filter(seriescode == "70722000" | seriescode == "70722500") 
  
co_ces_drinking_establisments_1 <- co_ces_drinking_establishments %>% 
  select(industry, time, current_emp) %>% 
  pivot_wider(names_from = industry, values_from = current_emp) %>% 
  rename("food_services_and_drinking_places" = "Food Services and Drinking Places",
         "restaurants" = "Restaurants") %>% 
  mutate(drinking_establishments = food_services_and_drinking_places - restaurants) %>% 
  select(time, drinking_establishments) %>% 
  pivot_longer(drinking_establishments, names_to = "industry", values_to = "current_emp")
  
co_ces_restaurant_bind <- co_ces_restaurant %>% 
  select(time, industry, current_emp) 

co_ces_restaurant_data <-rbind(co_ces_restaurant_bind, co_ces_drinking_establisments_1)

co_ces_restaurant_type_data <- co_ces_restaurant_data %>% 
  filter(industry == "drinking_establishments" | industry == "Limited-Service Eating Places" | 
           industry == "Full-Service Restaurants")

# Graph of all restaurant data in CO

restaurant_type_graph <- ggplot(data = co_ces_restaurant_type_data) +
  geom_line(mapping = aes(x = time, y = current_emp, group = industry, color = industry), size = 1) +
  labs(title = "CO Bar & Restaurant Employment 2018-2020", subtitle = "Note: Values for Dec 2020 are predicted values and subject to change",
       x = "Date", y = "# of Persons Employed (Thousands)")+
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), color = "darkorange1", size = 1, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2019-11-01"), y = 128000, label = "COVID closures ordered 3/16", size = 3) +
  annotate(geom = "text", x = as.Date("2020-09-01"), y = 126000, label = "Limited in person \ndining resumes 5/25", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), color = "purple4", size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = c(25000, 50000, 75000, 100000, 125000),labels = unit_format(unit = NULL, scale = 1e-3)) +
  scale_color_viridis(name = "Type", labels = c("Drinking Establishments",
                                                "Full-Service Restaurants", "Limited-Service Eating Places"),
                      discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Clean up of environment

rm(hl_restaurant, hl_target, co_ces_restaurant, co_ces_restaurant_bind, 
   co_ces_restaurant_data, co_ces_drinking_establishments, co_ces_drinking_establisments_1)

# Accommodation ------------------------------------------------------------------------------------            

co_ces_accommodation <- co_ces_service_industry_data %>% 
  filter(industry == "Accommodation") %>% 
  select(time, location, current_emp) 

# Getting rest of state information, eliminating colorado from location variable
co_ces_accommodation_2 <- co_ces_accommodation %>% 
  pivot_wider(names_from = location, values_from = current_emp) %>% 
  clean_names() %>% 
  mutate(rest_of_state = colorado - denver_aurora_msa) %>% 
  select(time, rest_of_state) %>% 
  pivot_longer(rest_of_state, names_to = "location", values_to = "current_emp")

co_ces_accommodation_data <- rbind(co_ces_accommodation, co_ces_accommodation_2) %>% 
  filter(location != "Colorado") %>% 
  mutate(location = if_else(location == "rest_of_state", "Rest of State", location))

# Accommodation Graph

accommodation_graph <- ggplot(data = co_ces_accommodation_data) +
  geom_line(mapping = aes(x = time, y = current_emp, group = location, color = location), size = 1) +
  labs(title = "CO Accommodation Employment 2018-2020", subtitle = "Note: Values for Dec 2020 are predicted values and subject to change",
       x = "Date", y = "# of Persons Employed (Thousands)") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), color = "darkorange1", size = 1, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2020-01-01"), y = 40000, label = "COVID closures \nordered 3/16", size = 3) +
  annotate(geom = "text", x = as.Date("2020-09-01"), y = 40000, label = "Limited in person \ndining resumes 5/25", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), color = "purple4", size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = c(10000, 20000, 30000, 40000),labels = unit_format(unit = NULL, scale = 1e-3)) +
  scale_color_viridis(name = "Locations", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Cleanup of environment

rm(co_ces_accommodation, co_ces_accommodation_2)

# Arts Recreation Entertainment--------------------------------------------------------------------------


co_ces_are <- co_ces_service_industry_data %>% 
  filter(industry == "Arts, Entertainment, and Recreation") %>% 
  select(time, location, current_emp)

co_ces_are_2 <- co_ces_are %>% 
  pivot_wider(names_from = location, values_from = current_emp) %>% 
  clean_names() %>% 
  mutate(rest_of_state = colorado - denver_aurora_msa) %>% 
  select(time, rest_of_state) %>% 
  pivot_longer(rest_of_state, names_to = "location", values_to = "current_emp")

co_ces_are_data <- rbind(co_ces_are, co_ces_are_2) %>% 
  filter(location != "Colorado") %>% 
  mutate(location = if_else(location == "rest_of_state", "Rest of State", location))

are_graph <- ggplot(data = co_ces_are_data) +
  geom_line(mapping = aes(x = time, y = current_emp, group = location, color = location), size = 1) +
  labs(title = "CO Arts, Entertainment, & Recreation Employment 2018-2020", subtitle = "Note: Values for Dec 2020 are predicted values and subject to change",
       x = "Date", y = "# of Persons Employed (Thousands)") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-01")), color = "darkorange1", size = 1, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2020-01-01"), y = 40000, label = "Ski resort closures \nordered 3/14", size = 3) +
  annotate(geom = "text", x = as.Date("2020-09-01"), y = 40000, label = "Limited in person \ndining resumes 5/25", size = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")), color = "purple4", size = 1, linetype = "dashed") +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = c(10000, 20000, 30000, 40000),labels = unit_format(unit = NULL, scale = 1e-3)) +
  scale_color_viridis(name = "Locations", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
  
