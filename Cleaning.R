library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)


#Production
production <- read.csv("Production_hourly_tech_2022_2023.csv", sep=";")
production <- production[c(1:4)]
production <- production %>% 
  rename(Date_Time = fecha_hora ,
         Technology = Nombre.Tecnología,
         Generation_MWh =Generación.Real..MWh.)

# check Solares 
production %>%
  filter(Technology == "Solares")
production <- production %>%
  filter(Technology != "Solares")

#long to wide 
production_wide <- spread(production, Technology, Generation_MWh)
#rename 
production_wide <- production_wide %>% 
  rename(Wind = Eólicas,
         Geothermal = Geotérmica,   
         Hydro = Hidroeléctricas, 
         Thermal= Termoeléctricas, 
         Hour = Hora)

production_wide$Date <- as.Date(production_wide$Date_Time, format = "%d-%m-%Y %H:%M")  

# Demand 
demand <- read.table("datos-de-demanda-sistemica-real.tsv", sep="\t",header=TRUE)

demand <- demand %>% 
  rename(Date = fecha,
         Hour = hora,
         Demand = demanda)


# merge 
merged_data <- merge(production_wide, demand, by = c("Date", "Hour"))
merged_data <- merged_data %>%
  arrange(Date, Hour)
merged_data <- merged_data %>%
  select(-Date_Time)

# checking merge values 
merged_data %>%
  filter(Date == "2022-12-01", Hour == 12)
demand %>%
  filter(Date == "2022-12-01", Hour == 12)
production_wide %>%
  filter(Date == "2022-12-01", Hour == 12)

merged_data_new <- merged_data %>%
  mutate(across(3:8, ~ gsub("\\.", "", .))) %>%
  mutate(across(3:8, ~ gsub(",", ".", .))) 

merged_data_new$Wind<- round(as.numeric(merged_data_new$Wind), 2)
merged_data_new$Geothermal<- round(as.numeric(merged_data_new$Geothermal), 2)
merged_data_new$Hydro<- round(as.numeric(merged_data_new$Hydro), 2)
merged_data_new$Geothermal<- round(as.numeric(merged_data_new$Geothermal), 2)
merged_data_new$Solar<- round(as.numeric(merged_data_new$Solar), 2)
merged_data_new$Thermal<- round(as.numeric(merged_data_new$Thermal), 2)
merged_data_new$Demand<- round(as.numeric(merged_data_new$Demand), 2)

colnames(merged_data_new) <- tolower(colnames(merged_data_new))


#####Solar capacity factor
#Taking table from ITD-PNCP-Jul-2022 and selecting first table for all regions 
solar_cap_data <- read_excel("ITD-PNCP-Jul-2022.xlsx", sheet = "Tabla 17-18-19", range = "B5:O17")

solar_cap <- rbind(solar_cap_data, solar_cap_data)
solar_cap <- solar_cap[order(solar_cap[,1]),]
solar_cap <- solar_cap %>% mutate(hour = row_number()) #set index equal to each hour
solar_cap <- solar_cap %>% select(hour, everything())
#convert month names to numbers 
names(solar_cap)[4:12] <- month.name[4:12]
names(solar_cap)[13:15] <- month.name[1:3]
solar_cap <- solar_cap[-c(2:3)]
#change from wide to long
long_solar_cap <-  solar_cap %>%
  pivot_longer(cols = -hour, names_to = "month", values_to = "solar_cap") %>%
  mutate(month = match(month, month.name))
# Create column in merged data for month
merged_data_new$month<-  month(ymd(merged_data_new$date), label = FALSE)
#merge merged data with solar cap
merged_data_with_solar_cap <- merge(long_solar_cap, merged_data_new, by.x = c("hour", "month"), by.y = c("hour", "month"))


#### Wind capacity factor 
#Taking table from ITD-PNCP-Jul-2022 and selecting first table for all regions 
wind_cap_data <- read_excel("ITD-PNCP-Jul-2022.xlsx", sheet = "Tabla 15-16", range = "B5:O17")

wind_cap <- rbind(wind_cap_data, wind_cap_data)
wind_cap <- wind_cap[order(wind_cap[,1]),]
wind_cap <- wind_cap %>% mutate(hour = row_number()) #set index equal to each hour
wind_cap <- wind_cap %>% select(hour, everything())
#convert month names to numbers 
names(wind_cap)[4:12] <- month.name[4:12]
names(wind_cap)[13:15] <- month.name[1:3]
wind_cap <- wind_cap[-c(2:3)]
#change from wide to long
long_wind_cap <-  wind_cap %>%
  pivot_longer(cols = -hour, names_to = "month", values_to = "wind_cap") %>%
  mutate(month = match(month, month.name))
#merge merged data with solar cap
merged_data_with_solar_and_wind_cap <- merge(long_wind_cap, merged_data_with_solar_cap, by.x = c("hour", "month"), by.y = c("hour", "month"))




####Final prep
merged_data_final<- merged_data_with_solar_and_wind_cap[-c(9,6)]
#reorder columns 
merged_data_final <- merged_data_final[,c("date", "month" , "hour","wind_cap","solar_cap","geothermal", "hydro",  "thermal", "demand")]


#keep only 2022 dates 
merged_data_final <- subset(merged_data_final, date < "2023-01-01")
#check max and min
summary(merged_data_final$date)

#sort 
merged_data_final <- merged_data_final[with(merged_data_final, order(date, hour)),]

#write to csv 
write.csv(merged_data_final, "merged_data.csv", row.names = FALSE)

