# Set the working directory and verify it
setwd("E:\\VCU\\Summer 2024\\Statistical Analysis & Modeling")
getwd()

# Load necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(BSDA)
library(readr)
library(glue)

# Read the file into R
data <- read.csv("NSSO68.csv")

# Filter for Kerala
kl_df <- data %>% filter(state_1 == "KE")

# Display dataset info
cat("Dataset Information:\n")
print(names(kl_df))
print(head(kl_df))
print(dim(kl_df))

# Finding missing values
missing_info <- colSums(is.na(kl_df))
cat("Missing Values Information:\n")
print(missing_info)

any(is.na(kl_df))
sum(is.na(kl_df))

# Sub-setting the data
klnew <- kl_df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(klnew)))

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
klnew$Meals_At_Home <- impute_with_mean(klnew$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(klnew)))

# Find outliers and removing them
boxplot(klnew$ricepds_v)
remove_outliers <- function(kl_df, column_name) {
  Q1 <- quantile(kl_df[[column_name]], 0.25)
  Q3 <- quantile(kl_df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  kl_df <- subset(kl_df, kl_df[[column_name]] >= lower_threshold & kl_df[[column_name]] <= upper_threshold)
  return(kl_df)
}


outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  klnew <- remove_outliers(klnew, col)
}

# Renaming districts and sectors
district_mapping <- c( "1" = "Kasargod", "2" = "Kannur", "3" = "Wayanad", "4" = "Kozhikode", "5" = "Malappuram", "6" = "Palakkad", "7" = "Thrissur", "8" = "Ernakulam", "9" = "Idukki", "10" = "Kottayam", "11" = "Alappuzha", "12" = "Pathanamthitta", "13" = "Kollam", "14" = "Thiruvananthapuram")
sector_mapping <- c("1" = "Rural", "2" = "Urban")

klnew$District <- as.character(klnew$District)
klnew$Sector <- as.character(klnew$Sector)
klnew$District <- ifelse(klnew$District %in% names(district_mapping), district_mapping[klnew$District], klnew$District)
klnew$Sector <- ifelse(klnew$Sector %in% names(sector_mapping), sector_mapping[klnew$Sector], klnew$Sector)

# Summarize consumption
klnew$total_consumption <- rowSums(klnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

View(klnew)

hist(klnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Kerala State")

klnew_consumption <- aggregate(total_consumption ~ District, data = klnew, sum) 
View(klnew_consumption)

barplot(klnew_consumption$total_consumption, 
        names.arg = klnew_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) 

#(b) Plot on the Kerala state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C:\\Users\\Ferah Shan\\Downloads\\KERALA_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 

# Merge klnew_consumption with data_map
data_map_data <- data_map %>%
  left_join(klnew_consumption, by = c("District" = "District"))

library(sf)

# Reproject data_map to UTM zone 45N
data_map_utm <- st_transform(data_map, crs = 32645)

# Merge klnew_consumption with data_map_utm
data_map_data_utm <- data_map_utm %>%
  left_join(klnew_consumption, by = c("District" = "District"))

# Plot the data
ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "red", high = "green") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")
