

# RangeX tomst logger data exploration ------------------------------------



# Load necessary libraries ------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


# read data one logger high site ------------------------------------------

d <- read.csv2("C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/Data_2024/Climate/TOMST_growing_season_24/High_site/data_94217301_2024_10_17_0.csv")

d


head(d)


# Ensure the temperature column is numeric
d$X21.125 <- as.numeric(as.character(d$X21.125))

# Convert time column to datetime format
d$time <- as.POSIXct(d$X2024.05.01.00.00, format="%Y.%m.%d %H:%M")

# Create ggplot for temperature (X21) vs time
ggplot(d, aes(x = time, y = X21.125)) +
  geom_point() +    #
  labs(title = "Temperature vs Time",
       x = "Time",
       y = "Temperature (\u00B0C)") +
  scale_y_continuous(breaks = pretty(range(d$X21.125, na.rm = TRUE), n = 5)) +  # Adjust breaks dynamically
  theme_minimal()


# Plot three temperatures -------------------------------------------------

## need to read d again
d <- read.csv2("C:/Users/naart3294/OneDrive - University of Bergen/PhD_RangeX/Data_2024/Climate/TOMST_growing_season_24/High_site/data_94217301_2024_10_17_0.csv")

## make a column with correct time
# Convert time column to datetime format
d$time <- as.POSIXct(d$X2024.05.01.00.00, format="%Y.%m.%d %H:%M")


# Reshape the data: gather temperature columns into key-value pairs
d_long <- d %>%
  pivot_longer(cols = c(X21, X21.1, X21.125),  # Specify the temperature columns
               names_to = "Temperature_Type",  # New column name for temperature types
               values_to = "Temperature")      # New column for temperature values

# Ensure Temperature is numeric
d_long$Temperature <- as.numeric(as.character(d_long$Temperature))

# Create the ggplot
ggplot(d_long, aes(x = time, y = Temperature, color = Temperature_Type)) +
  geom_point() +    # Points for each observation
  labs(title = "Temperature vs Time",
       x = "Time",
       y = "Temperature (\u00B0C)") +
  scale_y_continuous(breaks = pretty(range(d_long$Temperature, na.rm = TRUE), n = 5)) +  # Adjust breaks
  theme_minimal() +
  theme(legend.title = element_blank())   # Optional: remove the legend title



# import data high site ---------------------------------------------------


# List all files in the 'high' folder that start with 'data'
high_files <- list.files(path = "Data/High_site/", pattern = "^data_\\d+.*\\.csv$", full.names = TRUE)

high_files

# Import each file and combine them into a single data frame
# Define column names based on your data structure
column_names <- c("number", "Date", "Column1", "Temp1", "Temp2", "Temp3", "Soilmoisture", "Column6", "Column7")


# Function to extract the number from the filename
extract_number <- function(file) {
  str_extract(basename(file), "\\d+")
}

# Read all files, specifying there is no header and setting the column names manually

# Read all files, add a column for the file number
high <- high_files %>%
  map_dfr(~read_delim(.x, col_names = column_names) %>%
            mutate(tomst = extract_number(.x)))




# read plot codes 24 ------------------------------------------------------

plot_codes_24 <- read.csv2("Data/High_site/RangeX_tomst_plot_code_2024_high.csv")

plot_codes_24



# combine plot codes with data --------------------------------------------

plot_codes_24 <- plot_codes_24 %>% mutate(tomst = as.character(tomst))

# Join all_data with plot_codes_24 based on the tomst columns
tomst_high <- high_tomst %>%
  left_join(plot_codes_24, by = c("tomst" = "tomst"))


# rename ------------------------------------------------------------------

# Add treat_warming and treat_competition columns based on treat
tomst_high <- tomst_high %>%
  mutate(
    treat_warming = case_when(
      treat == "A" ~ "warm",
      treat == "B" ~ "ambi",
      treat == "C" ~ "warm",
      treat == "D" ~ "ambi",
      treat == "E" ~ "warm",
      treat == "F" ~ "ambi"
    ),
    treat_competition = case_when(
      treat == "A" ~ "vege",
      treat == "B" ~ "vege",
      treat == "C" ~ "control",
      treat == "D" ~ "control",
      treat == "E" ~ "bare",
      treat == "F" ~ "bare"
    )
  )

head(tomst_high)

# Remove columns that are entirely NA
tomst_high <- tomst_high %>%
  select(where(~ !all(is.na(.))))



# get temperature data ----------------------------------------------------

# Extract temperature columns into a new data frame
high_temperature <- tomst_high %>%
  select(tomst, Date, Temp1, Temp2, Temp3, block, treat, treat_warming, treat_competition)  # Adjust if your temp columns have different names

# View the first few rows of the extracted temperature data
head(high_temperature)

summary(high_temperature)



# Convert the Date column to Date format (if it's not already)
high_temperature$Date <- as.Date(high_temperature$Date, format = "%Y.%m.%d %H:%M")

# Create the plot for Temp1 per logger
ggplot(high_temperature, aes(x = Date, y = Temp1, color = tomst)) +
  geom_line() +
  labs(title = "Temperature (Temp1) per Logger Over Time",
       x = "Date",
       y = "Temperature (°C)",
       color = "Logger ID") +
  theme_minimal() +
  theme(legend.position = "right")



# define time period field season -----------------------------------------

# Define the date range
start_date <- as.Date("2024-05-31")
end_date <- as.Date("2024-10-15")

# Filter the data for the specified date range
high_filtered_temperature <- high_temperature %>%
  filter(Date >= start_date & Date <= end_date)
head(high_filtered_temperature)

# Create the plot for Temp1 per logger with the filtered data
ggplot(high_filtered_temperature, aes(x = Date, y = Temp1, color = tomst)) +
  geom_line() +
  labs(title = "Temperature (Temp1) per Logger from 31.05.2024 to 15.10.2024",
       x = "Date",
       y = "Temperature (°C)",
       color = "Logger ID") +
  theme_minimal() +
  theme(legend.position = "right")



# Group by treatment wamr, ambi and calculate average temperature --------------------

high_average_temperature <- high_filtered_temperature %>%
  group_by(Date, treat) %>%
  summarize(avg_temp = mean(Temp1, na.rm = TRUE), .groups = 'drop')
head(high_average_temperature)

# Create the plot for average temperature per treatment
ggplot(high_average_temperature, aes(x = Date, y = avg_temp, color = treat)) +
  geom_line() +
  labs(title = "Average Temperature (Temp1) per Treatment from 31.05.2024 to 15.10.2024",
       x = "Date",
       y = "Average Temperature (°C)",
       color = "Treatment") +
  theme_minimal() +
  theme(legend.position = "right")




# compare temp1 ambi and warm ---------------------------------------------

# Filter and group the data by Date and treat_warming
average_temp_time_series <- high_filtered_temperature %>%
  filter(treat_warming %in% c("warm", "ambi")) %>%  # Keep only warm and ambi
  group_by(Date, treat_warming) %>%  # Group by Date and treat_warming
  summarize(avg_temp = mean(Temp1, na.rm = TRUE), .groups = 'drop')  # Calculate average temperature

# Create the time series plot for average temperature per treatment group
high_plot <- ggplot(average_temp_time_series, aes(x = Date, y = avg_temp)) +
  geom_line(data = filter(average_temp_time_series, treat_warming == "warm"),
            aes(color = "warm"), size = 1) +  # Set color for warm
  geom_line(data = filter(average_temp_time_series, treat_warming == "ambi"),
            aes(color = "ambi"), size = 1) +  # Set color for ambi
  scale_color_manual(values = c("warm" = "red", "ambi" = "blue")) +  # Define colors
  labs(title = "Average Temperature Over Time for Warm and Ambi Treatments",
       x = "Date",
       y = "Average Temperature (°C)",
       color = "Treatment Group") +
  theme_bw() +
  theme(legend.position = "right")

high_plot

ggsave("high_average_temperature_time_series.png", plot = high_plot, width = 10, height = 6, dpi = 300)



# are the OTCs working? ---------------------------------------------------

# Filter for the specified date range and treatments
comparison_data <- high_filtered_temperature %>%
  filter(Date >= as.Date("2024-05-31") & Date <= as.Date("2024-10-15"),
         treat_warming %in% c("warm", "ambi"))


# Perform a t-test to compare average temperatures between warm and ambi
t_test_result <- t.test(Temp1 ~ treat_warming, data = comparison_data, var.equal = TRUE)
print(t_test_result)

# p-value < 2.2e-16 -> there is a significant difference in mean temperatures between the two treatments.

# Create a boxplot for Temp1 by treatment group
boxplot_temp <- ggplot(comparison_data, aes(x = treat_warming, y = Temp1, fill = treat_warming)) +
  geom_boxplot() +
  labs(title = "Temperature Comparison: Warm vs. Ambi",
       x = "Treatment Group",
       y = "Temperature (°C)") +
  scale_fill_manual(values = c("warm" = "red", "ambi" = "blue")) +
  theme_bw()

# Display the boxplot
print(boxplot_temp)

# Calculate average temperature for each treatment group over the date range
avg_temp_comparison <- comparison_data %>%
  group_by(Date, treat_warming) %>%
  summarize(avg_temp = mean(Temp1, na.rm = TRUE), .groups = 'drop')

# Create the time series plot
time_series_plot <- ggplot(avg_temp_comparison, aes(x = Date, y = avg_temp, color = treat_warming)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("warm" = "red", "ambi" = "blue")) +
  labs(title = "Average Temperature Over Time: Warm vs. Ambi",
       x = "Date",
       y = "Average Temperature (°C)",
       color = "Treatment Group") +
  theme_bw() +
  theme(legend.position = "right")

# Display the time series plot
print(time_series_plot)



# check for outliers ------------------------------------------------------

# View summary statistics to identify high values
summary(comparison_data$Temp1)

# Check specific rows with high temperature values
high_temp_values <- comparison_data %>%
  filter(Temp1 > 50)

print(high_temp_values)

# Create a histogram to visualize temperature values
ggplot(comparison_data, aes(x = Temp1)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Temperature Distribution",
       x = "Temperature (°C)",
       y = "Frequency") +
  theme_bw()

# Calculate IQR
Q1 <- quantile(comparison_data$Temp1, 0.25)
Q3 <- quantile(comparison_data$Temp1, 0.75)
IQR <- Q3 - Q1

# Define upper threshold for outliers
upper_threshold <- Q3 + 1.5 * IQR

# Filter out outliers
cleaned_comparison_data <- comparison_data %>%
  filter(Temp1 <= upper_threshold)

# Check the summary statistics again after cleaning
summary(cleaned_comparison_data$Temp1)

# Perform a t-test on cleaned data
t_test_result_cleaned <- t.test(Temp1 ~ treat_warming, data = cleaned_comparison_data, var.equal = TRUE)
print(t_test_result_cleaned)

# Create boxplot and time series plots as before
# (Refer to the previous code snippets for creating these plots)




