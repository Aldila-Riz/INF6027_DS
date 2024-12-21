########################################################
#                                      
############# Introduction to Data Science - Script
#
#######################################################

#1. Data Pre-Processing (Wide data format)
# Load necessary libraries
library(readxl)   # To read Excel files (chosen data set)
library(dplyr)    # For data manipulation and cleaning
library(tidyr)    # For reshaping data (pivoting)

# Specify the file path for the Excel file (download file)
file_path <- "bicycle theft.xlsx" #File name of the chosen data set

# Function to load, clean, and transpose data from a specified sheet
transpose_data <- function(file_path, sheet_name, start_drop_rows, end_drop_rows) {
  
  # SECTION 1: Read the Excel sheet
  # Read the data from the specified sheet in the Excel file
  data <- read_excel(file_path, sheet = sheet_name)
  
  # SECTION 2: Clean the data by removing unwanted rows
  # Drop the rows between the specified range (start_drop_rows to end_drop_rows)
  data_cleaned_time_classification <- data %>%
    slice(-(start_drop_rows:end_drop_rows))  # Remove rows within the specified range
  
  # SECTION 3: Set appropriate column names
  # Set the first row as the column names (adjust for the header)
  data_cleaned_time_classification[1, 1] <- "Time Type"  # Set the first column header to 'Time Type'
  colnames(data_cleaned_time_classification) <- as.character(unlist(data_cleaned_time_classification[1, ]))  # Set column names from the first row
  
  # Remove the first row after setting the column names
  data_cleaned_time_classification <- data_cleaned_time_classification[-1, ]
  
  # SECTION 4: Select relevant columns
  # Retain only the first 9 columns of the cleaned data
  data_cleaned_time_classification <- data_cleaned_time_classification[c(1:9)]
  
  # SECTION 5: Filter out unwanted values from the "Time Type" column
  # Remove rows where the "Time Type" column contains specific values
  data_filtered <- data_cleaned_time_classification %>%
    filter(!`Time Type` %in% c("Unweighted base - number of incidents", 
                               "Morning/afternoon (unsure which)", 
                               "Evening/night (unsure which)", 
                               "Other - Other",
                               "No cost[note 12]",
                               "Mean cost[note 13][note 14]",
                               "Median cost[note 14][note 15]"))
  
  # SECTION 6: Convert data types
  # Convert all columns except the first one to numeric
  data_filtered <- data_filtered %>%
    mutate(across(-1, as.numeric))
  
  # SECTION 7: Extract year information from column names
  # Rename column names by extracting the first year (from column headers)
  colnames(data_filtered)[-1] <- gsub("^.*?(\\d{4}).*$", "\\1", colnames(data_filtered)[-1])
  
  # SECTION 8: Transpose the data to a long format
  # Reshape the data into a long format, with 'Year' as the key and the values as 'Value'
  transposed_data <- data_filtered %>%
    pivot_longer(
      cols = -1,                # Select all columns except the first
      names_to = "Year",        # Create a new column 'Year' for the year information
      values_to = "Value"       # Store the values from the data in a new column 'Value'
    ) %>%
    pivot_wider(
      names_from = `Time Type`,  # Convert 'Time Type' column into separate columns
      values_from = Value        # Populate the new columns with values from the 'Value' column
    ) %>% 
    mutate(Year = as.numeric(Year))  # Convert 'Year' column to numeric type
  
  # Return the transposed data
  return(transposed_data)
}

# SECTION 9: Apply the function to different sheets
# Call the function for each sheet in the Excel file, passing the appropriate arguments for each
time_classification <- transpose_data(file_path, "Table_1", 1, 7)
loc_classification <- transpose_data(file_path, "Table_2", 1, 7)
cost_classification <- transpose_data(file_path, "Table_3", 1, 7)

# SECTION 10: Summarize the results
# Display summary statistics for each of the transposed dataframes
summary(time_classification)
summary(loc_classification)
summary(cost_classification)

# 2. Data Pre-Processing (For long data format)

#Inport data set
Time<- read_excel("bicycle theft.xlsx", sheet = 4, range ="A9:L24" )
Where<- read_excel("bicycle theft.xlsx", sheet =5, range ="A9:L19" )
Cost<- read_excel("bicycle theft.xlsx", sheet =6, range ="A9:L20" )

#__explore data
str(Time)

#__renaming variables
Time<-Time %>% 
  rename("time"="...1")

Cost<-Cost %>% 
  rename("cost"="...1")

Where<-Where %>% 
  rename("where"="...1")
view(Time)

str(Time)

Time<-Time %>% 
  rename("2012"="Apr 2012 to Mar 2013",
         "2013"="Apr 2013 to Mar 2014",
         "2014"="Apr 2014 to Mar 2015",
         "2015"="Apr 2015 to Mar 2016",
         "2016"="Apr 2016 to Mar 2017",
         "2017"="Apr 2017 to Mar 2018",
         "2018"="Apr 2018 to Mar 2019",
         "2019"="Apr 2019 to Mar 2020",
         "2022"="Apr 2022 to Mar 2023 [note 2]")
Time<-Time[,1:10] #selecting column 1-10 from time variable

Time<-Time %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  mutate(time = gsub("\\[.*?\\]", "", time)) %>% 
  filter(time!="Unweighted base - number of incidents")

Cost<-Cost %>% 
  rename("2012"="Apr 2012 to Mar 2013",
         "2013"="Apr 2013 to Mar 2014",
         "2014"="Apr 2014 to Mar 2015",
         "2015"="Apr 2015 to Mar 2016",
         "2016"="Apr 2016 to Mar 2017",
         "2017"="Apr 2017 to Mar 2018",
         "2018"="Apr 2018 to Mar 2019",
         "2019"="Apr 2019 to Mar 2020",
         "2022"="Apr 2022 to Mar 2023[note 2]")
Cost<-Cost[,1:10] #selecting column 1-10 from cost variable

Cost<-Cost %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  mutate(cost = gsub("\\[.*?\\]", "", cost)) %>% 
  filter(cost!="Unweighted base - number of incidents")
Where<-Where %>% 
  rename("2012"="Apr 2012 to Mar 2013",
         "2013"="Apr 2013 to Mar 2014",
         "2014"="Apr 2014 to Mar 2015",
         "2015"="Apr 2015 to Mar 2016",
         "2016"="Apr 2016 to Mar 2017",
         "2017"="Apr 2017 to Mar 2018",
         "2018"="Apr 2018 to Mar 2019",
         "2019"="Apr 2019 to Mar 2020",
         "2022"="Apr 2022 to Mar 2023 [note 2]")

Where<-Where[,1:10] #selecting column 1-10 from where variable


Where<-Where %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  mutate(where = gsub("\\[.*?\\]", "", where)) %>% 
  filter(where!="Unweighted base - number of incidents")

# Reshape datasets to long format (for visualisation purposes)
cost_long <- Cost %>%
  pivot_longer(cols = 2:10, names_to = "Year", values_to = "value") %>% 
  filter(cost!="No cost")

time_long <- Time %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value")

where_long <- Where %>%
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "value")


#3. Exploratory Data Analysis 
#3.1.1 Time Series Plot - Time Classifications
ggplot(time_long, aes(x = Year, y = value, color = time)) +
  geom_line(size = 1) +                   # Add lines to represent the data points
  geom_point(size = 2) +                  # Add points for better clarity of data points
  labs(
    title = "Time Series Plot",           # Title of the plot
    x = "Year",                           # Label for the x-axis
    y = "Number of Bicycle Thefts",       # Label for the y-axis
    color = "Time of Theft"                      # Label for the legend that shows variable names
  ) +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,
                              2018,2019,2022))+
  theme(axis.text.x = element_text(angle = 45))
  theme_minimal()      
#3.1.2 Time Series Plot - Location Attempts 
  ggplot(location_long, aes(Year, value, color = location)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    labs(
      title = "Time Series Plot",
      x = "Year",
      y = "Number of Bicycle Thefts",
      color = "Location Attempts"
    ) +
    scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2022)) +
    scale_y_continuous(
      breaks = seq(0, max(location_long$value, na.rm = TRUE), by = 10)  # Interval setiap 10
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Sudut label x-axis
    )
  
  ggplot(where_long, aes(Year, value, color = where, group = where)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    labs(
      title = "Time Series Plot",
      x = "Year",
      y = "Number of Bicycle Thefts",
      color = "Location Attempts"
    ) +
    scale_y_continuous(
      breaks = seq(0, max(location_long$value, na.rm = TRUE), by = 10)  # Interval setiap 10
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Sudut label x-axis
    )
  
#3.1.3 Time Series Plot - Cost of Stolen Item 

# Filter out "mean" and "median" costs and create the plot
unique(cost_long$cost) #checking the variable format 

library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)

cost_long %>%
  mutate(cost = str_trim(cost)) %>%        # Remove whitespace
  filter(!cost %in% c("Mean cost", "Median cost")) %>% # Filter out variables
  ggplot(aes(x = Year, y = value, color = cost, group = cost)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis_d()+
  labs(
    title = "Time Series Plot",
    x = "Year",
    y = "Number of Bicycle Thefts",
    color = "Cost of Stolen Bike"
  ) +
  theme_minimal()
#3.2. Understand trend & relationship using scatter plot
#3.2.1. Time Classification 
time_long %>% 
  filter(!time %in% c("Morning/afternoon (unsure which)", 
                      "Evening/night (unsure which)")) %>% # Filter out variables
  ggplot(aes(Year, value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~time, scales = "free_y") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019, 2022)) +
  labs(
    title = "Trends of Bicycle Thefts Based on Time Classification",
    x = "Year",
    y = "Number of Bicycle Thefts"
  ) +
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5))
#3.2.2. Location Attempts 

where_long %>%
  filter(!where %in% c("Other - Other")) %>% # Apply filter to the data frame
  ggplot(aes(Year, value)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~where, scales = "free_y") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019, 2022)) +
  labs(
    title = "Trends of Bicycle Thefts Based on Location Attempts",
    x = "Year",
    y = "Number of Bicycle Thefts"
  ) +
  theme(
    axis.text.x = element_text(angle = 45),
    plot.title = element_text(hjust = 0.5)
  )


#3.2.3. Cost of stolen items 

cost_long%>% 
  filter(!cost %in% c("Mean cost", "Median cost")) %>% 
  ggplot(aes(as.integer(Year),value))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_wrap(~cost, scales = "free_y")+
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,
                              2018,2019,2022))+
  labs(
    title = "Trends of Bicycle Thefts Based on Cost of Bike",
    x = "Year",
    y = "Number of Bicycle Thefts") + 
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5))


#3.3 Correlation Analysis 
#3.3.1. Find the correlation of time classification with year
time_long %>% 
  group_by(time) %>% 
  summarize(correlation=cor(Year,value, use="complete.obs")) %>% 
  write.csv("time_correlation.csv") #save the correlation result
#3.3.2. Find the correlation of cost of stolen item with year
cost_long %>% 
  group_by(cost) %>% 
  summarise(correlation=cor(as.integer(Year),value, 
                            use="complete.obs")) %>%
  write.csv("Cost_correlation.csv")

#3.3.3. Find the correlation of location attempts with year
location_long<-pivot_longer(data=loc_classification, cols = 2:9, 
                            names_to = "location", 
                            values_to = "value")

location_long %>% 
  group_by(location) %>% 
  summarise(correlation=cor(Year, value)) %>% 
  write.csv("Location_correlation.csv")

  
  

#3.4. Logistic Regression 
# Filter chosen locations
filtered_data_location <- where_long %>%
  filter(where %in% c(
    "Home - Semi-private",
    "Other - Car park",
    "Other - Grounds of a public place",
    "Other - Street",
    "Work - Car park"
  ))

# Pivot to wide format
wide_data_l <- filtered_data_location %>%
  pivot_wider(names_from = where, values_from = value)

#split data (train and test data)



# Add total thefts column to training data
wide_data_l <- wide_data_l %>%
  mutate(total_thefts = rowSums(select(., `Home - Semi-private`, 
                                       `Work - Car park`, 
                                       `Other - Car park`, 
                                       `Other - Grounds of a public place`, 
                                       `Other - Street`), 
                                na.rm = TRUE))

install.packages("rsample")

library(rsample)

# Splitting data (80% training, 20% testing)
set.seed(123)  # For reproducibility
data_split <- initial_split(wide_data_l, prop = 0.8)

train_data_l <- training(data_split)
test_data_l <- testing(data_split)


model_l <- lm(total_thefts ~ Year, data = train_data_l)

# Predictions
test_data_l <- test_data_l %>%
  mutate(predicted_thefts = predict(model_l, newdata = test_data_l))

# Calculate performance metrics
# Load yardstick for metrics
install.packages ("yardstick")
library(yardstick)

# Calculate evaluation metrics
metrics <- metrics(test_data_l, truth = total_thefts,
                   estimate = predicted_thefts)
print(metrics) #R2 indicates a good fit =1

# Define the range of future years (e.g., 2023 to 2027)
future_years <- data.frame(Year = 2023:2027)

# Predict future bicycle thefts based on the existing model
future_predictions_l <- predict(model_l, newdata = future_years)

# Combine predictions with the future years
future_data_l <- data.frame(Year = 2023:2027, 
                            predicted_thefts = future_predictions_l)

#Visualisation of predicted values using time series 

# Combine actual, predicted, and future predictions
plot_data_l <- bind_rows(
  test_data_l %>% mutate(predicted_thefts = 
                           predict(model_l, newdata = test_data_l)),
                          future_data_l)

# Create the time series plot
ggplot(plot_data_l, aes(x = Year)) +
  geom_line(aes(y = total_thefts, color = "Actual"), size = 1) +  # Actual values
  geom_line(aes(y = predicted_thefts, color = "Predicted"), size = 1, linetype = "dashed") +  # Predicted values
  labs(title = "Actual vs Predicted Bicycle Thefts",
       x = "Year",
       y = "Total Thefts",
       color = "Legend") +
  theme_minimal()


future_data_l<-future_data_l %>%
  mutate(total_thefts = NA) %>% view()


plot_data_l <- bind_rows(
  test_data_l %>% select(Year, total_thefts, predicted_thefts),
  future_data_l %>% select(Year, total_thefts, predicted_thefts)
)

ggplot(plot_data_l, aes(x = Year)) +
  geom_line(aes(y = total_thefts, color = "Actual"), size = 1, na.rm = TRUE) +  # Nilai aktual
  geom_line(aes(y = predicted_thefts, color = "Predicted"), size = 1, linetype = "dashed") +  # Nilai prediksi
  labs(title = "Actual vs Predicted Bicycle Thefts (Including Future Predictions)",
       x = "Year",
       y = "Total Thefts",
       color = "Legend") +
  theme_minimal()

