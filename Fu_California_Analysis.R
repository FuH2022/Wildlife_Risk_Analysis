##### California Wildlife Hazard Analysis #####
##### By Haoruo Mengyi and Dr. Lu #####

##### Notice #####
## This is just the analysis used in the paper. Most of the following code ##
## are just used for this analysis. ##
## For Shiny.io part, please refer to the second part of the code ##
## There will be some duplicates in this code, please check carefully ##

# Necessary packages
library(readxl)
library(gdata)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(tmap)
library(sf)
library(viridis)
library(cluster)
library(spdep)
library(gridExtra)
library(corrplot)



# Import Data We selected LAX and SAC data
# Modify your own file destination
LAX_Event <- read_excel("./LAX/LAX_Event_2002_2022.xlsx")
#LAX_Flight <- read_excel("./LAX/LAX_Flight_2002_2022.xls")
SAC_Event <- read_excel("./SAC/SAC_Event_2002_2022.xlsx")
#SAC_Flight <- read_excel("./SAC/SAC_Flight_2002_2022.xls")

##### Simply Descriptive Analysis #####
### SAC Airport Analysis
# Calculate the count of incidents per year for SAC
yearly_incidents_SAC <- SAC_Event %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(Incidents = n()) %>%
  arrange(INCIDENT_YEAR)

# Create a bar plot for SAC
ggplot(yearly_incidents_SAC, aes(x = INCIDENT_YEAR, y = Incidents)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3.5) +
  labs(x = "Year", y = "Number of Incidents",
       title = "Number of Incidents per Year at SAC Airport") +
  theme_minimal()


### LAX Airport Analysis
# Calculate the count of incidents per year for LAX
yearly_incidents_LAX <- LAX_Event %>%
  group_by(INCIDENT_YEAR) %>%
  summarise(Incidents = n()) %>%
  arrange(INCIDENT_YEAR)

# Create a bar plot for LAX
ggplot(yearly_incidents_LAX, aes(x = INCIDENT_YEAR, y = Incidents)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3.5) +
  labs(x = "Year", y = "Number of Incidents",
       title = "Number of Incidents per Year at LAX Airport") +
  theme_minimal()


# Combine these two for better comparison or visualization
yearly_incidents_SAC <- yearly_incidents_SAC %>%
  mutate(Airport = "SAC")

yearly_incidents_LAX <- yearly_incidents_LAX %>%
  mutate(Airport = "LAX")

# Combine the data
combined_data <- rbind(yearly_incidents_SAC, yearly_incidents_LAX)

# Create a grouped bar plot
ggplot(combined_data, aes(x = INCIDENT_YEAR, y = Incidents, fill = Airport)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Incidents), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  labs(x = "Year", y = "Number of Incidents",
       title = "Comparison of Incident Numbers per Year at SAC and LAX Airports") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "deepskyblue"))



##### Per Month Section #####
# Calculate the count of incidents per month for SAC
monthly_incidents_SAC <- SAC_Event %>%
  group_by(INCIDENT_MONTH) %>%
  summarise(Incidents = n()) %>%
  arrange(INCIDENT_MONTH)

# Create a bar plot for SAC
ggplot(monthly_incidents_SAC, aes(x = INCIDENT_MONTH, y = Incidents)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Number of Incidents",
       title = "Number of Incidents per Month at SAC Airport") +
  theme_minimal()

# Same for for LAX
monthly_incidents_LAX <- LAX_Event %>%
  group_by(INCIDENT_MONTH) %>%
  summarise(Incidents = n()) %>%
  arrange(INCIDENT_MONTH)

# Create a bar plot for LAX
ggplot(monthly_incidents_LAX, aes(x = INCIDENT_MONTH, y = Incidents)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Number of Incidents",
       title = "Number of Incidents per Month at LAX Airport") +
  theme_minimal()

#
monthly_incidents_SAC <- monthly_incidents_SAC %>%
  mutate(Airport = "SAC")

monthly_incidents_LAX <- monthly_incidents_LAX %>%
  mutate(Airport = "LAX")

# Combine the data
combined_monthly_data <- rbind(monthly_incidents_SAC, monthly_incidents_LAX)

# Create a grouped bar plot for comparison and visualization
ggplot(combined_monthly_data, aes(x = INCIDENT_MONTH, y = Incidents, fill = Airport)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Incidents), vjust = -0.3, position = position_dodge(0.9), size = 3.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Number of Incidents",
       title = "Comparison of Monthly Incident Numbers at SAC and LAX Airports") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "deepskyblue"))

###### Hour Analysis #####
# Extract hour and clean NA for SAC ( The NAs need to be careful), some
# can use NAs and some may not.
SAC_Event$Hour <- as.integer(sub("^(\\d{2}):.*", "\\1", SAC_Event$TIME))
SAC_Event <- SAC_Event[!is.na(SAC_Event$Hour), ]

# Count incidents by hour for SAC
hourly_incidents_SAC <- SAC_Event %>%
  group_by(Hour) %>%
  summarise(Incidents = n()) %>%
  arrange(Hour)

# Create a bar plot for SAC
hourly_bar_chart_SAC <- ggplot(hourly_incidents_SAC, aes(x = Hour, y = Incidents)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3) +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
  labs(x = "Hour of the Day", y = "Number of Incidents", title = "Number of Incidents by Hour of the Day at SAC Airport") +
  theme_minimal()
hourly_bar_chart_SAC

# Extract hour and clean NA for LAX (same as the SAC)
LAX_Event$Hour <- as.integer(sub("^(\\d{2}):.*", "\\1", LAX_Event$TIME))
LAX_Event <- LAX_Event[!is.na(LAX_Event$Hour), ]

# Count incidents by hour for LAX
hourly_incidents_LAX <- LAX_Event %>%
  group_by(Hour) %>%
  summarise(Incidents = n()) %>%
  arrange(Hour)

# Create a bar plot for LAX
hourly_bar_chart_LAX <- ggplot(hourly_incidents_LAX, aes(x = Hour, y = Incidents)) +
  geom_bar(stat = "identity", fill = "deepskyblue") +
  geom_text(aes(label = Incidents), vjust = -0.3, size = 3) +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
  labs(x = "Hour of the Day", y = "Number of Incidents", title = "Number of Incidents by Hour of the Day at LAX Airport") +
  theme_minimal()
hourly_bar_chart_LAX

# Add an airport column to each dataset
hourly_incidents_SAC <- hourly_incidents_SAC %>%
  mutate(Airport = "SAC")

hourly_incidents_LAX <- hourly_incidents_LAX %>%
  mutate(Airport = "LAX")

# Combine the data
combined_hourly_data <- rbind(hourly_incidents_SAC, hourly_incidents_LAX)

# Create a grouped bar plot for comparison and visualization
combined_hourly_chart <- ggplot(combined_hourly_data, aes(x = Hour, y = Incidents, fill = Airport)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Incidents), vjust = -0.3, position = position_dodge(0.9), size = 3) +
  scale_x_continuous(breaks = 0:23, labels = sprintf("%02d:00", 0:23)) +
  labs(x = "Hour of the Day", y = "Number of Incidents",
       title = "Comparison of Hourly Incident Numbers at SAC and LAX Airports") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "deepskyblue"))

# Print the combined chart
combined_hourly_chart





##### Time Section Analysis #####
# Create Function to categorize time into sections
# So for this we splitted them into four time sections
# Midnight 0000-0559 Morning 6000-1159 Afternoon 1200-1759 Evening 1800-2359
categorize_time <- function(data) {
  data$TimeObj <- as.POSIXct(data$TIME, format = "%H:%M", tz = "UTC")
  data$TimeOfDay <- cut(data$TimeObj,
                        breaks = c(as.POSIXct('00:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('06:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('12:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('18:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('23:59', format='%H:%M', tz='UTC')),
                        labels = c('Midnight', 'Morning', 'Afternoon', 'Evening'),
                        include.lowest = TRUE)
  data <- data[!is.na(data$TimeOfDay), ]
  return(data)
}

# Apply the function to LAX and SAC datasets
LAX_Event <- categorize_time(LAX_Event)
SAC_Event <- categorize_time(SAC_Event)

# Function to create time section analysis plots
create_time_section_plots <- function(data, airport_name) {
  # Count the number of incidents in each time section
  time_section_incidents <- data %>%
    group_by(TimeOfDay) %>%
    summarise(Incidents = n())
  
  # Create a bar chart
  bar_chart <- ggplot(time_section_incidents, aes(x = TimeOfDay, y = Incidents, fill = TimeOfDay)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Incidents), vjust = -0.3, size = 5) +
    labs(x = "Time of Day", y = "Number of Incidents", title = paste("Number of Incidents by Time of Day at", airport_name, "Airport")) +
    theme_minimal() +
    scale_fill_brewer(palette="Pastel1")
  
  # Calculate the percentage for the pie chart labels
  time_section_incidents$Percentage <- (time_section_incidents$Incidents / sum(time_section_incidents$Incidents)) * 100
  
  # Pie chart with numbers and percentages
  pie_chart <- ggplot(time_section_incidents, aes(x = "", y = Incidents, fill = TimeOfDay)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste(Incidents, " (", round(Percentage, 1), "%)", sep = "")),
              position = position_stack(vjust = 0.5)) +
    labs(x = "", y = "", title = paste("Incident Distribution by Time of Day at", airport_name, "Airport")) +
    theme_void() +
    scale_fill_brewer(palette="Pastel1")
  
  # Print the bar chart and the pie chart
  print(bar_chart)
  print(pie_chart)
}

# Apply the function to LAX and SAC
create_time_section_plots(LAX_Event, "LAX")
create_time_section_plots(SAC_Event, "SAC")

# Comparison we made plot place in the same chart for easier comparison
# There are a lot of duplicates down, you can modify them. 
# Function to Create Time Section Analysis Plots
create_time_section_plots <- function(data, airport_name) {
  # Count the number of incidents in each time section
  time_section_incidents <- data %>%
    group_by(TimeOfDay) %>%
    summarise(Incidents = n())
  
  # Create a bar chart
  bar_chart <- ggplot(time_section_incidents, aes(x = TimeOfDay, y = Incidents, fill = TimeOfDay)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Incidents), vjust = -0.3, size = 5) +
    labs(x = "Time of Day", y = "Number of Incidents", title = paste("Number of Incidents by Time of Day at", airport_name, "Airport")) +
    theme_minimal() +
    scale_fill_brewer(palette="Pastel1")
  
  # Calculate the percentage for the pie chart labels
  time_section_incidents$Percentage <- (time_section_incidents$Incidents / sum(time_section_incidents$Incidents)) * 100
  
  # Pie chart with numbers and percentages
  pie_chart <- ggplot(time_section_incidents, aes(x = "", y = Incidents, fill = TimeOfDay)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste(Incidents, " (", round(Percentage, 1), "%)", sep = "")),
              position = position_stack(vjust = 0.5)) +
    labs(x = "", y = "", title = paste("Incident Distribution by Time of Day at", airport_name, "Airport")) +
    theme_void() +
    scale_fill_brewer(palette="Pastel1")
  
  # Return the plots
  return(list(bar_chart = bar_chart, pie_chart = pie_chart))
}

# Apply the categorize_time function to LAX and SAC datasets
LAX_Event <- categorize_time(LAX_Event)
SAC_Event <- categorize_time(SAC_Event)

# Generate the plots for each airport
plots_LAX <- create_time_section_plots(LAX_Event, "LAX")
plots_SAC <- create_time_section_plots(SAC_Event, "SAC")

# Arrange the plots in a 2x2 grid
combined_plot <- grid.arrange(
  plots_SAC$bar_chart, plots_LAX$bar_chart,
  plots_SAC$pie_chart, plots_LAX$pie_chart,
  ncol = 2
)


##### Size Distribution #####
## THere are small medium and large in the data
## THere are also species in the data. you can do them in either way
## We used NA in this part
# Function to process and plot data
process_and_plot <- function(data, airport_name) {
  # Replace NA or empty entries with "Not Available"
  data$SIZE <- as.character(data$SIZE) 
  data$SIZE[is.na(data$SIZE) | data$SIZE == ""] <- "Not Available"
  
  # Convert back to factor with all levels
  data$SIZE <- factor(data$SIZE, levels = c("Small", "Medium", "Large", "Not Available"))
  
  # Count the number of incidents by wildlife size
  size_distribution <- data %>%
    group_by(SIZE) %>%
    summarise(Incidents = n())
  
  # Bar chart
  size_chart <- ggplot(size_distribution, aes(x = SIZE, y = Incidents, fill = SIZE)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Incidents), vjust = -0.3, size = 5) +
    labs(x = "Wildlife Size Category", y = "Number of Incidents",
         title = paste("Number of Wildlife Incidents by Size Category at", airport_name)) +
    theme_minimal() +
    scale_fill_brewer(palette="Pastel1")
  
  # Percentage for pie chart labels
  size_distribution$Percentage <- (size_distribution$Incidents / sum(size_distribution$Incidents)) * 100
  
  # Pie chart
  pie_chart <- ggplot(size_distribution, aes(x = "", y = Incidents, fill = SIZE)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste(Incidents, " (", round(Percentage, 1), "%)", sep = "")),
              position = position_stack(vjust = 0.5)) +
    labs(x = "", y = "",
         title = paste("Wildlife Size Category Distribution at", airport_name)) +
    theme_void() +
    scale_fill_brewer(palette="Pastel1")
  
  # Print the charts
  list(BarChart = size_chart, PieChart = pie_chart)
}

# Process and plot for LAX
lax_charts <- process_and_plot(LAX_Event, "LAX")
lax_charts$BarChart
lax_charts$PieChart

# Process and plot for SAC
sac_charts <- process_and_plot(SAC_Event, "SAC")
sac_charts$BarChart
sac_charts$PieChart


# Generate the charts for LAX and SAC
lax_charts <- process_and_plot(LAX_Event, "LAX")
sac_charts <- process_and_plot(SAC_Event, "SAC")

# Arrange the charts into a 2x2 grid
combined_chart <- grid.arrange(
  sac_charts$BarChart, lax_charts$BarChart,
  sac_charts$PieChart, lax_charts$PieChart,
  ncol = 2, nrow = 2
)

# Print the combined chart
combined_chart

# Percentage
# Function to create pie chart for a given dataset
create_pie_chart <- function(data, airport_name) {
  # Replace NA or empty entries with "Not Available"
  data$SIZE <- as.character(data$SIZE) # Convert factor to character if it's not already
  data$SIZE[is.na(data$SIZE) | data$SIZE == ""] <- "Not Available"
  data$SIZE <- factor(data$SIZE, levels = c("Small", "Medium", "Large", "Not Available"))
  
  # Count the number of incidents by wildlife size
  size_distribution <- data %>%
    group_by(SIZE) %>%
    summarise(Incidents = n())
  
  # Calculate the percentage for the pie chart labels
  size_distribution$Percentage <- (size_distribution$Incidents / sum(size_distribution$Incidents)) * 100
  
  # Pie chart with numbers and percentages
  pie_chart <- ggplot(size_distribution, aes(x = "", y = Incidents, fill = SIZE)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste(Incidents, " (", round(Percentage, 1), "%)", sep = "")),
              position = position_stack(vjust = 0.5)) +
    labs(x = "", y = "",
         title = paste("Wildlife Size Category Distribution at", airport_name)) +
    theme_void() +
    scale_fill_brewer(palette="Pastel1")
  
  return(pie_chart)
}

# Create pie chart for LAX
lax_pie_chart <- create_pie_chart(LAX_Event, "LAX")

# Create pie chart for SAC
sac_pie_chart <- create_pie_chart(SAC_Event, "SAC")

# Print the pie charts
lax_pie_chart
sac_pie_chart

##### Section 02 Event Categories #####
# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# In case the time slot is missing we did it again here. 
# Function to categorize time into sections
categorize_time <- function(data) {
  data$TimeObj <- as.POSIXct(data$TIME, format = "%H:%M", tz = "UTC")
  data$TimeOfDay <- cut(data$TimeObj,
                        breaks = c(as.POSIXct('00:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('06:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('12:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('18:00', format='%H:%M', tz='UTC'),
                                   as.POSIXct('23:59', format='%H:%M', tz='UTC')),
                        labels = c('Midnight', 'Morning', 'Afternoon', 'Evening'),
                        include.lowest = TRUE)
  data <- data[!is.na(data$TimeOfDay), ]
  return(data)
}

# Function to create time and size distribution plots
create_time_size_plots <- function(data, airport_name) {
  # Count the number of incidents by wildlife size for each time of day
  size_time_distribution <- data %>%
    group_by(TimeOfDay, SIZE) %>%
    summarise(Incidents = n(), .groups = 'drop') %>%
    mutate(Percentage = (Incidents / sum(Incidents)) * 100)
  
  # Bar chart with size distribution for each time of day
  bar_chart_time_size <- ggplot(size_time_distribution, aes(x = SIZE, y = Incidents, fill = SIZE)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Incidents), vjust = -0.3, size = 3) +
    facet_wrap(~TimeOfDay, scales = "free_y") +
    labs(x = "Wildlife Size Category", y = "Number of Incidents",
         title = paste("Wildlife Size Distribution for Each Time of Day at", airport_name)) +
    theme_minimal() +
    scale_fill_brewer(palette="Pastel1")
  
  # Pie charts for each time of day
  pie_charts_time_size <- lapply(unique(size_time_distribution$TimeOfDay), function(time_section) {
    data_section <- size_time_distribution[size_time_distribution$TimeOfDay == time_section, ]
    ggplot(data_section, aes(x = "", y = Incidents, fill = SIZE)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste(Incidents, " (", round(Percentage, 1), "%)", sep = "")),
                position = position_stack(vjust = 0.5)) +
      labs(title = paste("Size Distribution at", airport_name, "-", time_section), x = "", y = "") +
      theme_void() +
      scale_fill_brewer(palette="Pastel1")
  })
  
  # Print the bar chart and arrange the individual pie charts
  print(bar_chart_time_size)
  do.call(grid.arrange, pie_charts_time_size)
}

# Apply the function to LAX and SAC datasets
LAX_Event <- categorize_time(LAX_Event)
SAC_Event <- categorize_time(SAC_Event)






###### Total number of incidents per month for LAX #####
## This is to provide operator with the understand of the distribution of 
## the incident every month
total_incidents_per_month_lax <- LAX_Event %>%
  group_by(INCIDENT_MONTH) %>%
  summarise(TotalIncidents = n())

# Function to calculate and plot time of day incidents
plot_time_of_day_incidents <- function(data, time_of_day, color, label) {
  incidents_per_month <- data %>%
    filter(TimeOfDay == time_of_day) %>%
    group_by(INCIDENT_MONTH) %>%
    summarise(Incidents = n())
  
  # Merge with total incidents to calculate the percentage
  percentage_incidents <- merge(total_incidents_per_month_lax, incidents_per_month, by = "INCIDENT_MONTH", all = TRUE)
  percentage_incidents$Incidents[is.na(percentage_incidents$Incidents)] <- 0
  percentage_incidents$Percentage <- (percentage_incidents$Incidents / percentage_incidents$TotalIncidents)
  
  # Plot the percentages by month as a line plot
  percentage_plot <- ggplot(percentage_incidents, aes(x = INCIDENT_MONTH, y = Percentage)) +
    geom_line(group=1, colour=color) + 
    geom_point(colour=color) + 
    scale_x_continuous(breaks = 1:12, labels = month.abb) + 
    labs(x = "Month", y = "Percentage", title = paste("Percentage of Incidents in the", label, "by Month at LAX")) +
    theme_minimal()
  
  return(percentage_plot)
}

# Plot for each time of day for LAX
midnight_plot_lax <- plot_time_of_day_incidents(LAX_Event, 'Midnight', 'blue', 'Midnight')
morning_plot_lax <- plot_time_of_day_incidents(LAX_Event, 'Morning', 'green', 'Morning')
afternoon_plot_lax <- plot_time_of_day_incidents(LAX_Event, 'Afternoon', 'orange', 'Afternoon')
evening_plot_lax <- plot_time_of_day_incidents(LAX_Event, 'Evening', 'purple', 'Evening')

# Print the plots
midnight_plot_lax
morning_plot_lax
afternoon_plot_lax
evening_plot_lax


# Function to calculate percentages for a given time of day for LAX
calculate_percentages_lax <- function(data, time_of_day, color) {
  incidents_per_month <- data %>%
    filter(TimeOfDay == time_of_day) %>%
    group_by(INCIDENT_MONTH) %>%
    summarise(Count = n()) %>%
    merge(total_incidents_per_month_lax, by = "INCIDENT_MONTH", all = TRUE) %>%
    mutate(Percentage = Count / TotalIncidents,
           TimeOfDay = time_of_day,
           Color = color)
  
  # Replace NA with 0
  incidents_per_month$Count[is.na(incidents_per_month$Count)] <- 0
  incidents_per_month$Percentage[is.na(incidents_per_month$Percentage)] <- 0
  
  return(incidents_per_month)
}

# Calculate percentages for each time of day for LAX
midnight_data_lax <- calculate_percentages_lax(LAX_Event, "Midnight", "blue")
morning_data_lax <- calculate_percentages_lax(LAX_Event, "Morning", "green")
afternoon_data_lax <- calculate_percentages_lax(LAX_Event, "Afternoon", "orange")
evening_data_lax <- calculate_percentages_lax(LAX_Event, "Evening", "purple")

# Combine all data into one dataframe
combined_data_lax <- rbind(midnight_data_lax, morning_data_lax, afternoon_data_lax, evening_data_lax)

# Create one combined line plot for LAX
combined_line_plot_lax <- ggplot(combined_data_lax, aes(x = INCIDENT_MONTH, y = Percentage, group = TimeOfDay, color = TimeOfDay)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(Percentage, 2)), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Percentage of Incidents",
       title = "Combined Percentage of Incidents by Time of Day and Month at LAX") +
  theme_minimal() +
  scale_color_manual(values = c("Midnight" = "blue", "Morning" = "green", "Afternoon" = "orange", "Evening" = "purple"))

# Print the combined line plot for LAX
combined_line_plot_lax


##### Total number of incidents per month for SAC  #####
## Basically it is the same as LAX
# Total number of incidents per month for SAC
total_incidents_per_month_sac <- SAC_Event %>%
  group_by(INCIDENT_MONTH) %>%
  summarise(TotalIncidents = n())

# Function to calculate percentages for a given time of day for SAC
calculate_percentages_sac <- function(data, time_of_day, color) {
  incidents_per_month <- data %>%
    filter(TimeOfDay == time_of_day) %>%
    group_by(INCIDENT_MONTH) %>%
    summarise(Count = n()) %>%
    merge(total_incidents_per_month_sac, by = "INCIDENT_MONTH", all = TRUE) %>%
    mutate(Percentage = Count / TotalIncidents,
           TimeOfDay = time_of_day,
           Color = color)
  
  # Replace NA with 0
  incidents_per_month$Count[is.na(incidents_per_month$Count)] <- 0
  incidents_per_month$Percentage[is.na(incidents_per_month$Percentage)] <- 0
  
  return(incidents_per_month)
}

# Calculate percentages for each time of day for SAC
midnight_data_sac <- calculate_percentages_sac(SAC_Event, "Midnight", "blue")
morning_data_sac <- calculate_percentages_sac(SAC_Event, "Morning", "green")
afternoon_data_sac <- calculate_percentages_sac(SAC_Event, "Afternoon", "orange")
evening_data_sac <- calculate_percentages_sac(SAC_Event, "Evening", "purple")

# Combine all data into one dataframe
combined_data_sac <- rbind(midnight_data_sac, morning_data_sac, afternoon_data_sac, evening_data_sac)

# Create one combined line plot for SAC
combined_line_plot_sac <- ggplot(combined_data_sac, aes(x = INCIDENT_MONTH, y = Percentage, group = TimeOfDay, color = TimeOfDay)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(Percentage, 2)), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Percentage of Incidents",
       title = "Combined Percentage of Incidents by Time of Day and Month at SAC") +
  theme_minimal() +
  scale_color_manual(values = c("Midnight" = "blue", "Morning" = "green", "Afternoon" = "orange", "Evening" = "purple"))

# Print the combined line plot for SAC
combined_line_plot_sac









###### 3D Plot (Future Use) ######
## Note : This is a demo for the Risk Analysis it is not shown in the paper
## 
##
##
library(plotly)
# Filter out 'NA' sizes and count the number of incidents by wildlife size for each month
size_month_distribution_lax <- LAX_Event %>%
  filter(SIZE %in% c('Small', 'Medium', 'Large')) %>%  # Exclude 'NA' or 'Not Available'
  group_by(INCIDENT_MONTH, SIZE) %>%
  summarise(Incidents = n(), .groups = 'drop') %>%
  mutate(Percentage = (Incidents / sum(Incidents)) * 100)

# Transform INCIDENT_MONTH into a factor if it's not already
size_month_distribution_lax$INCIDENT_MONTH <- as.factor(size_month_distribution_lax$INCIDENT_MONTH)

# Create a 3D bar chart using plotly for LAX with square markers
fig_lax <- plot_ly(data = size_month_distribution_lax, 
                   x = ~INCIDENT_MONTH, 
                   y = ~SIZE, 
                   z = ~Incidents, 
                   type = 'scatter3d', 
                   mode = 'markers', 
                   marker = list(
                     size = 5, 
                     color = ~Incidents, 
                     colorscale = 'Viridis', 
                     symbol = 'square'  # Set marker shape to square
                   ))

# Add layout details to the 3D plot
fig_lax <- fig_lax %>% layout(title = '3D Wildlife Size Distribution by Month at LAX',
                              scene = list(
                                xaxis = list(title = 'Month'),
                                yaxis = list(title = 'Wildlife Size'),
                                zaxis = list(title = 'Number of Incidents'),
                                aspectratio = list(x = 1, y = 1, z = 0.7)
                              ))

# Print the 3D plot for LAX
fig_lax


#  SAC
# Filter out 'NA' sizes and count the number of incidents by wildlife size for each month for SAC
size_month_distribution_sac <- SAC_Event %>%
  filter(SIZE %in% c('Small', 'Medium', 'Large')) %>%  # Exclude 'NA' or 'Not Available'
  group_by(INCIDENT_MONTH, SIZE) %>%
  summarise(Incidents = n(), .groups = 'drop') %>%
  mutate(Percentage = (Incidents / sum(Incidents)) * 100)

# Transform INCIDENT_MONTH into a factor if it's not already
size_month_distribution_sac$INCIDENT_MONTH <- as.factor(size_month_distribution_sac$INCIDENT_MONTH)

# Create a 3D bar chart using plotly for SAC with square markers
fig_sac <- plot_ly(data = size_month_distribution_sac, 
                   x = ~INCIDENT_MONTH, 
                   y = ~SIZE, 
                   z = ~Incidents, 
                   type = 'scatter3d', 
                   mode = 'markers', 
                   marker = list(
                     size = 5, 
                     color = ~Incidents, 
                     colorscale = 'Viridis', 
                     symbol = 'square'  # Set marker shape to square
                   ))

# Add layout details to the 3D plot
fig_sac <- fig_sac %>% layout(title = '3D Wildlife Size Distribution by Month at SAC',
                              scene = list(
                                xaxis = list(title = 'Month'),
                                yaxis = list(title = 'Wildlife Size'),
                                zaxis = list(title = 'Number of Incidents'),
                                aspectratio = list(x = 1, y = 1, z = 0.7)
                              ))

# Print the 3D plot for SAC
fig_sac



##### SPECIES INCLUDED (Future Use)######
## Future Use
# For SAC_Event
sac_species_count <- SAC_Event %>%
  group_by(SPECIES) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Print the species frequency for SAC_Event
print(sac_species_count)

# For LAX_Event
lax_species_count <- LAX_Event %>%
  group_by(SPECIES) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Print the species frequency for LAX_Event
print(lax_species_count)





###### Flight of LAX and SAC (Next Paper)######
## Future use for number of flight added
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)

# Import Flight CSV
lax_flights <- read_csv('./LAX/LAX_Flight_2002_2022.csv', skip = 1)
sac_flights <- read_csv('./SAC/SAC_Flights_2002_2022.csv', skip = 1)

# Function to clean the data
clean_flight_data <- function(flight_data) {
  # Remove rows where 'Month' is "TOTAL"
  clean_data <- flight_data %>%
    filter(Month != "TOTAL") %>%
    mutate(Year = as.numeric(Year),
           Month = as.numeric(Month),
           TOTAL = as.numeric(TOTAL)) %>%
    filter(!is.na(Year), !is.na(Month), !is.na(TOTAL))  
  
  # Separate out the annual totals if needed
  annual_totals <- flight_data %>%
    filter(Month == "TOTAL") %>%
    mutate(Year = as.numeric(Year),
           TOTAL = as.numeric(TOTAL))
  
  list(CleanData = clean_data, AnnualTotals = annual_totals)
}

# Clean the LAX and SAC flight data
lax_cleaned <- clean_flight_data(lax_flights)
sac_cleaned <- clean_flight_data(sac_flights)

# Cleaned data frames and separate annual totals
lax_monthly_flights <- lax_cleaned$CleanData
lax_annual_totals <- lax_cleaned$AnnualTotals

sac_monthly_flights <- sac_cleaned$CleanData
sac_annual_totals <- sac_cleaned$AnnualTotals







