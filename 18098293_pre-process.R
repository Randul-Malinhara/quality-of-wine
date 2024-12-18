# Import libraries
library(tidyverse)
library(readxl)
library(tidymodels)
library(readxl)
library(caTools)
library(xlsx)

# --------------- PREPARATION OF DATA BEFRORE APPROACH --------------- #
# Load Data
power_usage <- read_excel("C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/UoW_load.xlsx")
View(power_usage)

#09:00 , 10: 00, 11:00
power_usage <- power_usage$`11:00`
View(power_usage)
str(power_usage)
plot(power_usage)

# Build the Partial Auto Correlation plot
pacf(x = power_usage, plot = TRUE)

# Create Lags for Time Series
lag1 = lag(power_usage,1)
lag2 = lag(power_usage,2)
lag3 = lag(power_usage,3)
lag4 = lag(power_usage,4)
lag5 = lag(power_usage,5)
power_usage <- cbind(power_usage,lag1,lag2,lag3,lag4,lag5)

# Formatting power_usage
power_usage <- na.omit(power_usage)
sum(is.na(power_usage))

colnames(power_usage) <- c("wineD_original_data","v2","v3","v4","v5","v6")

# Scaling Data
scaled_poweruse <- scale(power_usage)
str(scaled_poweruse)

# Save the processed data to excel file
# For Excel files separately 09:00 , 10:00, 11:00 hours
write.xlsx(scaled_poweruse,
           file = "C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/scaled_powerusage_09.xlsx",
           col.names = TRUE, append = TRUE, row.names = FALSE)
