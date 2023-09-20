salary_data <- read.csv("C:\\Users\\Brennan Rutherford\\Downloads\\Total_Employee_Compensation_2021 (1).csv")
print(salary_data)

# Packagaes
install.packages("ggplot2")
install.packages("dplyr")

#load Packagaes
library(ggplot2)
library(dplyr)

# Select departmnet Name and Title
department_name <- "FIRE"
title_name <- "Fire Fighter III"
# filtered data
filtered_data <- salary_data[salary_data$Department == department_name & salary_data$Title == title_name, ]

# function for gross pay summary statistics and standard deviation by department and title
gross_pay_by_department_and_title <- function(data, department_name, title_name) {
  filtered_data <- data[data$Department == department_name & data$Title == title_name, ]
  gross_pay_summary <- summary(filtered_data$Gross_Pay)
  gross_pay_sd <- sd(filtered_data$Gross_Pay) 
  gross_pay_summary <- c(gross_pay_summary, SD = gross_pay_sd)
  return(gross_pay_summary)
}
gross_pay_summary <- gross_pay_by_department_and_title(salary_data, department_name, title_name)

# Print the Gross_Pay for the specified department and title
cat("Gross Pay Summary Statistics for", title_name, "in the", department_name, "department:\n")
print(gross_pay_summary)


# Calculate the Z-scores for Gross Pay
mean_salary <- mean(filtered_data$Gross_Pay)
sd_salary <- sd(filtered_data$Gross_Pay)
filtered_data$Z_Score <- (filtered_data$Gross_Pay - mean_salary) / sd_salary
sorted_data <- filtered_data[order(filtered_data$Z_Score), ]

print(sorted_data[, c("Department", "Title", "Gross_Pay", "Z_Score")])

# Create a histogram
ggplot(data = filtered_data, aes(x = Gross_Pay)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  labs(title = paste("Histogram of Gross Salary for", title_name, "in the", department_name, "Department"),
       x = "Gross Salary",
       y = "Frequency") +
  theme_minimal()

