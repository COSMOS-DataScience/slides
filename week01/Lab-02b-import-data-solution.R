library(haven)

# download the dataset from the provided URL to data

# Import the HIV antibody dataset
hiv_data <- read_xpt("data/HIV_J.XPT")  # Make sure the file is in your working directory


# Check to ensure the data is loaded correctly
glimpse(hiv_data)
head(hiv_data)


### Task 1
# Rename 'female' to 'gender'
names(hiv_data)[names(hiv_data) == "female"] <- "gender"


### Task 2
# Recode 0/1 to male/female
hiv_data$gender <- ifelse(hiv_data$gender == 0, "male",
                          ifelse(hiv_data$gender == 1, "female", NA))
### Task 3
# Create count summary
gender_counts <- as.data.frame(table(hiv_data$gender))
names(gender_counts) <- c("gender", "count")


### Task 4
# Write the summary to a CSV file
write.csv(gender_counts, "gender_counts.csv", row.names = FALSE)
gender_summary <- hiv_data %>%
  
  # 1. Rename 'female' to 'gender' (assuming variable exists)
  rename(gender = female) %>% 
  
  # 2. Recode 0/1 to male/female
  mutate(gender = case_when(
    gender == 0 ~ "male",
    gender == 1 ~ "female",
    TRUE ~ NA_character_  # Handles any unexpected values
  )) %>%
  
  # 3. Count gender levels (automatically excludes NAs)
  count(gender, name = "count") %>%
  
  # 4. Write to CSV
  write_csv("gender_counts.csv")

# Print the summary
gender_summary

head(select(hiv_data, gender))

# Verify counts match
table(hiv_data$gender, useNA = "always")