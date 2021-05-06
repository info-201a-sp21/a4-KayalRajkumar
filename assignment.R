# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
library("dplyr")

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data
ks_projects_201801 <- read.csv("data/ks-projects-201801.csv")

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
column_names <- colnames(ks_projects_201801)
# - How many rows is the data frame?
number_of_rows <- nrow(ks_projects_201801)
# - How many columns are in the data frame?
number_of_columns <- ncol(ks_projects_201801)

# Use the `summary` function to get some summary information
ks_projects_summary <- summary(ks_projects_201801)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(column_name, dataframe) {
  if (typeof(dataframe[[column]]) == "integer") {
      sentence <- list(min = min(df[[column]], na.rm = TRUE),
                       max = max(df[[column]], na.rm = TRUE),
                       mean = mean(df[[column]], na.rm = TRUE))
    } else if (typeof(df[[column]]) == "character" & n_distinct(df[[column]]) 
               < 10) {
      sentence <- list(n_values = n_distinct(df[[column]]),
                       unique_values = unique(df[[column]]))
    } else {
      sentence <- list(n_values = n_distinct(df[[column]]), sample_values = 
                         sample(df[[column]], size = 10))
    }
sentence
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
kickstarter_data <- get_col_info("column", ks_projects_201801)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(dataframe) {
  column <- colnames(dataframe)
  sapply(column, get_col_info, dataframe)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
kickstarter_info <- get_summary_info(ks_projects_201801)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# It was interesting to see how things from the 'name' category were put into 2
# different higher-arching categories to make them more understandle for the 
# person looking at the data. 
# Many projects were missing an exact launch date in comparison to those with 
# launch dates.
# The 'usd_goal_real' was generally far larger than the 'usd_pledged' for the
# majority of projects.

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
project_with_highest_goal <- ks_projects_201801 %>%
  filter(usd_goal_real == max(usd_goal_real, na.rm = TRUE)) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?
project_with_lowest_goal <- ks_projects_201801 %>%
  filter(usd_goal_real == min(usd_goal_real, na.rm = TRUE)) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
projects_with_deadline_in_2018 <- ks_projects_201801 %>%
  filter(substr(deadline, 1, 4) == "2018") %>%
  pull(name) %>%

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
unsuccessful_projects <- 1 - (nrow(filter(ks_projects_201801, state == 
                                      "successful")) / nrow(ks_projects_201801))

# What was the amount pledged for the project with the most backers?
amount_pledged_with_backers <- ks_projects_201801 %>%
  filter(backers == max(backers, na.rm = TRUE)) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
failed_and_highest_amount <- ks_projects_201801 %>%
  filter(state == "failed")
  filter(usd_pledged_real == max(usd_pledged, na.rm == TRUE)) %>%
  pull(name)

# How much total money was pledged to projects that weren't marked successful?
total_money_pledged_to_unsuccessful <- ks_projects_201801 %>%
  filter(!state == "successful") %>%
  select(usd_pledged_real) %>%
  sum()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
category_with_most_pledged <- ks_projects_201801 %>%
  group_by(category) %>%
  summarize(total_usd_pledged_real = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(total_usd_pledged_real = max(total_usd_pledged_real, na.rn = TRUE)) %>%
  pull(category)
  
# Which country had the most backers?
country_with_most_backers <-ks_projects_201801 %>%
  group_by(country) %>%
  summarize(total_backers = sum(backers, na.rm = TRUE)) %>%
  filter(total_backers == max(total_backers, na.rm - TRUE)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
year_with_most_money_pledged <- ks_projects_201801 %>%
  mutate(year = substr(launched)) %>%
  group_by(year) %>%
  summarize(total_usd_pledged_real = sum(usd_pledged_real, na.rm = TRUE)) %>%
  filter(total_usd_pledged_real == max(total_usd_pledged_real, na.rm = TRUE)) %>%
  pull(year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from: I chose the launched dates to get the year from as it is 
# represents the start/beginning of when the money was pledged.


# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_three_main_categories <- ks_projects_201801 %>%
  mutate(year = substr(launched, 1, 4)) %>%
  filter(year == "2018") %>%
  group_by(category) %>%
  summarize(total_backers = sum(backers, na.rm = TRUE)) %>%
  filter(3, total_backers) %>%
  pull(category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_day_to_launch <- ks_projects_201801 %>%
  mutate(day_of_launch = weekday(as.Date(launched))) %>%
  group_by(day_of_launch) %>%
  tally(day_of_launch) %>%
  filter(most_day == max(most_day, na.rm = TRUE)) %>%
  pull(day_of_launch)
  
  
# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
least_successful_day <- ks_projects_201801 %>%
  mutate(launch_day = weekday(as.Date(launched))) %>%
  group_by(launch_day) %>%
  tally(launch_day) %>%
  filter(least_day == min(least_day, na.rm = TRUE)) %>%
  pull(launch_day)
  