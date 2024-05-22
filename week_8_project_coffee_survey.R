
# Install Packages --------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)


# Load Data ---------------------------------------------------------------

# We are going to use the most recent tidy tuesday data.
# This data is from an online taste test of coffee, and also asks questions
# about coffee consumption and demographics. There is some info on the data at 
# the below link. 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-05-14/readme.md

coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

coffee_survey <- 
  coffee_survey |> 
  clean_names()

coffee_survey |> 
  glimpse()


# Age and Coffee Consumption ----------------------------------------------

# Is coffee consumption associated with age? I would assume that older people
# will tend to drink a bit more coffee. 

# I will select only columns related to this question, specifically age groups,
# number of cups drank daily, and id. The code below gives us the count and
# percent of people per age group that drink different numbers of cups of
# coffee daily.

coffee_cups_by_age <- 
  coffee_survey |> 
    select(c(submission_id, age, cups)) |> 
    na.omit() |> 
    group_by(age) |> 
    count(cups) |> 
    rename(cups_count = n) |> 
    mutate(cups_pct = cups_count / sum(cups_count) * 100) |> 
    ungroup() |> 
    mutate(cups = fct_relevel(cups, c("Less than 1", "1", "2", "3", "4", "More than 4"))) |> 
    mutate(age = fct_relevel(age, c("<18 years old", "18-24 years old", "25-34 years old",
                               "35-44 years old", "45-54 years old", "55-64 years old",
                               ">65 years old")))

# I am going to try out a couple different graphs with the same information. 
# First, we are going to use position dodge to give us an idea of coffee consumption.

ggplot(data = coffee_cups_by_age,
       mapping = aes(x = age,
                     y = cups_pct,
                     fill = cups)) +
  geom_col(show.legend = TRUE,
           position = "dodge")+
  scale_y_continuous()+
  scale_fill_manual(values = c(
    "Less than 1" = "#dac4b5",
    "1" = "#caaa94",
    "2" = "#ba9073",
    "3" = "#a97653",
    "4" = "#885f43",
    "More than 4" = "#674833")) +
  labs(title = "Daily Coffee Intake by Age",
       subtitle = "Percentage of people by age group and daily coffee intake",
       x = "Age Group",
       y = "Percentage",
       fill = "Daily Cups")+
  theme_minimal()

# Looks like coffee consumption does tend to increase with age (or at least
# that older folks tend to like coffee more). That being said, most groups seem
# to peak at around 2 cups regardless of age.

# Now let's chart the information slightly different. Let's avoid using dodge
# and turn the graph sideways. This gives us a better glimpse of the 

ggplot(data = coffee_cups_by_age,
       mapping = aes(y = age,
                     x = cups_pct,
                     fill = cups)) +
  geom_col(show.legend = TRUE)+
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  scale_fill_manual(values = c(
    "Less than 1" = "#dac4b5",
    "1" = "#caaa94",
    "2" = "#ba9073",
    "3" = "#a97653",
    "4" = "#885f43",
    "More than 4" = "#674833")) +
  labs(title = "Daily Coffee Intake by Age",
       subtitle = "Percentage of people by age group and daily coffee intake",
       x = "Percentage",
       y = "Age Group",
       fill = "Daily Cups")+
  theme_minimal()

# We can see around 32% of people 55 and older drink at least 3 cups of coffee a
# day. This gets smaller for each age group, with only about 5% of folks under 18
# drinking 3 cups a day. 


# Favorite Coffee by Daily Cups -------------------------------------------

# I am also curious as to what coffee participants prefer based on their daily
# consumption. Do fast and furious drinkers have a clear favorite, and is it different
# than those that drink less?

# Creating a data set that has information on daily cups drank and ultimate
# preference during the taste test.

coffee_prefer_by_consumption <- 
  coffee_survey |> 
    select(c(submission_id, cups, prefer_overall)) |> 
    na.omit() |> 
    group_by(cups) |> 
    count(prefer_overall) |> 
    rename(prefer_count = n) |> 
    mutate(prefer_pct = prefer_count / sum(prefer_count) * 100) |> 
    ungroup() |> 
    mutate(cups = fct_relevel(cups, c("Less than 1", "1", "2", "3", "4", "More than 4")))

# Now let's plot this and see

ggplot(data = coffee_prefer_by_consumption,
       mapping = aes(x = cups,
                     y = prefer_pct,
                     fill = prefer_overall)) +
  geom_col(show.legend = TRUE,
           position = "dodge")+
  scale_y_continuous()+
  scale_fill_manual(values = c(
    "Coffee A" = "#dac4b5",
    "Coffee B" = "#caaa94",
    "Coffee C" = "#ba9073",
    "Coffee D" = "#a97653")) +
  labs(title = "Coffee Preference by Coffee Consumption",
       x = "Daily Cups",
       y = "Percentage",
       fill = "Coffee Sample")+
  theme_minimal()

# It looks like Coffee D tends to be the favorite of almost every group! 
# On face value daily consumption does not seem to influence coffee preference. 