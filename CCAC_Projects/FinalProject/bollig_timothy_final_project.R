#R for Analytics Final Project
#Created by: Timothy Bollig
#Date last edited: 12/14/2021
#Copyright: GNU General Public License v3.0
#Contact: bollig.timothy@gmail.com

################################################################################
################################################################################

#Install and load packages.
install.packages("treemap")
install.packages("tidyverse")
library(treemap)
library(tidyverse)

############################Import Data############################

#Import data from CSV file is same directory.
#Source: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016
suicide_data <- read_csv("master.csv")

############################Tidy Data############################

#Remove columns we aren't interested in and rename suicides/100k pop column.
#Drop rows with year == 2016.  Not enough data for 2016 to be relevant.
suicide_data <- select(suicide_data, -c("country-year", "HDI for year")) %>%
  filter(year != 2016) %>%
  rename(suicides_per_100k_pop = "suicides/100k pop",
         gdp_year = "gdp_for_year ($)",
         gdp_capita = "gdp_per_capita ($)")

#######################Transform and Visualize Data#######################

#Suicide Counts by Year.  Worst years seem to be 1999, 2002, and 2003.
by_year <- suicide_data %>%
  group_by(year) %>%
  summarise(year_count = sum(suicides_no)) %>%
  arrange(desc(year_count))

#Plot by_year as a time series graph.
#Adjust y scale to have commas and limit range to make visually appealing.
ggplot(by_year, aes(x = year, y = year_count)) +
  geom_line(na.rm = TRUE, size = 1.5) +
  scale_y_continuous(labels =  scales::comma, limits = c(100000, 275000)) +
  labs(title = "Suicides by Year 1985 - 2015 (For Reported Countries)", 
       x = "Year", y = "Number of Suicides")

#Top countries for average suicide rates and their average yearly GDP.
#Group by before aggregating.
suicide_rates_by_country <- group_by(suicide_data, country)

suicide_rates_by_country <- summarise(suicide_rates_by_country, 
                                      avg_rate_per_100k_pop = mean(suicides_per_100k_pop),
                                      avg_gdp_per_year = mean(gdp_year),
                                      avg_pop = mean(population))

#Arrange tibble descending order of average suicide rates.
#Slice tibble down to top 10 highest average suicide rates.
top_countries_by_suicide_rates <- suicide_rates_by_country %>%
  arrange(desc(avg_rate_per_100k_pop)) %>%
  slice(1:10)

#Write to CSV file for easy exporting.
write_csv(top_countries_by_suicide_rates, "top_countries_by_suicide_rate.csv")

#Average suicide rates of top 10 most populated countries.
#Based on average population size.
most_populated_countries <- suicide_rates_by_country %>%
  arrange(desc(avg_pop)) %>%
  slice(1:10) %>%
  arrange(desc(avg_rate_per_100k_pop))

#Write to CSV file for easy exporting.
write_csv(most_populated_countries, "most_populated_countries.csv")

#Filter data to only rows related to the Russian Federation.
russian_suicides <- suicide_data %>%
  filter(country == "Russian Federation")

#Plot Russian Federation suicide counts as time series graph.
ggplot(russian_suicides, aes(x = year, y = suicides_no)) +
  geom_smooth(na.rm = TRUE, size = 1.5, se = FALSE) +
  scale_y_continuous(labels =  scales::comma) +
  labs(title = "Suicides by Year 1985 - 2015 (Russian Federation)", 
       x = "Year", y = "Number of Suicides")

#Countries by average yearly GDP
suicide_rates_by_country %>%
  arrange(desc(avg_gdp_per_year))

#Scatterplot of average yearly GDP versus average suicide rates.
ggplot(suicide_rates_by_country, 
       aes(x = avg_rate_per_100k_pop, y = avg_gdp_per_year)) + 
  geom_point() +
  scale_y_continuous(labels =  scales::comma) +
  labs(title = "Suicides by Yearly GDP (For Reported Countries)", 
       x = "Suicides per 100k Pop.", y = "Yearly GDP")

#Top countries for total number of suicides.
totals_by_country <- group_by(suicide_data, country)

totals_by_country <- summarise(totals_by_country, suicides = sum(suicides_no))

top_countries_by_suicide_totals <- totals_by_country %>%
  arrange(desc(suicides)) %>%
  slice(1:10)

#Treemap plotting countries with highest suicide totals.
treemap(top_countries_by_suicide_totals, index = c("country"), vSize = "suicides", 
        title = "Countries with Most Suicides from 1985 - ")

#Suicides by age group.
totals_by_age_group <- group_by(suicide_data, age)

totals_by_age_group <- summarise(totals_by_age_group, suicides = sum(suicides_no))

#Treemap plotting suicide totals by age groups.
treemap(totals_by_age_group, index = c("age"), vSize = "suicides", 
        title = "Suicides by Age Group")

#Suicides by generational groups.
totals_by_generation <- group_by(suicide_data, generation)

totals_by_generation <- summarise(totals_by_generation, suicides = sum(suicides_no))

#Treemap plotting suicides by generational groups.
treemap(totals_by_generation, index = c("generation"), vSize = "suicides", 
        title = "Suicides by Generation")

#Suicides by sex.
totals_by_sex <- group_by(suicide_data, sex)

totals_by_sex <- summarise(totals_by_sex, suicides = sum(suicides_no))

#Pie chart showing suicide totals by sex.
ggplot(totals_by_sex, aes(x = "", y = suicides, fill = sex)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

#Groups of generations and age ranges  (No need to visualize)
generations_age <- group_by(suicide_data, generation, age)

generations_age <- summarise(generations_age, generation, age)

distinct(generations_age)


#Stuff for potential further exploration.

#Worst years for top countries (max suicides grouped by country and year)
#First create a tibble of the countries and their max value of suicides_no
max_suicides_per_country <- group_by(suicide_data, country)

max_suicides_per_country <- summarise(max_suicides_per_country,
                                      max_suicides = max(suicides_no))

#Create a new tibble by joining the original data to the tibble created above
#Filter to only include rows where the suicides_no == the values from the max suicides tibble.
worst_years_per_country <- suicide_data %>%
  left_join(max_suicides_per_country, by = "country") %>%
  filter(suicides_no == max_suicides) %>%
  select(country, year, suicides_no)

worst_years_per_country

#########################Communicate Insights#########################
#See FinalProjectReport.docx [Insert Github link]

##############################End of File#############################