# PROBLEM SET ####
# use the owid_covid_newyear dataset for questions 1-3



library(tidyverse)

# LOAD Our World In Data Covid dataset ####
owid_covid = read.csv("Session1/data/owid-covid-data.csv", header = TRUE,
                      stringsAsFactors = FALSE)
owid_covid$date = as.Date(owid_covid$date)

owid_covid %>%
  filter(location == "United Kingdom") -> owid_covid_uk

# filter for 4 countries
four_countries = c("United Kingdom", "Belgium", "Germany", "United States")
owid_covid %>%
  filter(location %in% four_countries) -> owid_covid_4countries

new_year = "2021-01-01"

owid_covid %>%
  filter(date == new_year) -> owid_covid_newyear

# Q1: Plot total_cases_per_million on x axis and total_deaths_per_million on y axis
# Change the colour of the plots to 'darkgreen'

plot(owid_covid_newyear$total_cases_per_million, owid_covid_newyear$total_deaths_per_million, col = "darkgreen")
with(owid_covid_newyear, plot(total_cases_per_million, total_deaths_per_million, col = "darkgreen"))

owid_covid_newyear %>%
  ggplot(aes(x = total_cases_per_million, y = total_deaths_per_million, colour = "darkgreen")) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(title = "Main title")

# Q2: Using the owid_covid_4countries dataset, draw a faceted plot (one panel for
# each country), where each panel shows the date on the x axis vs. new_deaths_per_million 
# on the y axis in black and also icu_patients_per_million in red on the y axis
# Please see session4/results/plot_faceted_4countries_date_vs_deaths_and_icu.pdf for the expected output
owid_covid_4countries %>%
  ggplot(mapping = aes(x = date, y = new_deaths_per_million, colour = location)) +
  geom_line() +
  geom_line(aes(y = icu_patients_per_million)) 

# Q3: (Advanced) Recreate the plot from Session2/results/plot_bubble_total_cases_vs_deaths.pdf from
# the owid_covid_newyear dataset
# use shape = 21 for geom_point()
# hint: use geom_label_repel() from ggrepel package
# hint: use scale_size(range = c(2, 40))
# hint: save the plot as a pdf with width and height = 10
library(ggrepel)
owid_covid_newyear %>%
  ggplot(aes(x = total_cases, y = total_deaths, fill = continent)) +
  geom_point(aes(size = population), shape = 21, alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  geom_label_repel(aes(label = location)) +
  scale_size(range = c(2, 40)) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Q4: For the covid data from India, plot date along the x axis and total number of 
# COVID-19 vaccination doses administered on the y axis; # also plot new confirmed 
# cases on the y axis in a different colour (look up the codebook https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv
# for description of the columns)
