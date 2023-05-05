# ---------------------------
#
# Script name: city_dalycity.R
#
# Purpose of script: Pull info from Census to graph Daly City.
#
# Author: Kelly Shee
#
# Date Created: 2023-05-05
#
# ---------------------------

rm(list = ls())

# install.packages("pacman")
library(pacman)
p_load(rstudioapi, tidyverse, magrittr, censusapi, demography)

# set directory to this script's source file location
setwd(dirname(getSourceEditorContext()$path))    

# years
current_year <- as.numeric(format(Sys.Date(), "%Y"))
my_vintage <- current_year - 2
years <- my_vintage:2009

# enter census api key (request here: https://api.census.gov/data/key_signup.html)
census_key <- "1c7866fb9cbb353de24dd9ec1d5d13cdb8307f50"

# list of all variables 
available_census_vars <- listCensusMetadata(name = "acs/acs5",
                                            vintage = my_vintage,
                                            type = "variables")

# my areas
my_city <- c("place:17918", "Daly City")
my_state <- c("state:06", "California")

# function to pull census info
get_census <- function(my_vars, my_region = my_city[1], my_regionin = my_state[1], which_vintage = my_vintage){
  
  output <- getCensus(name = "acs/acs5", 
                      vintage = which_vintage, 
                      key = census_key,
                      vars = c("NAME", my_vars),
                      region = my_region[1],
                      regionin = my_regionin[1]) 
  
  output$year <- which_vintage  # add year column
  output$NAME <- gsub("\\s(city|town).*", "", output$NAME) # remove city or town & everything after
  output %<>% 
    select(year, everything(), -state, -place, -NAME)
  
  return(output)
}

# function to loop over years
loop_get_census <- function(my_vars, my_years = years){
  df_empty <- data.frame()
  for (i in my_years) {
    output <- get_census(my_vars, which_vintage = i)
    df_empty <- rbind(output, df_empty)
  }
  return(df_empty)
}

# function to gather variables  
census_vars <- function(my_name, my_phrase){
  if (missing(my_phrase)) {
    # only search name column
    output <- available_census_vars %>%
      filter(str_detect(available_census_vars$name, my_name))
  } else {
    # search name AND label columns
    output <- available_census_vars %>%
      filter(str_detect(available_census_vars$name, my_name),
             str_detect(available_census_vars$label, my_phrase))
  }
  output <- output$name
  return(output)
}


# Population----

# goal: forecasting (need births, death, emigrants, immigrants)
# number + births + immigrants - emigrants - death

df_pop <- loop_get_census("B01003_001E")
df_pop$year <- as.character(df_pop$year)
df_pop %<>% 
  rename(pop = B01003_001E) %>% 
  mutate(pop_change = pop/lag(pop) - 1)

# Race----

# available only for 2015+

## Asian only----

years_asian <- my_vintage:2015
vars_asian <- census_vars("B02015_")

df_asian <- loop_get_census(vars_asian, my_years = years_asian)

# rename columns w/ races by matching column names to available_census_vars
## named vector of variable labels
name_to_label <- setNames(available_census_vars$label, available_census_vars$name)
## find indices of matching names
name_indices <- match(names(df_asian), names(name_to_label))
## replace column names with labels
names(df_asian)[which(!is.na(name_indices))] <- name_to_label[na.omit(name_indices)]

df_asian %<>% 
  rename(Total = `Estimate!!Total:`) %>% 
  rename_all(~gsub("Estimate!!Total:!!", "", .)) %>% 
  select(where(~!all(. == 0)))  # remove columns that contain all 0s 

df_asian[, order(names(df_asian))] %>% 
  select(year, everything(), -Total, Total)

df_graph_asian <- df_asian %>%   
  pivot_longer(cols = -year, names_to = "group", values_to = "count") %>% 
  filter(group != "Total")

df_graph_asian_smaller <- df_graph_asian %>% 
  filter(group != "Filipino" & group != "Chinese, except Taiwanese")

# Graphs----

## Population----

df_pop %>% 
  ggplot(aes(x = year, y = pop, group = 1)) +
  geom_line() +
  # geom_point(color = "white", size = 4.5) +  
  # geom_point(color = "red", size = 3) +  # for appearance: smaller red dot on top of bigger white dot 
  labs(x = NULL) 

## Race

df_graph_asian %>% 
  ggplot(aes(x = year, y = count, color = group)) +  
  geom_line() +
  scale_x_continuous(breaks = seq(min(df_graph_asian$year), max(df_graph_asian$year))) +
  scale_y_continuous(breaks = seq(min(df_graph_asian$count), max(df_graph_asian$count), 2000)) +
  labs(title = "Asian Subgroups in Daly City from 2015-2021",
       x = NULL,
       y = "Number of people",
       caption = "Census Bureau")

df_graph_asian_smaller %>% 
  ggplot(aes(x = year, y = count, color = group)) +  
  geom_line() +
  scale_x_continuous(breaks = seq(min(df_graph_asian_smaller$year), max(df_graph_asian_smaller$year))) +
  scale_y_continuous(breaks = seq(min(df_graph_asian_smaller$count), round(max(df_graph_asian_smaller$count), -2), 100)) +
  labs(title = "Asian Subgroups in Daly City",
       subtitle = paste(min(df_graph_asian_smaller$year), "-", max(df_graph_asian_smaller$year)),
       x = NULL,
       y = "Number of People",
       caption = "Census Bureau",
       color = "")





