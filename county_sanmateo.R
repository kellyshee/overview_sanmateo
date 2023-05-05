# ---------------------------
#
# Script name: county_sanmateo.R
#
# Purpose of script: Pull demographics for cities in specific county (San Mateo) from Census. 
#
# Author: Kelly Shee
#
# Date Created: 2023-05-05
#
# ---------------------------
    
# Notes----
## Currently for 1 state, 1 county inside state, and all cities in that county.
## Find your county's website to scrape city names (if not, modify getCensus).
## State is hardcoded to CA.
## Can change my_areas to look at more areas in/outside CA (then don't use preceding functions?).

rm(list = ls())

# install.packages("pacman")
library(pacman)
p_load(rstudioapi, censusapi, tidyverse, conflicted, magrittr, rvest)
conflicts_prefer(dplyr::filter)

# set directory to this script's source file location
setwd(dirname(getSourceEditorContext()$path))   

# enter census api key (request here: https://api.census.gov/data/key_signup.html)
census_key <- "1c7866fb9cbb353de24dd9ec1d5d13cdb8307f50"

# years (I think cities are 1 year behind while counties are 2 years behind)
current_year <- as.numeric(format(Sys.Date(), "%Y"))
my_vintage <- current_year - 2
my_vintage_5_behind <- my_vintage - 5
my_vintage_10_behind <- my_vintage - 10
my_vintage_all <- c(my_vintage, my_vintage_5_behind, my_vintage_10_behind)

available_census_vars <- listCensusMetadata(name = "acs/acs5",
                                            vintage = my_vintage,
                                            type = "variables")

# Gathering cities & counties----

# download file for US cities and counties (not inclusive)
temp <- tempfile()
download.file("http://download.geonames.org/export/zip/US.zip",temp)
con <- unz(temp, "US.txt")
US <- read.delim(con, header=FALSE)
unlink(temp)

colnames(US)[c(3,5,6)] <- c("place_name","state_name","county_name")

US %<>% 
  select(state_name, county_name, place_name, -starts_with("V")) %>% 
  mutate(county_name = gsub("City and County of |\\s*\\(CA\\)", "", county_name))  # for merging later 

# function: return list of cities or counties
place_county <- function(place_or_county){
  output <- getCensus(name = "acs/acs5",
                      vintage = my_vintage,
                      key = census_key,
                      vars = c("NAME", "B01003_001E"),
                      region = paste0("", place_or_county, ":*")) %>% 
    select(-B01003_001E) %>% 
    separate(NAME, into = c(paste0("", place_or_county, "_name"), "state_name"), sep = ",\\s+") %>%   # 2 columns: county and state  
    mutate(state_name = state.abb[match(state_name, state.name)]) 
  
  if (place_or_county == "county") {
    output$county_name <- gsub(" County| Census Area", "", output$county_name)
  } else {
    output$place_name <- gsub(" city| CDP| town", "", output$place_name)
  }
  
  return(output)
}

# all cities and counties in US
census_cities <- place_county("place")
census_counties <- place_county("county")

# merging all cities and counties (has NAs so check for your areas)
state_county_city <- merge(census_cities, US, by = c("state_name", "place_name"), all.x = T) %>% 
  merge(census_counties, by = c("state", "county_name"), all.x = T) %>% 
  rename(state_name = state_name.x) %>% 
  select(state, county, place, state_name, county_name, place_name) %>% 
  distinct(county, place, .keep_all = TRUE)  # remove repeated cities (due to zip codes) in the same county 

# web scraping city names in county
url <- "https://www.smcgov.org/lafco/cities-san-mateo-county"
page <- read_html(url)                    # download html content
span_node <- page %>% html_nodes("span")  # html element containing info
span_text <- span_node %>% html_text()    # extract text
city_names <- gsub(",.*", "", span_text)  # remove comma + words after it 
city_names <- city_names[-c(1:3,24)]      # your desired cities

# grab city's info from census_cities if it matches desired cities  
my_cities <- census_cities[census_cities$place_name %in% city_names,] %>%  
  dplyr::filter(state_name == "CA") %>% 
  merge(state_county_city, by = c("state", "place", "state_name", "place_name")) %>%
  select(c(state, county, place, state_name, county_name, place_name))

my_cities$county <- my_cities$county[!is.na(my_cities$county)][1]  # fill in NAs - only 1 county
my_cities$county_name <- my_cities$county_name[!is.na(my_cities$county_name)][1]
my_cities$state_name <- state.name[match(my_cities$state_name, state.abb)]  # abb > full name

# BUE <- formatC(8758, width = 5, format = "d", flag = 0)  # b/c BUE's FIPS starts w/0
# cities_counties <- c(BUE,11446,30378,31414,42524,43252,48186,54120,69070,69196,72576,70182,82072,
#                      "county:083", "county:111", "county:079") 

# Key areas----
my_cities_num <- paste0("place:", my_cities$place, "")
my_county <- c(paste0("county:", my_cities$county[1], ""), my_cities$county_name[1])
my_state <- c(paste0("state:", my_cities$state[1], ""), my_cities$state_name[1])
my_areas <- c(my_cities_num, my_county[1], my_state[1])

# this fn will be used inside get_area_stats
get_census <- function(my_vars, my_region, my_regionin, which_vintage = my_vintage){
  if(missing(my_regionin)){
    # state only (no my_regionin)
    output <- getCensus(name = "acs/acs5", 
                        vintage = which_vintage, 
                        key = census_key,
                        vars = c("NAME", my_vars),
                        region = my_region[1]) 
  } else {
    # cities / counties + state (includes my_regionin)
    output <- getCensus(name = "acs/acs5", 
                        vintage = which_vintage, 
                        key = census_key,
                        vars = c("NAME", my_vars),
                        region = my_region[1],
                        regionin = my_regionin[1]) 
  }
  # cleaning area names
  for (i in output$NAME) {
    if (grepl("County", i) == T) {
      output$NAME <- gsub("(County).*", "\\1", output$NAME)    # remove everything after County
    } else
      output$NAME <- gsub("\\s(city|town).*", "", output$NAME) # remove city or town & everything after
  }
  output$year <- which_vintage  # add year column
  output %<>% 
    select(year, NAME, everything())
  return(output)
}

# stats for all areas listed in my_areas
get_area_stats <- function(your_vars, year = my_vintage){
  df_empty <- data.frame()
  for (i in my_areas) {
    if (grepl("state", i) == T) {
      # for "state:*" bc different hierarchy
      output <- get_census(your_vars, i, which_vintage = year)
    } else {
      # for cities & counties
      output <- get_census(your_vars, i, my_state[1], which_vintage = year)
    }
    output %<>% 
      select(-matches("state|county|place"))  # remove columns with area codes
    df_empty <- rbind(df_empty, output)
  }
  return(df_empty)
}

# 1. Population----

df_population <- get_area_stats("B01003_001E")

# df_test <- data.frame()
# for (i in my_vintage_all) {
#   output <- get_area_stats("B01003_001E", i)
#   df_test <- rbind(df_test, output)
# }

# # year1 
# sb_pop_year1 <- population(year1, sb_county, california) 
# ca_pop_year1 <- population(year1, california) 
# # year3
# sb_pop_year3 <- population(year3, sb_county, california) 
# ca_pop_year3 <- population(year3, california) 
# # year2
# sb_pop_year2 <- population(year2, sb_county, california) 
# ca_pop_year2 <- population(year2, california) 
# 
# change <- function(new, old, column){
#   output <- tibble(region = new$region,
#                    year_range = paste(old$year,'-', new$year),
#                    decimal = (new[,c(column)] - old[,c(column)]) / 
#                      old[,c(column)])
#   
#   output <- output %>% mutate(pct = decimal*100)
#   
#   return(output)
# }
# 
# # year2 - year3
# sb_year2_year3 <- change(sb_pop_year3, sb_pop_year2, "total_pop")
# ca_year2_year3 <- change(ca_pop_year3, ca_pop_year2, "total_pop")
# # year3 - year1
# sb_year3_year1 <- change(sb_pop_year1, sb_pop_year3, "total_pop")
# ca_year3_year1 <- change(ca_pop_year1, ca_pop_year3, "total_pop")
# 
# pop_change <- rbind(sb_year2_year3, ca_year2_year3,
#                     sb_year3_year1, ca_year3_year1)
# 
# # 2. Household Characteristics----
# 
# ## 2.1 Size----

df_avg_household_size <- get_area_stats("B25010_001E")

# # year2
# sb_size_year2 <- avg_household_size(year2, sb_county, california)
# ca_size_year2 <- avg_household_size(year2, california)
# # year3
# sb_size_year3 <- avg_household_size(year3, sb_county, california)
# ca_size_year3 <- avg_household_size(year3, california)
# # year1
# sb_size_year1 <- avg_household_size(year1, sb_county, california)
# ca_size_year1 <- avg_household_size(year1, california)
# 
# # change year2 - year3
# sb_change_old <- change(sb_size_year3, sb_size_year2, "persons")
# ca_change_old <- change(ca_size_year3, ca_size_year2, "persons")
# # change year3 - year1
# sb_change <- change(sb_size_year1, sb_size_year3, "persons")
# ca_change <- change(ca_size_year1, ca_size_year3, "persons")
# 
# household_size_list <- rbind(sb_change, ca_change,
#                              sb_change_old, ca_change_old)
# 
# ## 2.2 Composition----
# 
vars_household <- c("B11001_003E","B11001_004E","B11001_008E","B11001_009E")

df_household_type <- get_area_stats(vars_household)
df_household_type %>% 
  rename(`Married-couple families` = B11001_003E, `Other families` = B11001_004E,
         `People living alone` = B11001_008E, `Other nonfamily households` = B11001_009E)


#   output <- output %>% 
#     dplyr::rename(`Married-couple families` = B11001_003E, `Other families` = B11001_004E,
#                   `People living alone` = B11001_008E, `Other nonfamily households` = B11001_009E) %>% 
#     gather() %>%                                         
#     dplyr::rename(household_type = key) %>% 
#     filter(household_type %in%                           
#              c("Married-couple families", "Other families", 
#                "People living alone", "Other nonfamily households")) %>% 
#     mutate(location = my_region[2], year = my_vintage)
#   
#   output$value <- as.double(output$value)                
#   
#   output <- output %>% 
#     mutate(decimal = value / sum(value), pct = decimal * 100)
#   
#   return(output)
# }
# 
# # SB County
# sb_household_year1 <- household_type_fun(year1, sb_county, california)
# sb_household_year2 <- household_type_fun(year2, sb_county, california)
# # CA
# ca_household_year1 <- household_type_fun(year1, california)
# ca_household_year2 <- household_type_fun(year2, california)
# 
# ## SB vs CA households (year1)
# household_comp_year1 <- list(sb_household_year1, ca_household_year1) %>% 
#   reduce(rbind)
# 
# # 3. Age Profile of Population----

df_median_age <- get_area_stats("B01002_001E")

# # median age
# sb_median_age <- list(median_age(year1, sb_county, california), 
#                       median_age(year2, sb_county, california)) %>% 
#   reduce(rbind)
# ca_median_age <- list(median_age(year1, california), median_age(year2, california)) %>% 
#   reduce(rbind)
# 
# # retrieve available_census_vars$name codes
# age_codes <- function(my_age_phrase){
#   output <- available_census_vars %>% 
#     filter(concept == "SEX BY AGE", 
#            str_detect(available_census_vars$label, my_age_phrase))
#   output <- output$name
#   return(output)
# }
# 
# # returns decimals
# age_function <- function(my_age_phrase, my_region, my_regionin){
#   if(missing(my_regionin)){
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", age_codes(my_age_phrase), "B01001_001E"),
#                         region = my_region[1])
#   } else {
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", age_codes(my_age_phrase), "B01001_001E"),
#                         region = my_region[1],
#                         regionin = my_regionin[1])
#   }
#   output <- output %>% 
#     mutate(decimal = as.numeric(sum(across(age_codes(my_age_phrase))) / B01001_001E),
#            year = my_vintage,
#            age_range = my_age_phrase,
#            region = my_region[2])
#   
#   output <- output[,c("region", "year", "age_range", "decimal")]
#   
#   return(output)
# }
# 
# # new age ranges
# new_age_cat <- function(age_1, age_2, new_range){
#   output <- list(age_1, age_2) %>% 
#     reduce(rbind)
#   
#   output[nrow(output) + 1,] <- c(output$region[1], my_vintage,  # creates new row 
#                                  new_range, sum(age_1$decimal, age_2$decimal))
#   
#   output <- output %>% tail(1)  # only last row
#   
#   output$decimal <- as.numeric(output$decimal)
#   
#   return(output)
# }
# 
# ## year1 SB vs CA----
# 
# my_vintage <- year1
# available_census_vars <- listCensusMetadata(name = "acs/acs5", 
#                                             vintage = my_vintage, 
#                                             type = "variables")
# 
# # SB County
# sb_00_05 <- age_function("Under 5", sb_county, california)
# sb_05_09 <- age_function("5 to 9", sb_county, california)
# sb_10_14 <- age_function("10 to 14", sb_county, california)
# sb_15_19 <- age_function("15|19", sb_county, california)
# sb_20_24 <- age_function("20|21|24", sb_county, california)
# sb_25_29 <- age_function("25 to 29", sb_county, california)
# sb_30_34 <- age_function("30", sb_county, california)
# sb_35_39 <- age_function("35", sb_county, california)
# sb_40_44 <- age_function("40", sb_county, california)
# sb_45_49 <- age_function("45", sb_county, california)
# sb_50_54 <- age_function("50", sb_county, california)
# sb_55_59 <- age_function("55 to 59", sb_county, california)
# sb_60_64 <- age_function("60|64", sb_county, california)
# sb_65_69 <- age_function("65|69", sb_county, california)
# sb_70_74 <- age_function("70", sb_county, california)
# sb_75_79 <- age_function("75", sb_county, california)
# sb_80_84 <- age_function("80", sb_county, california)
# sb_85_over <- age_function("85", sb_county, california)
# # new ranges
# sb_25_34 <- new_age_cat(sb_25_29, sb_30_34, "25-34")  
# sb_35_44 <- new_age_cat(sb_35_39, sb_40_44, "35-44")
# sb_45_54 <- new_age_cat(sb_45_49, sb_50_54, "45-54")
# sb_65_74 <- new_age_cat(sb_65_69, sb_70_74, "65-74")
# sb_75_84 <- new_age_cat(sb_75_79, sb_80_84, "75-84")
# 
# # CA
# ca_00_05 <- age_function("Under 5", california)
# ca_05_09 <- age_function("5 to 9", california)
# ca_10_14 <- age_function("10 to 14", california)
# ca_15_19 <- age_function("15|19", california)
# ca_20_24 <- age_function("20|21|24", california)
# ca_25_29 <- age_function("25 to 29", california)
# ca_30_34 <- age_function("30", california)
# ca_35_39 <- age_function("35", california)
# ca_40_44 <- age_function("40", california)
# ca_45_49 <- age_function("45", california)
# ca_50_54 <- age_function("50", california)
# ca_55_59 <- age_function("55 to 59", california)
# ca_60_64 <- age_function("60|64", california)
# ca_65_69 <- age_function("65|69", california)
# ca_70_74 <- age_function("70", california)
# ca_75_79 <- age_function("75", california)
# ca_80_84 <- age_function("80", california)
# ca_85_over <- age_function("85", california)
# # new ranges
# ca_25_34 <- new_age_cat(ca_25_29, ca_30_34, "25-34")  
# ca_35_44 <- new_age_cat(ca_35_39, ca_40_44, "35-44")
# ca_45_54 <- new_age_cat(ca_45_49, ca_50_54, "45-54")
# ca_65_74 <- new_age_cat(ca_65_69, ca_70_74, "65-74")
# ca_75_84 <- new_age_cat(ca_75_79, ca_80_84, "75-84")
# 
# # year1 age megalist 
# age_list <- list(
#   ca_00_05,ca_05_09,ca_10_14,ca_15_19,ca_20_24,ca_25_34,ca_35_44,
#   ca_45_54,ca_55_59,ca_60_64,ca_65_74,ca_75_84,ca_85_over,
#   sb_00_05,sb_05_09,sb_10_14,sb_15_19,sb_20_24,sb_25_34,sb_35_44,
#   sb_45_54,sb_55_59,sb_60_64,sb_65_74,sb_75_84,sb_85_over
# ) %>% 
#   reduce(rbind)
# 
# # 4. Race, Ethnicity, Foreign-Born----
# 
# ## Hispanic % in most recent census year----
# 
# my_vintage <- year1

df_hispanic_or_not <- get_area_stats(c("B03003_003E", "B03003_001E"))

# 
# hispanic_pct <- function(my_region, my_regionin){
#   if(missing(my_regionin)){
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", "B03003_003E", "B03003_001E"), 
#                         region = my_region[1]) %>% 
#       mutate(hispanic = B03003_003E / B03003_001E,
#              non_hispanic = 1 - (B03003_003E / B03003_001E))
#   } else{
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", "B03003_003E", "B03003_001E"), 
#                         region = my_region[1],
#                         regionin = my_regionin[1])
#   }
#   output <- output %>% 
#     mutate(`Hispanic/Latino` = (B03003_003E / B03003_001E)) %>% 
#     mutate(`Non-Hispanic/Latino` = 1 - `Hispanic/Latino`) %>% 
#     gather() %>% 
#     filter(key == "Hispanic/Latino" | key == "Non-Hispanic/Latino") %>% 
#     dplyr::rename(decimal = value) %>% 
#     mutate(year = my_vintage)
#   
#   output$decimal <- as.numeric(output$decimal)
#   
#   output <- output %>% 
#     mutate(pct = scales::percent(decimal),
#            region = my_region[2])
#   
#   return(output)
# }
# 
# sb_hispanic <- hispanic_pct(sb_county, california)
# ca_hispanic <- hispanic_pct(california)
# total_hispanic <- rbind(sb_hispanic, ca_hispanic)
# 
# ## CA non-Hispanic pop by race (% in each category) in most recent census year----
# 
# my_vintage <- year1

vars_race <- c("B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E",
               "B03002_007E", "B03002_008E", "B03002_009E", "B03002_010E",
               "B03002_011E")

df_race_nonhispanic <- get_area_stats(vars_race)


#
# 
# non_hispanic <- function(my_region, my_regionin){
#   if(missing(my_regionin)){
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", vars_race),
#                         region = my_region[1])
#   } else {
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", vars_race),
#                         region = my_region[1],
#                         regionin = my_regionin[1])
#   }
#   output <- output %>% 
#     select(starts_with("B03002")) %>% 
#     dplyr::rename(`White` = B03002_003E, `Black or African American` = B03002_004E, 
#                   `American Indian and Native Alaskan` = B03002_005E, `Asian` = B03002_006E, 
#                   `Native Hawaiian and Other Pacific Islander` = B03002_007E, `Other Race` = B03002_008E) %>% 
#     gather() %>% 
#     dplyr::rename(race = key)
#   
#   output$value <- as.numeric(output$value)
#   
#   output[nrow(output) + 1,] <- 
#     c("Two or More Races", output$value %>% tail(3) %>% sum()) 
#   
#   output <- output %>% 
#     filter(!str_detect(race, "E$")) %>%  # not ending in "E" 
#     mutate(decimal = as.numeric(value) / sum(as.numeric(value))) %>% 
#     mutate(pct = scales::percent(decimal),
#            region = my_region[2], year = my_vintage)
#   
#   output$race <- factor(output$race,  # reorders pie chart, legend 
#                         levels = rev(as.character(output$race)))
#   
#   return(output)
# }

# ## Foreign born in the most recent census year----
# 
# my_vintage <- year1

df_native_or_foreign <- get_area_stats(c("B05012_002E", "B05012_003E", "B05012_001E"))
# 

#   output <- output %>% 
#     dplyr::rename(native = B05012_002E, foreign = B05012_003E, total = B05012_001E) %>% 
#     mutate(across(.cols = c(native, foreign), .fns = ~. / total)) %>% 
#     select(c(native, foreign)) %>% 
#     gather() %>% 
#     dplyr::rename(native_status = key, decimal = value) %>% 
#     mutate(location = my_region[2], year = my_vintage)
#   
#   return(output)
# }

# # 5. Language Skills----
# 
# my_vintage <- year1
# available_census_vars <- listCensusMetadata(name = "acs/acs5", 
#                                             vintage = my_vintage, 
#                                             type = "variables")
# 
census_codes <- function(my_group, my_phrase){
  output <- available_census_vars %>%
    filter(group == my_group,
           str_detect(available_census_vars$label, my_phrase))
  output <- output$name
  return(output)
}

code_english_other <- census_codes("C16002", "Not a limited English")
code_english_limited <- census_codes("C16002", "Limited English")
vars_english <- c("C16002_002E", code_english_other, code_english_limited, "C16002_001E")

df_english_lvl <- get_area_stats(vars_english)

#   output <- output %>% 
#     mutate(english_only = C16002_002E / C16002_001E,
#            english_other = as.numeric(sum(across(english_other_code))) / C16002_001E,
#            english_limited = as.numeric(sum(across(english_limited_code))) / C16002_001E)
#   
#   output <- output[,c("english_only", "english_other", "english_limited")] %>% 
#     gather() %>% 
#     dplyr::rename(english_status = key, decimal = value) %>% 
#     mutate(region = my_region[2], year = my_vintage)
#   
#   return(output)
# }
# 
# sb_eng_status <- english_status(sb_county, california)
# ca_eng_status <- english_status(california)
# 
# eng_status_list <- list(sb_eng_status, ca_eng_status) %>% 
#   reduce(rbind)
# 
# # speak spanish

df_speak_spanish <- data.frame()
for (i in my_cities_num) {
  output <- get_census(c("C16002_003E", "C16002_001E"), i, my_state[1])
  df_speak_spanish <- rbind(df_speak_spanish, output)
}
# 
# spanish <- function(my_vintage, my_region, my_regionin){
#   if(missing(my_regionin)){
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", "C16002_003E", "C16002_001E"), 
#                         region = my_region[1])
#   } else {
#     output <- getCensus(name = "acs/acs5",
#                         vintage = my_vintage,
#                         key = census_key,
#                         vars = c("NAME", "C16002_003E", "C16002_001E"), 
#                         region = my_region[1],
#                         regionin = my_regionin[1])
#   }
#   output <- output %>% 
#     mutate(decimal = C16002_003E/C16002_001E, region = my_region[2], year = my_vintage)
#   
#   output <- output[,c("region", "year", "decimal")]
#   
#   return(output)
# }
# 
# sb_spanish <- spanish(year1, sb_county, california)
# ca_spanish <- spanish(year1, california)
# 
# # 6. Educational Attainment----
# 
# my_vintage <- year1
# available_census_vars <- listCensusMetadata(name = "acs/acs5", 
#                                             vintage = my_vintage, 
#                                             type = "variables")
# 
grade_8_less_code <- census_codes("B15001", "Less than 9")
grade_9_12_code <- census_codes("B15001", "9th to 12th")
hs_grad_code <- census_codes("B15001", "High school graduate")
college_degreeless_code <- census_codes("B15001", "Some college")
associate_code <- census_codes("B15001", "Associate's")
bachelor_code <- census_codes("B15001", "Bachelor's")
graduate_code <- census_codes("B15001", "Graduate")

vars_education <- c(grade_8_less_code,grade_9_12_code,hs_grad_code,college_degreeless_code,
                    associate_code,bachelor_code,graduate_code)

df_edu_lvl <- get_area_stats(vars_education)

#   output <- output %>% 
#     mutate(grade_8_less = as.numeric(sum(across(grade_8_less_code))) / B15001_001E,
#            grade_9_12 = as.numeric(sum(across(grade_9_12_code))) / B15001_001E,
#            hs_grad = as.numeric(sum(across(hs_grad_code))) / B15001_001E,
#            some_college = as.numeric(sum(across(college_degreeless_code))) / B15001_001E,
#            associate = as.numeric(sum(across(associate_code))) / B15001_001E,
#            bachelor = as.numeric(sum(across(bachelor_code))) / B15001_001E,
#            graduate = as.numeric(sum(across(graduate_code))) / B15001_001E) %>% 
#     select(
#       grade_8_less, grade_9_12, hs_grad, some_college, associate, bachelor, graduate
#     ) %>% 
#     gather() %>% 
#     dplyr::rename(edu_attainment = key) %>% 
#     mutate(region = my_region[2], year = my_vintage)
#   
#   return(output)
# }
# 
# # year1
# sb_edu_attainment <- edu_attainment(sb_county, california)
# ca_edu_attainment <- edu_attainment(california)
# 
# edu_attainment_list <- list(sb_edu_attainment, ca_edu_attainment) %>% 
#   reduce(rbind)
# 
# # year2
# 
# my_vintage <- year2
# available_census_vars <- listCensusMetadata(name = "acs/acs5", 
#                                             vintage = my_vintage, 
#                                             type = "variables")
# 
# sb_edu_attainment_year2 <- edu_attainment(sb_county, california)
# ca_edu_attainment_year2 <- edu_attainment(california)
# 
# # 7. Income----
# 
# # CA median household income in each income bracket in the most recent census year----

df_med_household_income <- get_area_stats("B19013_001E")

#   output <- output %>% 
#     dplyr::rename(median_income = B19013_001E) %>% 
#     mutate(year = my_vintage, region = my_region[2])
#   
#   output <- output[,c("region", "year", "median_income")]
#   
#   return(output)
# }
# 
# sb_med_income <- med_household_income(year1, sb_county, california)
# ca_med_income <- med_household_income(year1, california)
# 
# ## % of households in each income bracket ----
# 
# my_vintage <- year1
# available_census_vars <- listCensusMetadata(name = "acs/acs5", 
#                                             vintage = my_vintage, 
#                                             type = "variables")
# 
vars_income <- c("B19101_001E", "B19101_002E", "B19101_003E", "B19101_004E",
                 "B19101_005E", "B19101_006E", "B19101_007E", "B19101_008E",
                 "B19101_009E", "B19101_010E", "B19101_011E", "B19101_012E",
                 "B19101_013E", "B19101_014E", "B19101_015E", "B19101_016E",
                 "B19101_017E")

df_households_per_income_bracket <- get_area_stats(vars_income)

#   output <- output %>% 
#     mutate(across(.cols = starts_with("B19101"), .fns = ~. / B19101_001E)) %>% 
#     mutate(usd_less_15k = rowSums(across(c(B19101_002E,B19101_003E))),
#            usd_150k = rowSums(across(c(B19101_016E, B19101_017E)))) %>% 
#     dplyr::rename(usd_15k_19999 = B19101_004E,
#                   usd_20k_24999 = B19101_005E, usd_25k_29999 = B19101_006E, usd_30k_34999 = B19101_007E,
#                   usd_35k_39999 = B19101_008E, usd_40k_44999 = B19101_009E, usd_45k_49999 = B19101_010E,
#                   usd_50k_59999 = B19101_011E, usd_60k_74999 = B19101_012E, usd_75k_99999 = B19101_013E,
#                   usd_100k_124999 = B19101_014E, usd_125k_149999 = B19101_015E) %>% 
#     gather() %>% 
#     dplyr::rename(income_bracket = key, decimal = value) %>% 
#     filter(str_detect(income_bracket, "usd")) %>% 
#     mutate(region = my_region[2], year = my_vintage)
#   
#   output$decimal <- as.numeric(output$decimal)
#   
#   return(output)
# }
# 
# 
# # GRAPHS----
# 
EFPcolors <- c('deepskyblue4','brown3','darkolivegreen4','cornflowerblue','darkorange3','brown4','mediumpurple4')
# 
# # Note: Changed Exhibit 14 - 30 to Figure 1 - 13.
# 
# ## 1. Population----
# 
# ### Fig 1----
# 
# # year2-year3, year3-year1 pop growth
# fig_01_pop_growth <- pop_change %>% 
#   ggplot(aes(x = region, y = decimal, 
#              fill = factor(year_range, levels = c(year_range[1], year_range[3])))) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.75) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1L),  # round to whole number
#                      breaks = seq(round(min(pop_change$decimal), 1), round(max(pop_change$decimal), 1), by = 0.01)) +
#   scale_fill_manual(values = c("darkolivegreen4", "cornflowerblue")) +
#   labs(x = NULL, 
#        y = "Percent Change", 
#        title = "Figure 1: Population Change by Region",
#        fill = NULL) +
#   theme_bw() +
#   coord_flip() +
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_01_pop_growth.png", width = 16, height = 13, units = "in")
# 
# ## 2. Household Characteristics----
# 
# ### 2.1 Size----
# 
# #### Fig 2----
# 
# # change in avg household size
# fig_02_household_size_change <- household_size_list %>% 
#   ggplot(aes(x = region, y = decimal, 
#              fill = factor(year_range, levels = c(year_range[3], year_range[1])))) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.75) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1L),
#                      breaks = seq(round(min(household_size_list$decimal), 2), round(max(household_size_list$decimal), 2), by = 0.01)) +
#   scale_fill_manual(values = c("darkolivegreen4", "cornflowerblue")) +
#   labs(x = NULL, 
#        y = "Percent Change", 
#        title = "Figure 2: Change in Average Household Size by Region",
#        fill = NULL) +
#   theme_bw() +
#   coord_flip() +
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_02_household_size_change.png", width = 16, height = 13, units = "in")
# 
# ### 2.2 Composition----
# 
# #### Fig 3----
# 
# # Household Composition by Region (year1)
# fig_03_household_comp_region <- household_comp_year1 %>% 
#   ggplot(aes(x = factor(location, 
#                         levels = c(sb_county[2], california[2])),  
#              y = decimal, 
#              fill = factor(household_type, 
#                            levels = c("Other nonfamily households", "People living alone",
#                                       "Other families", "Married-couple families")))) +
#   geom_bar(stat = "identity", width = 0.4) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, 0.1)) +
#   scale_fill_manual(values = c("Other nonfamily households" = "darkolivegreen4",
#                                "People living alone" = "cornflowerblue",
#                                "Other families" = "darkorange3",
#                                "Married-couple families" = "brown4")) +
#   labs(x = NULL, y = "Percent of Households", fill = NULL,
#        title = paste0("Figure 3: Household Composition by Region (", year1,")")) +
#   theme_bw() +
#   guides(fill=guide_legend(ncol=2)) +  # 2x2 legend 
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_03_household_comp_region.png", height=13, width=16, units="in") 
# 
# ## 3. Age Profile of Population----
# 
# ### Fig 4----
# 
# # Age Profile by Region (year1)
# fig_04_age_profile_region <- age_list %>% 
#   ggplot(aes(x = factor(age_range, 
#                         levels = c("Under 5","5 to 9","10 to 14","15|19","20|21|24",
#                                    "25-34","35-44","45-54","55 to 59","60|64",
#                                    "65-74","75-84","85"),
#                         labels = c("0 - 4","5 - 9","10 - 14","15 - 19","20 - 24","25 - 34",
#                                    "35 - 44","45 - 54","55 - 59","60 - 64","65 - 74","75 - 84","85+")), 
#              y = decimal, 
#              fill = factor(region, 
#                            levels = c(sb_county[2], california[2])))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, max(age_list$decimal), by = 0.025)) + 
#   scale_fill_manual(values = c("Santa Barbara County" = "cornflowerblue",
#                                "California"  = "darkorange3")) +
#   labs(x = NULL, 
#        y = "Percent of Population", 
#        title = paste0("Figure 4: Age Profile by Region (", year1, ")"),
#        fill = NULL) +
#   theme_bw() +
#   theme(legend.position="bottom", text = element_text(size = 30),
#         axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(path = "images", "fig_04_age_profile_region.png", height=13, width=16, units="in") 
# 
# ## 4. Race/Ethnicity & Foreign Born----
# 
# ### Fig 5----
# 
# fig_05_hispanic_region <- total_hispanic %>% 
#   ggplot(aes(x = factor(region, levels = c(sb_county[2], california[2])), 
#              y = decimal, 
#              fill = key)) +
#   geom_bar(stat = "identity", width = 0.4) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, by = 0.10)) +
#   scale_fill_manual(values = c("Hispanic/Latino" = "darkolivegreen4",
#                                "Non-Hispanic/Latino" = "cornflowerblue")) +
#   labs(x = NULL, y = "Percent of Population", 
#        fill = NULL, title = paste0("Figure 5: Hispanic/Latino Population by Region (", year1, ")")) +
#   theme_bw() + 
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_05_hispanic_region.png", height=13, width=16, units="in") 
# 
# ### Fig 8----
# 
# fig_06_nonhisp_region <- total_non_hispanic %>% 
#   ggplot(aes(x = factor(region, levels = c(sb_county[2], california[2])), 
#              y = decimal, 
#              fill = factor(race, levels = c("White", "Black or African American",
#                                             "American Indian and Native Alaskan", "Asian",
#                                             "Native Hawaiian and Other Pacific Islander", "Other Race",
#                                             "Two or More Races")))) +
#   geom_bar(stat = "identity", width = 0.4) + 
#   scale_fill_manual(values = EFPcolors) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = "Percent of Population", 
#        fill = NULL, 
#        title = paste0("Figure 6: Non-Hispanic/Latino Population by Region (", year1, ")")) +
#   theme_bw() +
#   theme(text = element_text(size = 30),
#         legend.position = "bottom")+
#   guides(fill=guide_legend(ncol=2)) 
# 
# ggsave(path = "images", "fig_06_nonhisp_region.png", height=13, width=16, units="in") 
# 
# ### Fig 9----
# 
# fig_07_nativity_region <- native_list %>% 
#   ggplot(aes(x = factor(location, 
#                         levels = c(sb_county[2], california[2])), 
#              y = decimal, 
#              fill = factor(native_status,
#                            labels = c("Foreign-born", "Native-born")))) +
#   geom_bar(stat = "identity", width = 0.4) + 
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, by = 0.1)) + 
#   scale_fill_manual(values = c("Foreign-born" = "darkolivegreen4",
#                                "Native-born" = "cornflowerblue")) +
#   labs(x = NULL, y = "Percent of Population", 
#        title = paste0("Figure 7: US Nativity Status by Region (", year1, ")"),
#        fill = NULL) +
#   theme_bw() +
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_07_nativity_region.png", height=13, width=16, units="in") 
# 
# ## 5. Language Skills----
# 
# ### Fig 10 ----
# 
# # English Fluency at Home by Region (year1)
# fig_08_english_region <- eng_status_list %>% 
#   ggplot(aes(x = factor(region,
#                         levels = c(sb_county[2], california[2])),
#              y = decimal, 
#              fill = factor(english_status, 
#                            levels = c("english_limited", "english_other", "english_only"),
#                            labels = c("Less than \"very well\"", 
#                                       "\"Very well\" + other language",
#                                       "English only")))) + 
#   geom_bar(stat = "identity", width = 0.4) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, by = 0.1)) + 
#   scale_fill_manual(values = c("Less than \"very well\"" = "darkolivegreen4",
#                                "\"Very well\" + other language" = "cornflowerblue",
#                                "English only" = "darkorange3")) +
#   labs(x = NULL, 
#        y = "Percent of Households",
#        title = paste0("Figure 8: English Fluency at Home by Region (", year1, ")"),
#        fill = NULL) +
#   theme_bw() + 
#   theme(legend.position="bottom", text = element_text(size = 30))
# 
# ggsave(path = "images", "fig_08_english_region.png", height=13, width=16, units="in") 
# 
# ## 6. Educational Attainment----
# 
# ### Fig 9----
# 
# # Educational Attainment by Region for the Population 25+ Years of Age (year1)"
# fig_09_edu_attain_region <- edu_attainment_list %>% 
#   ggplot(aes(x = factor(region, 
#                         levels = c(sb_county[2], california[2])), 
#              y = value, 
#              fill = factor(edu_attainment, 
#                            levels = c("graduate","bachelor","associate",
#                                       "some_college","hs_grad","grade_9_12","grade_8_less"),
#                            labels = c("Graduate / professional degree",
#                                       "Bachelor's degree", "Associate's degree",
#                                       "Some college, no degree", 
#                                       "High school graduate / equivalency",
#                                       "9th to 12th grade, no diploma",
#                                       "Less than 9th grade")))) +
#   geom_bar(stat = "identity", width = 0.4) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, 1, by = 0.1)) + 
#   scale_fill_manual(values = EFPcolors) +
#   labs(x = NULL, y = "Percent of Population", 
#        title = paste0("Figure 9: Educational Attainment by Region (", year1, ")"),
#        fill = NULL,
#        subtitle = "Population 25+ Years of Age") +
#   theme_bw() +
#   theme(text = element_text(size = 30),
#         plot.subtitle = element_text(face = "italic"),
#         legend.position = "bottom")+
#   guides(fill=guide_legend(ncol=2)) 
# 
# ggsave(path = "images", "fig_09_edu_attain_region.png", height=13, width=16, units="in") 
# 
# ## 7. Income Distribution----
# 
# ### Fig 10----
# 
# # Income Distribution by Region (year1)
# fig_10_income_dist <- income_list %>% 
#   ggplot(aes(x = factor(income_bracket, 
#                         level = c('usd_less_15k', 'usd_15k_19999',
#                                   'usd_20k_24999','usd_25k_29999','usd_30k_34999',
#                                   'usd_35k_39999','usd_40k_44999','usd_45k_49999',
#                                   'usd_50k_59999','usd_60k_74999','usd_75k_99999',
#                                   'usd_100k_124999','usd_125k_149999',
#                                   'usd_150k'),
#                         labels = c("< $15,000","$15,000 - $19,999",
#                                    "$20,000 - $24,999","$25,000 - $29,999","$30,000 - $34,999",
#                                    "$35,000 - $39,999","$40,000 - $44,999","$45,000 - $49,999",
#                                    "$50,000 - $59,999","$60,000 - $74,999","$75,000 - $99,999",
#                                    "$100,000 - $124,999","$125,000 - $149,999","$150,000+")), 
#              y = decimal, 
#              fill = factor(region,
#                            levels = c(sb_county[2], california[2])))) + 
#   geom_bar(stat = "identity", position = "dodge", width = 0.75) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  # sets max width of labels
#   scale_y_continuous(labels = scales::percent,
#                      breaks = seq(0, max(income_list$decimal), by = 0.025)) + 
#   scale_fill_manual(values = c("Santa Barbara County" = "cornflowerblue",
#                                "California"  = "darkorange3")) +
#   labs(x = NULL, 
#        y = 'Percent of Households', 
#        title = paste0("Figure 10: Income Distribution by Region (", year1, ")"),
#        fill = NULL) +
#   theme_bw() +
#   theme(legend.position="bottom", text = element_text(size = 30),
#         axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(path = "images", "fig_10_income_dist.png", height=13, width=16, units="in")
