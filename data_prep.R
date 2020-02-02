# Read in 2010 population data
counties <- read_rds(
  "data/counties.rds"
  ) %>% 
  as_tibble() %>% 
  mutate(
    county = str_remove(name, ".*,") %>% str_trim(),
    state = str_remove(name, ",.*") %>% str_trim()
  ) %>% 
  select(-name)

# Read in mapping data
us_map <- maps::map("county", plot = FALSE, fill = TRUE) %>% 
  st_as_sf() %>% 
  mutate(
    county = str_remove(ID, ".*,") %>% str_trim(),
    state = str_remove(ID, ",.*") %>% str_trim()
  ) %>% 
  select(-ID)

# # Check that state names overlap 
#  setdiff(counties$county, us_map$county)
#  setdiff(us_map$county, counties$county)
 
 #fix county match for miami-dade
counties<- counties %>% 
   mutate(county = replace(county, 
                           state == "florida" & county == "dade", 
                           "miami-dade"))
#creat final dataset 
county_data <- left_join(us_map, counties, by = c("state", "county"))

# Read in mapping data
state_map <- maps::map("state", plot = FALSE, fill = TRUE) %>% 
  st_as_sf() %>% 
  rename(state = ID)

# state names for widget 
state_name_widget <- county_data %>% 
  pull(state) %>% 
  unique() %>% 
  str_to_title()

