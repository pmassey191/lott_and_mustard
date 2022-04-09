

crime_data <- read_dta(here("Data/UpdatedStateLevelData-2010.dta")) %>% 
  filter(year>=1997 & year <= 2000)

table_1 <- crime_data

