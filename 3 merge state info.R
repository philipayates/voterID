library(tidyverse)

# Read in county data
congressional.2018.county <- read.csv("congressional-2018-county.csv")

# Read in file containing state information
state.info <- read.csv("state_info.csv")

# Pull the unique state FIPS for each county FIPS code
congress.2018.state.v1 <- congressional.2018.county %>%
  group_by(fips) %>%
  summarize(state_fips=mean(state_fips))

# Merge the state info into previously created data set
congress.2018.state.v2 <- congress.2018.state.v1 %>%
  left_join(state.info,id="state_fips")

# Merge this with county demographics for final data set
county.demographics <- read.csv("county-demographics.csv")
final.data <- county.demographics %>%
  inner_join(congress.2018.state.v2,id="fips")
final.data.v2 <- final.data %>%
  filter(!is.na(median_hh_inc))
write_csv(final.data.v2,"final-data-VID.csv")
