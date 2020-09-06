library(tidyverse)

# Read in data
all.offices.2018.County <- read.csv("all-offices-2018-County.csv")

# Remove rows with FIPS NA
all.offices.2018.County.noNA <- all.offices.2018.County %>%
  filter(!is.na(fips))

# Select only congressional data
congressional.2018.county <- all.offices.2018.County.noNA %>%
  filter(office=="US Representative" | office=="US Representative (Partial Term Ending 01/03/2019)")
write_csv(congressional.2018.county,"congressional-2018-county.csv")

# Convert parties to democrat, republican, or other
congress.2018 <- congressional.2018.county %>%
  select(fips,party,candidatevotes) %>%
  mutate(party=ifelse(party %in% c("democrat","democratic-farmer-labor","democratic-npl"),"democrat",ifelse(party=="republican","republican","other")))

# Convert NA votes to 0 votes
congress.2018 <- congress.2018 %>%
  mutate(candidatevotes=ifelse(!is.na(candidatevotes),candidatevotes,0))

# Sum party votes for each FIPS
congress.2018.v2 <- congress.2018 %>%
  group_by(fips,party) %>%
  summarize(votes=sum(candidatevotes))
congress.2018.wide <- congress.2018.v2 %>%
  pivot_wider(names_from=party,values_from=votes)

# Convert NA votes to 0 votes
congress.2018.wide <- congress.2018.wide %>%
  mutate(democrat=ifelse(!is.na(democrat),democrat,0),
         republican=ifelse(!is.na(republican),republican,0),
         other=ifelse(!is.na(other),other,0))

# Create other variables
congress.2018.wide <- congress.2018.wide %>%
  rename(votes.dem=democrat,votes.rep=republican,votes.other=other)

# Use only contested races: democrat versus republican
congress.2018.wide <- congress.2018.wide %>%
  filter(votes.dem!=0 & votes.rep!=0) %>%
  mutate(total=votes.dem+votes.rep+votes.other,
         share.dem=votes.dem/total,
         share.rep=votes.rep/total,
         share.other=votes.other/total,
         Y=log(votes.rep/votes.dem))
  
write_csv(congress.2018.wide,"county-votes.csv")  

