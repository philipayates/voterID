library(tidyverse)

# Read in data
congressional.2018.county <- read.csv("congressional-2018-county.csv")

# Select variables for demographics
congress.2018.demo.1 <- congressional.2018.county %>%
  select(fips:ruralurban_cc,voterID)

# Convert NA's to 0's
congress.2018.demo.2 <- congress.2018.demo.1 %>%
  mutate(demsen16=ifelse(!is.na(demsen16),demsen16,0),
         repsen16=ifelse(!is.na(repsen16),repsen16,0),
         othersen16=ifelse(!is.na(othersen16),othersen16,0),
         demhouse16=ifelse(!is.na(demhouse16),demhouse16,0),
         rephouse16=ifelse(!is.na(rephouse16),rephouse16,0),
         otherhouse16=ifelse(!is.na(otherhouse16),otherhouse16,0),
         demgov16=ifelse(!is.na(demgov16),demgov16,0),
         repgov16=ifelse(!is.na(repgov16),repgov16,0),
         othergov16=ifelse(!is.na(othergov16),othergov16,0))

# Take averages to get values of demographics for each FIPS
congress.2018.demo.3 <- congress.2018.demo.2 %>%
  group_by(fips) %>%
  summarize(rep.past=mean(trump16+romney12+repsen16+rephouse16+repgov16),
            dem.past=mean(clinton16+obama12+demsen16+demhouse16+demgov16),
            other.past=mean(otherpres16+otherpres12+othersen16+otherhouse16+othergov16),
            cvap=mean(cvap),white_pct=mean(white_pct),black_pct=mean(black_pct),
            hispanic_pct=mean(hispanic_pct),nonwhite_pct=mean(nonwhite_pct),
            foreignborn_pct=mean(foreignborn_pct),female_pct=mean(female_pct),
            age29andunder_pct=mean(age29andunder_pct),age65andolder_pct=mean(age65andolder_pct),
            median_hh_inc=mean(median_hh_inc),clf_unemploy_pct=mean(clf_unemploy_pct),
            lesshs_pct=mean(lesshs_pct),lesscollege_pct=mean(lesscollege_pct),
            lesshs_whites_pct=mean(lesshs_whites_pct),lesscollege_whites_pct=mean(lesscollege_whites_pct),
            rural_pct=mean(rural_pct),ruralurban_cc=mean(ruralurban_cc),voterID=mean(voterID))

# Merge with county.votes
county.votes <- read.csv("county-votes.csv")
county.demographics <- county.votes %>%
  inner_join(congress.2018.demo.3,by="fips")
write_csv(county.demographics,"county-demographics.csv")
