library(legislators)
library(tidyverse)
library(magrittr)

missing_files <- here::here("data", "agencies") |> str_replace("legislators-data", "correspondence_data") |> 
list.files(pattern = "-missing", full.names = T)

load(missing_files[10])

d <- missing 
  
for(i in missing_files){
  message(i)
  load(i)
  d <<- full_join(d, missing) |> ungroup() 
}

# CONGRESS VAR 
year_congress<- function(year){
  return(floor((year - 1787)/2))
}

d %<>% mutate(
  year = str_sub(DATE, 1,4) |> as.numeric(), 
  congress = year_congress(year))

d %<>% mutate(FROM = str_squish(FROM))

d |> count(FROM, sort = T) |> filter(n>1) |> kable()

# sample 
d |> drop_na(congress) |> 
  count(FROM) |> 
  filter(nchar(FROM) < 1000) |> 
  slice_max(n, n = 100) |> 
  arrange(-n) |> 
  kable()

# sample including congress 
d |> drop_na(congress) |> 
  count(FROM, congress) |> 
  filter(nchar(FROM) < 1000) |> 
  slice_max(n, n = 100) |> 
  arrange(-n) |> 
  kable()

d1 <- d |> extractMemberName("FROM", 
                  members = members, 
                  congress = "congress")


# sample 
d1 |> drop_na(congress) |> 
  count(FROM, bioname) |> 
  filter(nchar(FROM) < 1000) |> 
  slice_sample(n = 200) |> 
  arrange(-n) |> 
  kable()

# sample 
d1 |> drop_na(congress) |> distinct(FROM, congress) |> 
  filter(#str_detect(FROM, "senat|repres|cong|house"),
         nchar(FROM) < 1000) |> slice_sample(n = 200) |> kable()

# newly matched
d1 |> drop_na(bioname, congress) |> filter(nchar(FROM) < 100) |> distinct(FROM, bioname, congress) |> head(200) |> kable()

members_all |> filter(bioname == "SMITH, Christopher Henry") |> distinct(pattern) 

d1 |> drop_na(bioname, congress) |> distinct(FROM, bioname, congress) |> write_csv(here::here("tests", "out", "cor-missing-new-matches.csv"))


fixes <- read_csv(here::here("tests", "fixes.csv"))

fixes <- fixes |>  mutate(FROM = from) |> left_join(d1)

extractMemberName(fixes, "from",
                  members= members,
                  congress = 105) |> 
  drop_na(bioname)
