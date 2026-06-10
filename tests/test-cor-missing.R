library(legislators)
library(tidyverse)
library(magrittr)

missing_files <- here::here("data", "agencies") |> str_replace("legislators-data", "correspondence_data") |> 
list.files(pattern = "-missing.rda", full.names = T)

load(missing_files[92])

missing |> filter(nchar(FROM)< 150 ) |> group_by(FROM) |>  tally(n, sort = T) |> filter(n>1) |> head(20) |> kable(caption = "These people are affilliated with US congress. What is their affiliation?") |> print() 

d <- missing 
  
for(i in missing_files){
  message(i)
  load(i)
  d <<- full_join(d, missing) |> ungroup() 
}

d %<>% mutate(FROM = str_squish(FROM))

d |> count(FROM, sort = T) |> filter(n>1) |> head(100) |> kable()

# sample 
d |> drop_na(congress) |> 
  count(FROM) |> 
  filter(nchar(FROM) < 1000) |> 
  slice_max(n, n = 30) |> 
  arrange(-n) |> 
  kable()

# sample including congress 
d |> drop_na(congress) |> 
  count(FROM, congress) |> 
  filter(nchar(FROM) < 1000) |> 
  slice_max(n, n = 100) |> 
  arrange(-n) |> 
  kable()

# MYSTERIES 
#  |dutch ruppersberger             |      111|  4|
#  |dutch ruppersberger             |      112|  4|
# |radewagen, aumua amata                  |      114|  12|DHS_USCIS_2016 |
# |representative blunt-rochester    |      116|  16|VA     |

#TODO list: x = fixed 

# |FROM                      | congress|    n|agency    |
#   |:-------------------------|--------:|----:|:---------|
#   |bera, amerish             |      115|  121|DHS_USCIS | x 
#   |bordallo, madeleine       |      115|   38|DHS_USCIS | x not in congress 
#   |brooks, morris            |      115|  150|DHS_USCIS | x 
#   |cheney, elizabeth         |      115|   35|DHS_USCIS | x
#   |farenthold, randolph      |      115|   28|DHS_USCIS | x 
#   |gibbs, robert             |      115|   17|DHS_USCIS | x 
#   |gonzález colón, jenniffer |      115|   50|DHS_USCIS | x non-voting 
#   |gottheimer, joshua        |      115|  226|DHS_USCIS | x
#   |gowdy, harold             |      115|  125|DHS_USCIS | x
#   |guthrie, steven           |      115|   52|DHS_USCIS | x
#   |isakson, john             |      115| 1031|DHS_USCIS | x
#   |kelly, george             |      115|  107|DHS_USCIS | x
#   |khanna, rohit             |      115|  547|DHS_USCIS | x
#   |knight, stephen           |      115|   90|DHS_USCIS | x
#   |mcnerney, gerald          |      115|  102|DHS_USCIS | x
#   |messer, allen             |      115|   14|DHS_USCIS | x
#   |mooney, alexander         |      115|   26|DHS_USCIS | x
#   |norton, eleanor           |      115|  218|DHS_USCIS | x nonvoting
#   |nugent, richard           |      115|    1|DHS_USCIS | x not in congresss 
#   |o'rourke, robert          |      115|  523|DHS_USCIS | x 
# |perlmutter, edwin         |      115|   35|DHS_USCIS | x
# |plaskett, stacey          |      115|   78|DHS_USCIS | not in congress 
# |radewagen, aumua amata    |      115|   40|DHS_USCIS | ??????
# |rokita, theodore          |      115|   50|DHS_USCIS | x 
# |ruppersberger, charles    |      115|  171|DHS_USCIS | X 
# |sablan, gregorio          |      115|  204|DHS_USCIS | not in congress 
# |scalise, stephen          |      115|   76|DHS_USCIS | x
# |scott, james              |      115|  130|DHS_USCIS |x 
# |sewell, terrycina         |      115|   70|DHS_USCIS |x 
# |vacant, becerra           |      115|    1|DHS_USCIS |
# |vacant, bridenstine       |      115|   24|DHS_USCIS |
# |vacant, chaffetz          |      115|    4|DHS_USCIS |
# |vacant, dent              |      115|   14|DHS_USCIS |
# |vacant, desantis          |      115|    6|DHS_USCIS |
# |vacant, meehan            |      115|   51|DHS_USCIS |
# |vacant, mulvaney          |      115|    1|DHS_USCIS |
# |vacant, murphy            |      115|   14|DHS_USCIS |
# |vacant, pompeo            |      115|    1|DHS_USCIS |
# |walorski, jacqueline      |      115|  128|DHS_USCIS | x
# |wilson, addison           |      115|   65|DHS_USCIS | x

# |representative duncan        |      115|  1|DOD_USACE | ambig
#   |representative farentholt    |      115|  2|DOD_USACE | typo
#   |representative grajilva      |      115|  1|DOD_USACE | typo
#   |representative graves        |      115|  3|DOD_USACE | ambig
#   |representative herrera       |      115|  1|DOD_USACE | x 
#   |representative jenkins       |      115|  2|DOD_USACE | ambig
#   |representative johnson       |      114|  1|DOD_USACE | ambig
#   |representative jones         |      115|  6|DOD_USACE | ambig
#   |representative kelly         |      113|  1|DOD_USACE | ambig
#   |representative kelly         |      115|  2|DOD_USACE | ambig
#   |representative letter        |      114|  1|DOD_USACE | ? 
#   |representative lipinksi      |      114|  1|DOD_USACE | typo 


# |o'rourke, robert                        |      114| 489|DHS_USCIS_2016 | x 
# |rigell, edward                          |      114| 382|DHS_USCIS_2016 | x
# |norton, eleanor                         |      114| 233|DHS_USCIS_2016 | nonvoting
# |sablan, gregorio                        |      114| 215|DHS_USCIS_2016 |  ????????????????????????????????
# |graves, john                            |      114| 107|DHS_USCIS_2016 | x
# |plaskett, stacey                        |      114|  89|DHS_USCIS_2016 |x 
# |pierluisi, pedro                        |      114|  80|DHS_USCIS_2016 |x
# |neugebauer, robert                      |      114|  63|DHS_USCIS_2016 | x 
# |perlmutter, edwin                       |      114|  62|DHS_USCIS_2016 | x
# |sewell, terrycina                       |      114|  42|DHS_USCIS_2016 |x
# |mulvaney, john                          |      114|  38|DHS_USCIS_2016 | x
# |bordallo, madeleine                     |      114|  37|DHS_USCIS_2016 | no longer in congress 
# |vacant, vacant                          |      114|  15|DHS_USCIS_2016 | x 
# |radewagen, aumua amata                  |      114|  12|DHS_USCIS_2016 | ??????
# |grimm, michael                          |      114|   9|DHS_USCIS_2016 | not in congress 
# |hastings, doc                           |      114|   6|DHS_USCIS_2016 | no longer in congress
# |foreign relations - senate              |      114|   5|DHS_USCIS_2016 | issue 221
# |gerlach, jim                            |      114|   5|DHS_USCIS_2016 | no longer in congress
# |bachus, spencer t                       |      114|   4|DHS_USCIS_2016 | no longer in congress 
# |chambliss, saxby                        |      114|   4|DHS_USCIS_2016 | no longer in congress 
# |judiciary - house                       |      114|   4|DHS_USCIS_2016 | issue 221
# |wolf, frank r                           |      114|   3|DHS_USCIS_2016 | no longer in congress
# |energy and natural resources - senate   |      114|   2|DHS_USCIS_2016 | issue 221
# |oversight and government reform - house |      114|   2|DHS_USCIS_2016 | issue 221
# |radel, trey                             |      114|   2|DHS_USCIS_2016 | no longer in congress 
# |congressional budget office             |      114|   1|DHS_USCIS_2016 | issue 221
# |congressional research service          |      114|   1|DHS_USCIS_2016 | issue 221
# |kingston, jack                          |      114|   1|DHS_USCIS_2016 | no longer in congress 
# |waxman, henry a                         |      114|   1|DHS_USCIS_2016 | no longer in congress 

d1 <- d |> 
  #filter(agency %in% c("DHS_USCIS", "DHS_USCIS_2016")) |> 
  extractMemberName("FROM", 
                  members = members, 
                  congress = "congress") |> 
  # newly matched 
  drop_na(bioname, congress) 


# newly matched
d1 |>
  count(FROM, bioname, sort = T) |> 
  # filter(nchar(FROM) < 1000) |> 
  slice_max(n, n = 100) |> 
  kable()

# newly matched by congress 
d1 |> 
  count(FROM, bioname, congress, 
        agency,
        sort = T) |> 
  # filter(#str_detect(FROM, "senat|repres|cong|house"),
  #   nchar(FROM) < 1000) |> 
  slice_max(n, n = 100) |> 
  arrange(-n) |> 
  kable()


# Archive changes for future testing (these are good ones to test on because output changed based on changes to make_members_data.R)
d1 %>% distinct(FROM, bioname, congress) %>% 
  save(file = here::here("tests", "out", paste0("cor-missing-new-matches", Sys.time(), ".rda")))

d1 |> distinct(FROM, bioname, congress) |> 
  write_csv(here::here("tests", "out", "cor-missing-new-matches.csv"))

# trying again after changing members 
d1 |>
  #filter(agency %in% c("DHS_USCIS", "DHS_USCIS_2016")) |> 
  extractMemberName("FROM", 
                    members = members, 
                    congress = "congress") |> 
  # newly matched 
  drop_na(bioname, congress) 


# GPT fixes table (many have already been fixed---needs to be per congress )
fixes <- read_csv(here::here("tests", "fixes.csv"))

fixes <- fixes |>  mutate(FROM = from) |> left_join(d1)

extractMemberName(fixes, "from",
                  members= members,
                  congress = 105) |> 
  drop_na(bioname)
