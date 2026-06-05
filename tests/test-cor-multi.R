library(legislators)
library(tidyverse)
library(magrittr)
library(knitr)

multi_files <- here::here("data", "agencies") |> str_replace("legislators-data", "correspondence_data") |> 
  list.files(pattern = "-multi", full.names = T)

load(multi_files[1])
d <- multi |> drop_na(congress)

for(i in multi_files){
  message(i)
  load(i)
  d <<- full_join(d, multi) |> ungroup() 
}

d %<>% drop_na(congress)

d1 <- d |> distinct(FROM, congress) |> 
  extractMemberName("FROM", 
                         members = members, 
                         congress = "congress")

# no longer matching
d1 %>% 
  count(FROM, congress, bioname, sort = T) %>% 
  filter(is.na(bioname))

# check letters with multiple authors (or possible false matches) 
multi <- d1 %>% 
  distinct(FROM, congress, bioname) %>% 
  ungroup() %>% 
  group_by(FROM, congress) %>% 
  add_count() %>% 
  filter(n>1, nchar(FROM) > 6, nchar(FROM)<100 )  %>% 
  ungroup() %>% 
  arrange(nchar(FROM)) %>% 
  ungroup() 

# still possibly duplicates 
head(multi, 200) |> kable(caption = "Multi-author letters (or possible false matches) ") |> print() 

# not congress specific + sampling 
multi |> distinct(FROM, bioname) |> head(100) |> kable()

multi |> write_csv(here::here("tests", "out", "cor-multi-matches.csv"))


# TROUBLE SHOOTIUNG CODE FOR PROBLEM STRINGS 

# solved 

problem <- "s king"

########################################
# SHOULD BE SOLVED, but not tested 
######################################

# |representative hyde-smith, c                                                               |      115|HYDE-SMITH, Cindy                   |  1|VA     |
#   |representative hyde-smith, c                                                               |      115|SMITH, Christopher Henry            |  1|VA     |
#   |representative hyde-smith, c                                                               |      115|SMITH, Tina                         |  1|VA     |
problem <- "smith, c"  

#   |representative mccarthy, carolyn o                     |      113|MCCARTHY, Carolyn        |  1|DOD_Navy |
#   |representative mccarthy, carolyn o                     |      113|MCCARTHY, Kevin          |  1|DOD_Navy |x
problem <- "representative mccarthy, ca"  

#   |representative murphy, patrick o                       |      113|MURPHY, Patrick          |  1|DOD_Navy |
#   |representative murphy, patrick o                       |      113|MURPHY, Timothy          |  1|DOD_Navy |x
problem <- "representative murphy, pa"  

#   |representative scott garrett nj |      111|GARRETT, Scott |  1|Amtrak |
#   |representative scott garrett nj |      111|SCOTT, David   |  1|Amtrak |
problem <- "representative scott"
# |johnson, timothy |      112|JOHNSON, Timothy Peter (Tim) |  6|DHHS_CMS |
# |johnson, timothy |      112|JOHNSON, Timothy V.          |  6|DHHS_CMS |
problem <- "johnson, tim" # SHOULD BE FIXED 
#   |representative bennie g thompson                               |      111|THOMPSON, Bennie             | 74|DHS_HQ | 
#   |representative bennie g thompson                               |      111|THOMPSON, Glenn              | 74|DHS_HQ | x
problem <- "g thompson"   


#   |representative edward m kennedy                                |      109|KENNEDY, Edward Moore (Ted)  | 47|DHS_HQ |
#   |representative edward m kennedy                                |      109|KENNEDY, Mark                | 47|DHS_HQ | x 
problem <- "m kennedy"  

#   |senator susan m collins                                        |      108|COLLINS, Michael Allen (Mac) | 11|DHS_HQ | x
#   |senator susan m collins                                        |      108|COLLINS, Susan Margaret      | 11|DHS_HQ | 
problem <- "m collins"  

#   |representative susan a davis                                   |      111|DAVIS, Artur                 |  7|DHS_HQ | x 
#   |representative susan a davis                                   |      111|DAVIS, Susan A.              |  7|DHS_HQ |
problem <- "a davis"  

#   |representative christopher s murphy                            |      111|MURPHY, Christopher          |  3|DHS_HQ |
#   |representative christopher s murphy                            |      111|MURPHY, Scott                |  3|DHS_HQ |x
problem <- "s murphy"  

#   |representative lynn a westmoreland                             |      112|WEST, Allen                  |  3|DHS_HQ |X
#   |representative lynn a westmoreland                             |      112|WESTMORELAND, Lynn A.        |  3|DHS_HQ |
problem <- "a west"  

#   |representative michael r turner                                |      112|TURNER, Michael R.           |  2|DHS_HQ |
#   |representative michael r turner                                |      112|TURNER, Robert L.            |  2|DHS_HQ | x
problem <- "r turner"  

#   |representative walter b jones                                  |      115|JONES, Brenda                |  1|DHS_HQ | x
#   |representative walter b jones                                  |      115|JONES, Walter Beaman, Jr.    |  1|DHS_HQ | 
problem <- "b jones"  

#   |ron h johnson        |      116|JOHNSON, Hank       |  2|DOJ_CIV | x 
#   |ron h johnson        |      116|JOHNSON, Ron        |  2|DOJ_CIV |
problem <- "h johnson"  

#   |lewis, j |      110|LEWIS, Charles Jeremy (Jerry) |  4|DOD_OSDJS | x
#   |lewis, j |      110|LEWIS, John R.                |  4|DOD_OSDJS | x 
problem <- "lewis, j"  


###################
# UNSOLVED 
#####################






#TODO drop chamber_last for everyone whose first name is a last name or last name is a first name in a congress 
#   |representative john b larson                                   |      108|JOHN, Christopher            |  2|DHS_HQ |X
#   |representative john b larson                                   |      108|LARSON, John B.              |  2|DHS_HQ |X
problem <- "representative john"  







filter(members, pattern |> str_detect(problem), congress > 100) |> distinct(pattern)
filter(members_all, pattern |> str_detect(problem), congress > 100) |> select(any_of(matches("_last"))) |> distinct() |> kable()



multi |> 
  filter(str_detect(FROM, "blunt"))  |> 
  distinct(FROM, congress) |> 
  extractMemberName("FROM", 
                  members = members, 
                  congress = "congress") |> 
  add_count(data_id) |> 
  filter(n>1) |> 
  kable()

