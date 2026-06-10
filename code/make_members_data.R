# This script aguments member names from voteview to enable merging with wide variety of name formats. 
# this script creates members.rda
library(tidyverse)
library(magrittr)
source("code/stateFromLower.R")


# old method used Rvoteview package. Now using bulk data. http://voteview.com/data
members <- read_csv(here::here("data", "HSall_members.csv"))

## Not currently using the parties data 
# parties <- read_csv(here::here("data", "HSall_parties.csv"))


  # format state full names from abbrev
 members %<>%
   mutate(state = stateFromLower(state_abbrev))
   
 if("party_size" %in% names(members)){
   members%<>%
  group_by(chamber, party_code, congress) %>%
   mutate(party_size = n()) %>% 
   ungroup()
 }
   
   
   members %<>%
    # mutate(state = as.character(state)) %>%
    # extract first, middle, last, and common names
    mutate(last_name = gsub(", .*", "", bioname)) %>%
    mutate(first_name = gsub("^.*?, |, Jr.| Jr.|, III| III| IV", "", bioname)) %>% # this messes up Joe Kennedy Jr, Joe Kennedy III, but need to correct in MemberNameDateCorrections.R function because matching this with nameMethods is unrealistic and they do not overlap
    mutate(first_name = gsub(", II| II", "", first_name)) %>% 
    mutate(common_name = stringr::str_extract(bioname, "\\(.*\\)")) %>%
    mutate(common_name = gsub("\\)|\\(", "", common_name)) %>%
    mutate(first_name = gsub("\\(.*\\)", "", first_name)) %>%
    mutate(middle_name = stringr::str_extract(first_name, " .*")) %>%
    mutate(middle_name = str_squish(middle_name)) %>%
    mutate(middle_initial = substr(middle_name, 1, 1)) %>%
    mutate(first_name = first_name %>% str_squish() %>% str_remove(" .*")) %>%
    mutate(common_name = ifelse(is.na(common_name), "", common_name)) %>%
    mutate(first_initial = str_sub(first_name, 1,1)) %>% 
    mutate(last_name = ifelse(last_name == "MCCARTHY", "McCARTHY", last_name)) %>% # IS THIS A TYPO FROM VOTEVIEW, OR ARE THEY ALL LIKE THIS?
     
     # additional last names
     mutate(add_last_name = NA) %>%
     mutate(add_last_name = ifelse(bioname == "BONO, Mary", "Mack", add_last_name)) %>%
   
     # maiden names
     mutate(maiden_name = NA) %>%
     mutate(maiden_name = ifelse(bioname == "CORTEZ MASTO, Catherine Marie", "(CORTEZ-MASTO|MASTRO)", maiden_name)) %>% #FIXME MASTRO IS A TYPO AND SHOULD BE IN THE TYPOS TABLE INSTEAD 
     mutate(maiden_name = ifelse(bioname == "GONZÁLEZ-CÓLON, Jenniffer", "(GONZALEZ-COLON|GONZALEZ COLON)", maiden_name)) %>% 
     mutate(maiden_name = ifelse(bioname == "WATSON COLEMAN, Bonnie", "COLEMAN", maiden_name)) %>% 
     mutate(maiden_name = ifelse(bioname == "BLUNT ROCHESTER, Lisa", "BLUNT-ROCHESTER", maiden_name)) %>% #FIXME this did not seem to work - it creats the pattern "representative blunt-rochester" but this did not match "representative blunt-rochester" in the VA data for the 116th
     mutate(maiden_name = ifelse(bioname == "TORRES SMALL, Xochitl", "(TORRES-SMALL|SMALL)", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "Bailey", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "HASSAN, Margaret (Maggie)", "Wood", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "KUSTER, Ann McLane", "McLane", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "SMITH, Tina", "Flint", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "BONO, Mary", "(Mack Bono|Bono Mack|Mack)", maiden_name)) %>% #Mack really isn't her maiden name
    mutate(maiden_name = ifelse(bioname == "CAPITO, Shelley Moore", "Moore", maiden_name)) %>%
    mutate(maiden_name = ifelse(bioname == "FEINSTEIN, Dianne", "(Goldman|Berman|Goldman Berman)", maiden_name)) %>%
    mutate(maiden_name = ifelse(bioname == "ERNST, Joni", "Culver", maiden_name)) %>%
    mutate(maiden_name = ifelse(bioname == "FISCHER, Debra (Deb)", "Strobel", maiden_name)) %>%
    mutate(maiden_name = ifelse(bioname == "WARREN, Elizabeth", "Herring", maiden_name)) %>%
    mutate(maiden_name = ifelse(bioname == "AYOTTE, Kelly", "Daley", maiden_name)) %>%
     mutate(maiden_name = ifelse(bioname == "McMORRIS RODGERS, Cathy", "McMORRIS", maiden_name)) %>%
     mutate(maiden_name = ifelse(bioname == "LUJAN GRISHAM, Michelle", "(LUJAN|GRISHAM)", maiden_name)) %>% # Lujan is Maiden, but she is called M Grisham
     mutate(maiden_name = ifelse(bioname == "Jaime HERRERA BEUTLER", "(HERRERA BEUTLER|HERRERA-BEUTLER|HERRERA|BEUTLER)", maiden_name)) %>% # Herrera is Maiden
     
    
     # common names
     # NOTE, as written this will overwrite existing common names. 
     # FIXME by adding "common_name == "" &" unless we want to overwrite
     mutate(common_name = ifelse(first_name == "Andy" & common_name == "", "Andrew", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Constance" & common_name == "", "Connie", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Ernest" & common_name == "", "Ernie", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Gilbert" & common_name == "", "Gil", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Doug" & common_name == "", "Douglas", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Douglas" & common_name == "", "Doug", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Thaddeus" & common_name == "", "Thad", common_name)) %>%
    mutate(common_name = ifelse( first_name == "Daniel" & common_name == "", "Dan", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Andrew" & common_name == "", "Andy", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Rick" & common_name == "", "Richard", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Gregory" & common_name == "", "Greg", common_name)) %>%
    mutate(common_name = ifelse(  (first_name == "Dan") & (common_name==""), "Daniel", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Michael"& common_name == "", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Joe"& common_name == "", "Joseph", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Joseph"& common_name == "", "Joe", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Mike"& common_name == "", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "David")&(common_name==""), "Dave", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Dave")&(common_name==""), "David", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Thomas")&(common_name==""), "Tom", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Tom")&(common_name==""), "Thomas", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kathleen")&(common_name==""), "Kathy", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kathy")&(common_name==""), "Kathleen", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Patrick")&(common_name==""), "Pat", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Pat")&(common_name==""), "Patrick", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "James")&(common_name==""), "Jim", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jim")&(common_name==""), "James", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Pete")&(common_name==""), "Peter", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Peter")&(common_name==""), "Pete", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Richard")&(common_name==""), "Rich", common_name)) %>% #FIXME these should be done one by one
    mutate(common_name = ifelse(  (first_name == "Chris")&(common_name==""), "Christopher", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Christopher")&(common_name==""), "Chris", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Robert")&(common_name==""), "Bob", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Bob")&(common_name==""), "Robert", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "William")&(common_name==""), "Bill", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Bill")&(common_name==""), "William", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Melvin")&(common_name==""), "Mel", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jeffrey")&(common_name==""), "Jeff", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jeff")&(common_name==""), "Jeffrey", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Ben")&(common_name==""), "Benjamin", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Benjamin")&(common_name==""), "Ben", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Charles")&(common_name==""), "Charlie", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Charlie")&(common_name==""), "Charles", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Chuck")&(common_name==""), "Charles", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kenneth")&(common_name==""), "Ken", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Ken")&(common_name==""), "Kenneth", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Rod")&(common_name==""), "Rodney", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Rodney")&(common_name==""), "Rod", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Mathew")&(common_name==""), "Matt", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Matthew")&(common_name==""), "Matt", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Matt")&(common_name==""), "Matthew", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Ed")&(common_name==""), "Edward", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Edward")&(common_name==""), "Ed", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Theodore")&(common_name==""), "Ted", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Stevan")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Stephen")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Steven")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Steve")&(common_name==""), "Steven", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Brad")&(common_name==""), "Bradley", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Bradley|Bradly")&(common_name==""), "Brad", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Nicholas")&(common_name==""), "Nick", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Russell")&(common_name==""), "Russ", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Philip")&(common_name==""), "Phil", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Dennis")&(common_name==""), "Denny", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Denny")&(common_name==""), "Dennis", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Tim")&(common_name==""), "Timothy", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Timothy")&(common_name==""), "Tim", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Al")&(common_name==""), "Alan", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Alfred")&(common_name==""), "Al", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Donald")&(common_name==""), "Don", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Don")&(common_name==""), "Donald", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Samuel")&(common_name==""), "Sam", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Joshua")&(common_name==""), "Josh", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Josh")&(common_name==""), "Joshua", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Liz")&(common_name==""), "Elizabeth", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Elizabeth")&(common_name==""), "Liz", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Johnny")&(common_name==""), "John", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Gerald")&(common_name==""), "Jerry", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Jerry")&(common_name==""), "Gerald", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Alex")&(common_name==""), "Alexander", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Ed")&(common_name==""), "Edwin", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Todd")&(common_name==""), "Theodore", common_name)) %>% 
     mutate(common_name = ifelse(  (first_name == "Jackie")&(common_name==""), "Jacqueline", common_name)) %>% 
     
     # hac 
     mutate(common_name = ifelse(bioname == "PAYNE, Donald Milford" & congress == 112, "Donald M", common_name)) %>%
     mutate(common_name = ifelse(bioname == "PAYNE, Donald, Jr." & congress == 112, "(Donald, Jr|Donald Jr)", common_name)) %>%
     
    # Specific common names 
     mutate(common_name = ifelse(bioname == "WHITEHOUSE, Sheldon", "Shelden", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "BOOZMAN, John", "Jon", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "SLAUGHTER, Louise McIntosh", "Louis", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "FISCHER, Debra (Deb)", "(Deb|Debbie)", common_name)) %>% 
     mutate(common_name = ifelse(bioname == "GRAVES, Garret", "Garrett", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "HARPER, Gregg", "Greg", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "HERRERA BEUTLER, Jaime", "Jamie", common_name)) %>% # FIXME this is a typo and should be corrected in typos, then removed here 
     mutate(common_name = ifelse(bioname == "MULVANEY, Mick", "John", common_name)) %>%
     mutate(common_name = ifelse(bioname == "PERLMUTTER, Ed", "Edwin", common_name)) %>%
     mutate(common_name = ifelse(bioname == "RUPPERSBERGER, C. A. Dutch", "Dutch", common_name)) %>%
     mutate(first_name = ifelse(bioname == "RUPPERSBERGER, C. A. Dutch", "Charles", first_name)) %>%
     mutate(common_name = ifelse(bioname == "SCALISE, Steve", "Stephen", common_name)) %>%
     mutate(common_name = ifelse(bioname == "SCOTT, Austin", "James", common_name)) %>%
     # mutate(common_name = ifelse(bioname == "NEUGEBAUER, Randy", "Robert", common_name)) %>% # another common name "Rand" added below 
     mutate(common_name = ifelse(bioname == "WILSON, Joe", "Addison", common_name)) %>%
     mutate(common_name = ifelse(bioname == "SEWELL, Terri", "Terrycina", common_name)) %>%
     mutate(common_name = ifelse(bioname == "GRAVES, Tom", "John", common_name)) %>%
     mutate(first_name = ifelse(bioname == "RIGELL, E. Scott", "Edward", first_name)) %>%
     mutate(common_name = ifelse(bioname == "KHANNA, Ro", "Rohit", common_name)) %>%
     
     mutate(common_name = ifelse(bioname == "GUTHRIE, Brett", "Steven", common_name)) %>%
     mutate(common_name = ifelse(bioname == "KELLY, Mike", "George", common_name)) %>%
     mutate(common_name = ifelse(bioname == "KHANNA, Ro", "Rohit", common_name)) %>%
     mutate(common_name = ifelse(bioname == "KNIGHT, Steve", "Stephen", common_name)) %>%
     mutate(common_name = ifelse(bioname == "MESSER, Luke", "Allen", common_name)) %>%
     mutate(common_name = ifelse(bioname == "GOWDY, Trey", "Harold", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GRUCCI, Jr., Felix J.", "Phil", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ARMEY, Richard Keith", "Dick", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MOAKLEY, John Joseph", "Joe", common_name)) %>%
    mutate(common_name = ifelse(bioname == "THURMOND, James Strom", "Strom", common_name)) %>%
    mutate(common_name = ifelse(bioname == "FROST, Jonas Martin", "Martin", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HURD, William Ballard", "Will", common_name)) %>%
    mutate(common_name = ifelse(bioname == "BUNNING, James Paul David", "Jim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "FORBES, J. Randy", "(J|Randy)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GRIFFITH, H. Morgan", "Morgan", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DURBIN, Richard Joseph", "Dick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BARLETTA, Lou", "Lou", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BUCHANAN, Vernon G.", "Vern", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SCHAKOWSKY, Janice D.", "Jan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KOHL, Herbert H.", "Herb", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "STEARNS, Clifford Bundy", "Cliff", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SNYDER, Victor F.", "Vic", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "EVERETT, Robert Terry", "Terry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WAMP, Zachary Paul", "Zach", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DEAL, John Nathan", "Nathan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COCHRAN, William Thad", "Thad", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GOODLATTE, Robert William", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "THOMPSON, Michael", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WILSON, Charlie", "Charles", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PENCE, Mike", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GAETZ, Matthew L. II", "Matt", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "STABENOW, Deborah Ann", "(Deb|Debbie)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "VAN HOLLEN, Christopher", "Chris", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ROSS, Michael Avery", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ASHFORD, John Bradley", "Brad", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MCEACHIN, Aston Donald", "Donald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WITTMAN, Robert J.", "Rob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ALLARD, A. Wayne", "Wayne", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GRASSLEY, Charles Ernest", "Chuck", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GOHMERT, Louie", "(Lou|Louis)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WALKER, Bradley Mark", "Mark", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DEMINGS, Valdez Butler", "Val", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SENSENBRENNER, Frank James, Jr.", "(James|Jim)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "AKIN, W. Todd", "Todd", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GRAVES, Samuel", "Sam", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "UPTON, Frederick Stephen", "Fred", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MURPHY, Timothy", "Tim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARNEY, Chris", "Christopher", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CAMP, David Lee", "Dave", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CRAPO, Michael Dean", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LANGEVIN, James", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SANDERS, Bernard", "Bernie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PAUL, Ronald Ernest", "Ron", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "YOUNG, Donald Edwin", "Don", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COBURN, Thomas Allen", "Tom", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BUYER, Stephen Earle", "Steve", common_name)) %>%  
    mutate(common_name = ifelse(bioname == "WYDEN, Ronald Lee", "Ron", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DELAHUNT, Bill", "William", common_name)) %>% 
    mutate(first_name = ifelse(bioname == "BOYD, F. Allen, Jr.", "A", first_name)) %>% 
     mutate(first_initial = ifelse(bioname == "BOYD, F. Allen, Jr.", "Allen", first_initial)) %>% 
     mutate(common_name = ifelse(bioname == "BOYD, F. Allen, Jr.", "F Allen", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOEKSTRA, Peter", "Pete", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "REICHERT, David G.", "Dave", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BOREN, Daniel David", "Dan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MACK, Connie, IV", "Connie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HALVORSON, Deborah L.", "(Deb|Debbie)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COHEN, Stephen", "Steve", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "INGLIS, Robert Durden", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ETHERIDGE, Bobby R.", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BOUCHER, Frederick C.", "Rick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "RYAN, Timothy J.", "Tim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KAGEN, Steven", "Steve", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BURTON, Danny Lee", "Dan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LATHAM, Thomas", "Tom", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COOPER, James Hayes Shofner", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GORDON, Barton Jennings", "Bart", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DICKS, Norman DeValois", "Norm", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HONDA, Mike", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "JOHNSON, Ron", "Ronald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MEEHAN, Patrick", "Pat", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SCHILLING, Bobby", "Robert", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "RIGELL, E. Scott", "Scott", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WILLIAMS, Roger", "John", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BERRY, Robert Marion", "Marion", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LABRADOR, Raúl R.", "Raul", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOLLINGSWORTH, Joseph Albert III", "Trey", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "NOLAN, Richard Michael", "Rick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SAXTON, Hugh James", "(James|Jim)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "McNERNEY, Jerry", "Gerald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KHANNA, Rohit", "Ro", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KRISHNAMOORTHI, S. Raja", "Raja", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "JOHNSON, Hank", "Henry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DUNCAN, John J., Jr.", "(Jimmy|Jim)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "NEUGEBAUER, Randy", "(Robert|Rand)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DENT, Charles W.", "Charlie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WOODALL, Rob", "Robert", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LOTT, Chester Trent", "Trent", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CONAWAY, K. Michael", "(Mike|Michael)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CRAWFORD, Rick", "Eric", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LONG, Billy", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "FERGUSON, Anderson Drew IV", "Drew", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
    
    mutate(common_name = ifelse(bioname == "ROSEN, Jacklyn", "Jacky", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BURR, Richard M.", "Rich", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HURT, Robert", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LOTT, Chester Trent", "Trent", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MURPHY, Timothy", "Tim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HOLDEN, Thomas Timothy (Tim)", "Tim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "TSONGAS, Nicola S. (Niki)", "Niki", common_name)) %>%
    mutate(common_name = ifelse(bioname == "KENNEDY, Edward Moore (Ted)", "Ted", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CUNNINGHAM, Randall (Duke)", "(Randy|Duke)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HOUGHTON, Amory, Jr.", "Amo", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GOODE, Virgil H., Jr", "Virgil", common_name)) %>%
    mutate(common_name = ifelse(bioname == "RUPPERSBERGER, C.", "Dutch", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MEEHAN, Martin Thomas", "Marty", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ROUKEMA, Margaret Scafati", "Marge", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KLECZKA, Gerald Daniel", "Jerry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BERGMAN, John", "Jack", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOLLINGS, Ernest Frederick", "Fritz", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "THOMPSON, Glenn", "G.T.", common_name)) %>% 
    
    mutate(common_name = ifelse(bioname == "COX, Charles Christopher", "Christopher", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GRAHAM, Daniel Robert (Bob)", "Bob", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SHAW, Eugene Clay, Jr.", "Clay", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ANDREWS, Robert Ernest", "(Bob|Rob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "LUGAR, Richard Green", "(Dick|Rich)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SCOTT, Robert C.", "(Bob|Bobby)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "VAN DREW, Jefferson", "Jeff", common_name)) %>%
    mutate(common_name = ifelse(bioname == "OWENS, William", "(Will|Bill)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "NELSON, Earl Benjamin (Ben)", "(Benjamin|Ben|E.B.)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "(Rob|Bob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "WHITFIELD, Wayne Edward (Ed)", "(Ed|Edward)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CLELAND, Joseph Maxwell (Max)", "(Max|Joe)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "EHLERS, Vernon James", "Vern", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MARKEY, Edward John", "(Ed|EJ)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ROSEN, Jacklyn Sheryl", "Jacky", common_name)) %>%
    mutate(common_name = ifelse(bioname == "KUSTER, Ann McLane", "Annie", common_name)) %>%
    mutate(common_name = ifelse(bioname == "BISHOP, Robert (Rob)", "(Rob|Bob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DOOLEY, Calvin M.", "Cal", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SHOWS, Clifford Ronald", "Ronnie", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PACKARD, Ronald C", "Ron", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MURRAY, Patty", "(Patricia|Pat)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SCARBOROUGH, Charles Joseph", "(Charlie|Joe)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SPACE, Zack", "Zachary", common_name)) %>%
    mutate(common_name = ifelse(bioname == "RICE, Tom", "Hugh", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SMITH, Tina", "Christine", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ROY, Charles", "Chip", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PORTMAN, Robert Jones (Rob)", "(Bob|Rob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PANETTA, James Varni", "(Jimmy|Jim)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CISNEROS, Gil", "Gilbert", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CLAY, William Lacy, Jr.", "(Bill|Lacy)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "AXNE, Cynthia", "Cindy", common_name)) %>%
    mutate(common_name = ifelse(bioname == "STEUBE, William", "(Bill|Greg|Gregory)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CORREA, Jose Luis", "(Lou|Luis)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HUDSON, Richard", "(Rich|Rick)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HAWLEY, Joshua David", "Josh", common_name)) %>%
    mutate(common_name = ifelse(bioname == "TOWNS, Edolphus", "Ed", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HILL, French", "(J. French|James)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "WOMACK, Steve", "Stephen", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HASTERT, John Dennis", "Dennis", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ROUNDS, Marion Michael (Mike)", "(Mike|Michael)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DINGELL, Debbie", "Deborah", common_name)) %>%
    # mutate(common_name = ifelse(bioname == "JOHNSON, Eddie Bernice", "Bernice", common_name)) %>% # CAUSES ERRORS WITH Bill Johnson as B Johnson
    mutate(common_name = ifelse(bioname == "TAYLOR, Nicholas", "Van", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MALONEY, Sean Patrick", "Patrick", common_name)) %>%
    mutate(common_name = ifelse(bioname == "JOHNSON, Dustin", "Dusty", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GARRETT, Scott", "Ernest", common_name)) %>%
    mutate(common_name = ifelse(bioname == "WARREN, Elizabeth", "Liz", common_name)) %>%   
    mutate(common_name = ifelse(bioname == "PAUL, Rand", "(Randy|Randal)", common_name)) %>%
     mutate(common_name = ifelse(bioname == "EMERSON, Jo Ann", "JoAnn", common_name)) %>%
     mutate(common_name = ifelse(bioname == "DEMINGS, Valdez", "Val", common_name)) %>%
     mutate(common_name = ifelse(bioname == "BERA, Ami", "Amerish", common_name)) %>%
     mutate(common_name = ifelse(bioname == "BROOKS, Mo", "Morris", common_name)) %>%
     mutate(common_name = ifelse(bioname == "CHENEY, Liz", "Elizabeth", common_name)) %>%
     mutate(common_name = ifelse(bioname == "FARENTHOLD, Blake", "Randolph", common_name)) %>%
     mutate(common_name = ifelse(bioname == "GIBBS, Bob", "Robert", common_name)) %>%
     mutate(common_name = ifelse(bioname == "O'ROURKE, Beto", "Robert", common_name)) %>%
     # Tim usually refers to the other Timothy Johnson
    mutate(common_name = ifelse(bioname == "JOHNSON, Timothy V.", "Tim V", common_name)) %>% 

    # remove accent marks (using RegEx dot for specials, not exact matching)
    mutate(common_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", common_name)) %>% 
    mutate(last_name = ifelse(grepl("VEL.ZQUEZ, Nydia M.", bioname), "VELAZQUEZ", last_name)) %>% 
    mutate(last_name = ifelse(grepl("C.RDENAS, Tony", bioname), "CARDENAS", last_name)) %>% 
    mutate(last_name = ifelse(grepl("GUTI.RREZ, Luis V.", bioname), "GUTIERREZ", last_name)) %>% 
    mutate(first_name = ifelse(grepl("SERRANO, Jos. E.", bioname), "Jose", first_name)) %>% 
    mutate(first_name = ifelse(grepl("CARSON, Andr.", bioname), "Andre", first_name)) %>% 
    mutate(last_name = ifelse(grepl("LUJ.N, Ben Ray", bioname), "LUJAN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("LUJ.N GRISHAM, Michelle", bioname), "LUJAN GRISHAM", last_name)) %>% 
    mutate(last_name = ifelse(grepl("BARRAG.N, Nanette Diaz", bioname), "BARRAGAN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("S.NCHEZ, Linda T.", bioname), "SANCHEZ", last_name)) %>% 
    mutate(first_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", first_name)) %>% 
    mutate(first_name = ifelse(grepl("HINOJOSA, Rub.n", bioname), "Ruben", first_name)) %>% 
    mutate(first_name = ifelse(grepl("LABRADOR, Ra.l R.", bioname), "Raul", first_name)) %>% 
  
  # middle name
     mutate(middle_name = ifelse(bioname == "SUOZZI, Thomas", "Richard", middle_name)) %>% 
      mutate(middle_name = ifelse(bioname == "DENT, Charles W.", "Weider", middle_name)) %>% 
     mutate(middle_name = ifelse(bioname == "KATKO, John", "Michael", middle_name)) %>% 
     mutate(middle_name = ifelse(bioname == "McSALLY, Martha", "Elizabeth", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "GRUCCI, Jr., Felix J.", "James", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "ZINKE, Ryan", "Keith", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "AUSTRIA, Steve", "Clement", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "FORD, Harold, Jr.", "Eugene", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "SUTTON, Betty", "Sue", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "PLATTS, Todd", "Russell", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CARTER, Buddy", "Leroy", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "GRIFFITH, H. Morgan", "Morgan", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "HARRIS, Kamala Devi", "Devi", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CLARKE, Yvette", "Diane", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CRAIG, Larry Edwin", "Edwin", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "GOODLATTE, Robert William", "William", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "WOLF, Frank Rudolph", "Rudolph", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "LUGAR, Richard Green", "Green", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "COLLINS, Susan Margaret", "Margaret", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "FEINGOLD, Russell Dana", "Dana", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "WARNER, John William", "William", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "GRASSLEY, Charles Ernest", "Ernest", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "MIKULSKI, Barbara Ann", "Ann", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "LYNCH, Stephen F.", "Francis", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "McCLINTOCK, Tom", "Miller", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "PELOSI, Nancy", "Patricia", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HARMAN, Jane L.", "Margaret Lakes", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "BONNER, Jr., Josiah Robins (Jo).", "Robins", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ESHOO, Anna Georges", "Georges", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "FRANKEN, Al", "Stuart", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "SCHIFF, Adam", "Bennett", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "CAPITO, Shelley Moore", "Wellons", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "Ann", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "THOMPSON, Glenn", "William", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "PRICE, Tom", "Edmunds", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "RICE, Tom", "Thompson", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "SMITH, Tina", "Elizabeth", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CULBERSON, John", "Abney", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "SCOTT, David", "Albert", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CRAMER, Kevin", "John", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "KLINE, John", "Paul", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "DUFFY, Sean", "Patrick", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ABRAHAM, Ralph", "Lee", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ROSS, Dennis", "Alan", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "WILSON, Frederica", "Smith", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HANABUSA, Colleen", "Wakako", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "RIVERA, David", "Mauricio", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "KAGEN, Steven", "Leslie", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "DRIEHAUS, Steve", "Leo", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "BURRIS, Roland", "Wallace", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HOLT, Rush", "Dew", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HINOJOSA, Rubén", "Eloy", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "PETERS, Scott", "Harvey", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "DENHAM, Jeff", "John", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ELLISON, Keith", "Maurice", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ERNST, Joni", "Kay", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "DELANEY, John", "Kevin", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "BARLETTA, Lou", "James", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HIRONO, Mazie", "Keiko", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "MURPHY, Patrick" & state == "florida", "Erin", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "GOHMERT, Louie", "Buller", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "YARMUTH, John", "Allen", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "LUMMIS, Cynthia M.", "Marie", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "FRANKEL, Lois", "Jane", middle_name)) %>%
   mutate(middle_name = ifelse(bioname == "WAGNER, Ann", "Louise", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "MEADOWS, Mark", "Randall", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "PAYNE, Donald, Jr.", "Milford", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "RATCLIFFE, John", "Lee", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "JOHANNS, Mike", "Owen", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "MINNICK, Walt", "Clifford", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "LAMBORN, Doug", "Lawrence", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "ENZI, Rick", "George", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "TAYLOR, Nicholas", "Van Campen", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "HORSFORD, Steven", "Alexander", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "MURPHY, Patrick" & state == "pennsylvania", "Joseph", middle_name)) %>%
     mutate(middle_name = ifelse(bioname == "MURRAY, Patty" & state == "washington", "Lynn", middle_name))%>%
     mutate(middle_name = ifelse(bioname == "FEINSTEIN, Dianne" & state == "california", "Emiel", middle_name))%>%
     mutate(middle_name = ifelse(bioname == "FISCHER, Debra (Deb)" & state == "nebraska", "Lynelle", middle_name))%>%     
     mutate(middle_name = ifelse(bioname == "WARREN, Elizabeth" & state == "massachusetts", "Ann", middle_name))%>%       
     mutate(middle_name = ifelse(bioname == "JOHNSON, Ron" & state == "wisconsin", "Harold", middle_name))%>%     
     mutate(middle_name = ifelse(bioname == "LEE, Mike" & state == "utah", "Shumway", middle_name))%>% 
     mutate(middle_name = ifelse(bioname == "HOEVEN, John" & state == "north dakota", "Henry", middle_name))%>% 
     mutate(middle_name = ifelse(bioname == "AYOTTE, Kelly" & state == "new hampshire", "Ann", middle_name))%>%      
     mutate(middle_name = ifelse(bioname == "PAUL, Rand" & state == "kentucky", "Howard", middle_name))%>%      
     mutate(middle_name = ifelse(bioname == "COHEN, Stephen", "Ira", middle_name)) %>% 
     mutate(middle_name = ifelse(bioname == "SCHNEIDER, Brad", "Scott", middle_name)) %>% 
     mutate(middle_name = ifelse(bioname == "COURTNEY, Joe", "David", middle_name)) %>% 
     
     #mutate(middle_name = ifelse(bioname == "TIERNEY, John F.", "David", middle_name)) %>% 
     
    
   
    # middle initials
     mutate(middle_initial = ifelse(is.na(middle_initial), str_sub(middle_name, 1,1), middle_initial)) %>% 
     mutate(middle_initial = ifelse(bioname == "AMASH, Justin", "A.", middle_initial)) %>% 
     mutate(middle_initial = ifelse(bioname == "COHEN, Stephen", "I.", middle_initial)) %>% 
   mutate(middle_initial = ifelse(bioname == "BRAUN, Michael", "K.", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SPACE, Zack", "T", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "GROTHMAN, Glenn", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HIGGINS, Brian", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "FRANKEN, Al", "S", middle_initial)) %>%
    mutate(middle_initial = ifelse(bioname == "ESHOO, Anna", "G", middle_initial)) %>%
    mutate(middle_initial = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BOND, Christopher Samuel (Kit)", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PENCE, Mike", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "LANGEVIN, James", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WEST, Allen", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RISCH, James", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HIRONO, Mazie", "K", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "DJOU, Charles", "K", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "GILLIBRAND, Kirsten", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "KUCINICH, Dennis", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SMITH, Adrian", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "THUNE, John", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WALZ, Tim", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SARBANES, John", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SALAZAR, John", "T", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "UDALL, Mark", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "THOMPSON, Bennie", "G", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BENNETT, Robert", "F", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PLATTS, Todd", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "DELAHUNT, Bill", "D", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RICHARDSON, Laura", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "TIBERI, Patrick (Pat)", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BRALEY, Bruce", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HAGAN, Kay", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WARNER, Mark", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BEYER, Donald Sternoff Jr.", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RICHMOND, Cedric", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ADERHOLT, Robert", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "McKINLEY, David", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SEWELL, Terri", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HANNA, Richard", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "FLEISCHMANN, Chuck", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "MURPHY, Christopher", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SCHIFF, Adam", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HONDA, Mike", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "CARTWRIGHT, Matt", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "LOWENTHAL, Alan", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ADAMS, Alma", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOYCE, David", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WENSTRUP, Brad", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RENACCI, Jim", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "TONKO, Paul", "D", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOHNSON, Hank", "C", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "VISCLOSKY, Peter", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "OBAMA, Barack", "H", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "OWENS, William", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WILSON, Heather", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BILIRAKIS, Gus", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ISSA, Darrell", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PUTNAM, Adam", "H", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PALAZZO, Steven", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOHNSON, Ron", "H", middle_initial)) %>% 
  
  mutate(middle_initial = ifelse(bioname == "CRAWFORD, Rick", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "AYOTTE, Kelly", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BOXER, Barbara", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "MATSUI, Doris", "O", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "MERKLEY, Jeff", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "ROTHMAN, Steven", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "REHBERG, Denny", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "YOHO, Ted", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "LIPINSKI, Daniel", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GOSAR, Paul", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROSEN, Jacklyn Shery", "S", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SNOWE, Olympia Jean", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WEBB, James H. (Jim)", "H.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SCOTT, Robert C.", "C.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROGERS, Mike", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "KING, Peter T.", "T.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CRAIG, Larry Edwin", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GOODE, Virgil H., Jr", "H.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SHELBY, Richard C.", "C.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "FEINGOLD, Russell Dana", "D", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "LANDRIEU, Mary L.", "L.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RUPPERSBERGER, C. A. (Dutch)", "A", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "MIKULSKI, Barbara Ann", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GRASSLEY, Charles Ernest", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WARNER, John William", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "COLLINS, Susan Margaret", "M", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "LUGAR, Richard Green", "G", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "WOLF, Frank Rudolph", "R", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARRIS, Kamala Devi", "D", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CLARKE, Yvette", "D", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "BURR, Richard", "M", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "ESTY, Elizabeth", "H", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "MURPHY, Timothy", "F", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "WILSON, Charlie", "A", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "ROTHFUS, Keith", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "KING, Steve", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RIBBLE, Reid", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WEBER, Randy", "K", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MEEHAN, Patrick", "L", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HODES, Paul", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "LEE, Mike", "S", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "FERGUSON, Michael", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MOOLENAAR, John", "R", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "PETERSON, John", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MERKLEY, Jeff", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARMAN, Jane L.", "(L|ML)", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "REID, Harry","M", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CARNEY, Chris","P", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "KILDEE, Dan","T", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GRUCCI, Jr., Felix J.","J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WARREN, Elizabeth","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HASSAN, Margaret (Maggie)","C", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ARCURI, Michael","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HALL, John","J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "TIPTON, Scott","R", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "KUSTER, Ann McLane","L", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "COVERDELL, Paul","D", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CAPITO, Shelley Moore","W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "PRYOR, Mark","L", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RICE, Tom","T", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "PRICE, Tom","E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROSKAM, Peter","J", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "NUGENT, Richard","B", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WOMACK, Steve","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARRIS, Andy","P", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CRAMER, Kevin","J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "BOYDA, Nancy","E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "LIEU, Ted", "W", middle_initial))%>%
  mutate(middle_initial = ifelse(bioname == "MURRAY, Patty", "L", middle_initial))%>%
  mutate(middle_initial = ifelse(bioname == "FEINSTEIN, Dianne", "E", middle_initial))%>%
  mutate(middle_initial = ifelse(bioname == "FISCHER, Debra (Deb)", "L", middle_initial))%>%
  mutate(middle_initial = ifelse(bioname == "WARREN, Elizabeth", "A", middle_initial))%>%   
  mutate(middle_initial = ifelse(bioname == "HOEVEN, John", "H", middle_initial))%>% 
  mutate(middle_initial = ifelse(bioname == "PAUL, Rand", "H", middle_initial)) 
   
  members %<>%    
  # first names
    mutate(first_name = ifelse(bioname == "GRUCCI, Jr., Felix J.", "Felix", first_name)) %>% 
    mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>%
    mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%
    mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name)) %>% 
    mutate(first_name = ifelse(bioname == "CONAWAY, K. Michael", "Kenneth", first_name)) %>%
    mutate(first_name = ifelse(bioname == "BONNER, Jr., Josiah Robins (Jo)", "Josiah", first_name)) %>%
    mutate(first_name = ifelse(bioname == "GRUCCI, Jr., Felix J.)", "Felix", first_name)) %>% 
    # first names that reduce false matches in specific congresses 
    mutate(first_name = ifelse(bioname == "JOHNSON, Timothy Peter (Tim)" & congress %in% 107:112, "(Tim|Timothy Peter)", first_name)) 
    
  
  # make blank common names NA
  #members %<>%
    #mutate(common_name = ifelse(common_name=="", NA,  common_name))
  
  # # Creates new rows in member dataset. These are not actual members, but common names that we know shouldn't be matching
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "JEWELL"; members$first_name[nrow(members)] <- "Sally"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "NORTON"; members$first_name[nrow(members)] <- "Eleanor"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "SABLAN"; members$first_name[nrow(members)] <- "Gregorio"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PLASKETT"; members$first_name[nrow(members)] <- "Stacey"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "RADEWAGEN"; members$first_name[nrow(members)] <- "Amata"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "CHRISTENSEN"; members$first_name[nrow(members)] <- "Donna";members$middle_initial[nrow(members)] <- "M"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PIERLUISI"; members$first_name[nrow(members)] <- "Pedro"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "WHITEHOUSE";
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "BORDALLO"; members$first_name[nrow(members)] <- "Madeleine"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "FALEOMAVAEGA"; members$first_name[nrow(members)] <- "Eni"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "JOHNSON"; members$first_name[nrow(members)] <- "Tia"; members$common_name[nrow(members)] <- "M. Tia"
  # 
  
  # select
  members %<>% 
    select(first_name, first_initial ,common_name, middle_name, middle_initial, maiden_name, last_name, add_last_name, bioname, everything()) 
  
  # NOTE: 
  # Voteview is missing non-voting members:
  # American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
  # District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
  # Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
  # Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
  # Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
  # U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014
  
  # Replace blanks with NA 
  members %<>% 
    mutate(middle_initial = ifelse(middle_initial == "", NA, middle_initial)) %>% 
    mutate(middle_name = ifelse(middle_name == "", NA, middle_name)) %>% 
    mutate(common_name = ifelse(common_name=="", NA,  common_name))
  # seo middle names don't get us anything, and this just ends up filling in middle names with last names 
    #mutate(middle_name = ifelse(is.na(middle_initial), str_remove_all(seo_name, ".*?-|-.*"), middle_name))
  
  # Periods
  #TODO document that text clearning must remove punctuation 
  members$middle_initial %<>% str_remove("\\.")
  members$first_initial %<>% str_remove("\\.")
  members$middle_name %<>% str_remove("\\.")
  members$first_name %<>% str_remove("\\.")
  
  members %<>%
    # add any missing middle initials
    mutate(middle_initial = ifelse(is.na(middle_initial) & !is.na(middle_name),
                                   str_sub(middle_name, 1,1),
                                   middle_initial)) %>%   
    # add any missing first initials
    mutate(first_initial = ifelse(is.na(first_initial),
                                   str_sub(first_name, 1,1),
                                   first_initial))
  
  replace404 <- function(x){
    ifelse(is.na(x), "404error", x)
  }
  
  members %<>% mutate_all(replace404)
  
  #create full name variables with different combinations of first, common, middle, middle initial, and last name
  members$first_last <- paste(members$first_name, members$last_name, sep = " ")
  members$first_maiden <- paste(members$first_name, members$maiden_name, sep = " ")
  members$common_last <- paste(members$common_name, members$last_name, sep = " ")
  members$first_middle_last <- paste(members$first_name, members$middle_name, members$last_name, sep = " ")
  members$first_initial_last <- paste(members$first_name, members$middle_initial, members$last_name, sep = " ")
  members$common_middle_last <- paste(members$common_name, members$middle_name, members$last_name, sep = " ")
  #members$first_addlast <- paste(members$first_name, members$add_last_name, sep = " ")
  #members$first_last_addlast <- paste(members$first_name, members$last_name, members$add_last_name, sep = " ")
  #members$firstinitial_middleinitial_last <- paste(members$first_initial, members$middle_initial, members$last_name, sep = " ")
 
  
  members %<>% 
    ungroup() %>% 
    mutate(last = paste0("^", last_name, "$"), # ADDING $ TO PREVENT OVER MATCHING WITH PEOPLE WITH THE SAME LAST NAME, but may reduce matches
           last_state = paste0("\\b", last_name, "( .| )", state_abbrev, "\\b"),
           last_comma_first = paste0("\\b",last_name, ", ", first_name, "\\b"),
           last_first = paste0("\\b", last_name, " ", first_name, "\\b"),
           first_maiden_last = paste(first_name, maiden_name),
           common_middle_initial_last = paste(common_name, middle_initial, last_name),
           common_maiden = paste(common_name, maiden_name),
           first_initial_last = paste0("\\b", first_initial, " ", last_name, "\\b"),
           commoninitial_last = paste0("\\b", common_name %>% str_extract("[A-Z]") %>% str_sub(1, 1), " ", last_name),
           first_middle_initial_last = paste0("\\b", first_name, " ", middle_initial, " ", last_name),
           firstinitial_middleinitial_last = paste0("\\b", first_initial, " ", middle_initial, " ", last_name),
           #last_comma_firstinitial_middleinitial = paste0(last_name, ", ", first_initial, " ", middle_initial), # redundent
           last_comma_initial = paste0("\\b", last_name, ", ", first_initial, "\\b"),
           last_comma_commoninitial = paste0("\\b", last_name, ", ", common_name %>% str_extract("[A-Z]") %>% str_sub(1, 1), "\\b"),
           last_comma_common = paste0("\\b", last_name, ", ", common_name, "\\b"),
           maiden_comma_first = paste0("\\b", maiden_name, ", ", first_name),  # e.g. Mack, Mary
           maiden_comma_firstinitial = paste0("\\b", maiden_name, ", ", first_initial, "\\b"), # e.g. Grisham, M. in VA 
           #last_comma_first_maiden = paste0(last_name, ", ", first_name, " ",maiden_name), # redundent
           #last_addlast_comma_first = paste0(last_name, " ", add_last_name, ", ", first_name),
           #addlast_comma_first = paste0(add_last_name, ", ", first_name),
           title = chamber  %>% 
             str_replace("Senate", "Senator") %>% 
             str_replace("House", "Representative") , 
           chamber_last = paste0(title, " ", last_name, "\\b"),
           chamber_maiden = paste0(title, " ", maiden_name, "\\b"),
           # FIXME, this is a good idea and would get a few correct matches, but name first letter matches too many first letters of states 
           # chamber_last_firstinitial = paste0(chamber, " ", last_name, ".{1,4}", first_initial) %>% 
           #   str_replace("Senate", "Senator") %>% 
           #   str_replace("House", "Representative"),
           chamber_last_state = paste0(title, " ", last_name, ".{1,4}", state_abbrev)
           )

  # check for mirror names 
  members %>% filter(last_first %in% members$first_last)
  
  # duplicates created by using maiden names 
  members %>% filter(maiden_comma_first %in% members$last_comma_first)
  
  # last names that are also first names
  problem_last_names <- members %>% 
    filter(congress > 105) %>%
    filter(last_name %in% str_to_upper(first_name) ) %>% 
    distinct(last_name)
  
  problem_last_names
  
  members %>% 
    filter(congress > 105) %>%
    filter(first_name %in% {problem_last_names$last_name |>
             str_to_title() }
           ) %>% 
    distinct(bioname)
  
  # non-unique last names in a chamber and congress 
  # require sate abrev to appear within the next 4 characters (e.g. senator warner, va) --- however senator warner is unique within all congresses, except the 48th and 49th, so ", va" is not needed to identify the correct senator warner 
  last_name_count <- members %>% 
    select(last_name, chamber, bioname, congress) %>%  
    distinct() %>%     
    count(last_name, chamber, congress, name = "last_name_count") 
  
  # last_name_count %>% filter(last_name_count >1) %>% ungroup() %>% 
  #   group_by(last_name, last_name_count) %>% 
  #   summarise(congress = str_c(congress, collapse = ";"),
  #             chamber = str_c(unique(chamber), collapse = ";")) %>%
  #   kable()

  # FIXME, for people with first names, this should be a negation of
  # FIXME really this should all be replaced by losts of patterns that are then checked for uniqueness 

  # if same last name in chamber OR last name is first name in the chamber, require state abrev within 4 chr
  members %<>% 
    full_join(last_name_count) %>% # add counts 
    ungroup() %>% 
    group_by(congress) %>% 
    # if last names are not unique, require state info
    mutate(chamber_last = ifelse(last_name_count > 1,  
                                 # if chamber last is not unique, require state or first initial 
                                 paste0(chamber_last, ".{1,4}", state_abbrev), # e.g. "representative king (ny-2)" 
                                       chamber_last)) 
    
    members %<>% 
    ungroup() %>% 
    group_by(congress, chamber) %>% 
    # if not same last name in chamber OR last name is first name in the chamber, require state abrev within 4 chr
    mutate(first_names_in_chamber = paste(first_name, collapse = " ") |> str_to_upper()  ) %>% 
    # if last names are unique AND last names are a first name, require state info
    mutate(chamber_last = ifelse(last_name_count == 1 & str_detect(first_names_in_chamber, paste0("\\b", last_name, "\\b")),  
                                 # if chamber last is not unique, require state or first initial 
                                 paste0(chamber_last, ".{1,4}", state_abbrev), # e.g. "representative king (ny-2)" 
                                 # # or first initial #FIXME THIS CREATES TOO MANY DUPLICATE, NEEDS TO BE ADDED IN A NON-REGEX WAY SO IT CAN BE DEDUPED, commenting out for now
                                 # paste0("(", chamber_last, ".{1,4}", state_abbrev, "|", ", ", first_initial, "$)"),
                                 chamber_last)) 
  
  if(F){
    tail(members$first_names_in_chamber)
    
    # confirm mark warner does not have state 
  members |> filter(last_name == "WARNER") |> 
    mutate(test = str_detect( first_names_in_chamber, "WARNER")) |> 
    select(chamber_last, last_name_count, test) |> 
    kable()
  
  members |> filter(last_name == "ALLEN") |> 
    mutate(test = str_detect( first_names_in_chamber, "ALLEN")) |> 
    select(chamber_last, last_name_count, test) |> 
    kable()
  
  #FIXME why is the 107-11th getting triggerd here 
  members |> filter(last_name == "ISSA") |> 
    #distinct(first_names_in_chamber) |> mutate(test = str_extract_all( first_names_in_chamber, "\\bISSA\\b")) |> select(test)
    mutate(test = str_detect(first_names_in_chamber, paste0("\\b", last_name, "\\b"))) |> 
    select(chamber_last, last_name_count, test) |> 
    kable()
  
  # confirm barney frank in the 110th does 
  members |> filter(last_name == "FRANK") |> 
    mutate(test = str_detect( first_names_in_chamber, "FRANK")) |> 
    select(chamber_last, last_name_count, test) |> 
    kable()
  
  }
    
  # non-unique last names in a congress 
  last_name_count <- members %>% 
    select(last_name, bioname, congress) %>%  
    distinct() %>%     
    count(last_name, congress, name = "last_name_count") 
  
  members %<>% 
    select(-last_name_count, -first_names_in_chamber) %>%  # drop counts
    full_join(last_name_count) %>% # add counts 
    # if last name is not unique 
    mutate(last = ifelse(last_name_count > 1,
                         # if last is not unique, require state 
                         str_c(last, ".{1,4}", state_abbrev) |> str_remove("$"), # e.g. "representative king (ny-2)" 
                         last)) %>% # remove non unique 
    select(-last_name_count) # drop counts
  

  # non-unique first initials + last names
  last_name_count <- members %>% 
    select(first_initial_last, bioname, congress) %>%  
    distinct() %>%     
    count(first_initial_last, congress, name = "last_name_count") 
  
  members %<>% 
    full_join(last_name_count) %>% # add counts 
    mutate(first_initial_last = ifelse(last_name_count > 1, 
                                       # if chamber last is not unique, require state 
                                       str_c(first_initial_last, ".{1,4}", state_abbrev), # e.g. "representative king (ny-2)" 
                                       first_initial_last)) %>% # remove non unique 
    mutate(last_comma_initial = ifelse(last_name_count > 1, 
                                       "404error",
                                       last_comma_initial)) %>% # remove non unique 
    select(-last_name_count) # drop counts
  
  # non-unique common initials + last names
  last_name_count <- members %>% 
    select(commoninitial_last, bioname, congress) %>%  
    distinct() %>%     
    count(commoninitial_last, congress, name = "last_name_count") 
  
  members %<>% 
    full_join(last_name_count) %>% # add counts 
    mutate(commoninitial_last = ifelse(last_name_count > 1, "404error", commoninitial_last)) %>% # remove non unique 
    mutate(last_comma_commoninitial = ifelse(last_name_count > 1, "404error", last_comma_commoninitial)) %>% # remove non unique 
    select(-last_name_count) # drop counts
  
  
  members %<>%    
    # last names + state abbrevs that match other names 
    mutate(chamber_last = ifelse(bioname == "GRAVES, Tom" & congress %in% 114:116, "Representative GRAVES \\(GA", chamber_last)) # not graves, GArret 
  
  
  
  # # drop common last when there are multiple members with the same name
  # # commented out because I am option to over match and then drop people in merge, then fail to match people with the same name 
  # last_name_count <- members %>% 
  #   select(last_name, common_name, bioname) %>%  
  #   distinct() %>%     
  #   count(last_name, common_name) %>% 
  #   rename(last_name_count = n)
  # 
  # members %<>% 
  #   full_join(last_name_count) %>% 
  #   mutate(common_last = ifelse(last_name_count > 1, NA, common_last))%>% 
  #   select(-last_name_count)
  
  replace404 <- function(x){
    ifelse(is.na(x), "404error", x)
  }
  
  members %<>% mutate_all(replace404)
  
  
replace404 <- function(x){
  ifelse(str_detect(x, "\\bNA\\b|\\bNA,|\\\\bNA,|\\\\bNA |404error"), "404error", x)
}

members %<>% mutate_all(replace404)

members %<>% mutate(across(everything(), replace404 ))

head(members$commoninitial_last, 100)


members <- members |> 
  # special names 
  mutate(last = ifelse(bioname == "OCASIO-CORTEZ, Alexandria", paste0(last, "|\\bAOC\\b"), last)) |> 
  mutate(last = ifelse(bioname == "SESSIONS, Pete", "404error", last)) |> # not Jeff
  mutate(last_state = ifelse(bioname == "JOHNSON, Eddie Bernice" & congress < 116, "404error", last_state)) |> # not
  mutate(last_state = ifelse(bioname == "JOHNSON, Sam" & congress < 116, "404error", last_state)) |> # not
  # ambiguous 
  # FIXME instead of 404, many of these could just require state or something 
  mutate(first_initial_last = ifelse(bioname == "KING, Steve" & congress %in% 113:116, "^S King\\b", first_initial_last)) |> # not KING, Angus Stanley, Jr. 
  mutate(commoninitial_last = ifelse(bioname == "KING, Steve" & congress %in% 113:116, "^S King\\b", commoninitial_last)) |> # not KING, Angus Stanley, Jr. 
  mutate(commoninitial_last = ifelse(bioname == "WILSON, Joe" & congress %in% 105:110, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "KENNEDY, Mark" & congress %in% 107:109, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "KENNEDY, Mark" & congress %in% 107:109, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "THOMPSON, Glenn" & congress %in% 111:119, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "THOMPSON, Glenn" & congress %in% 111:119, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "Michael Allen (Mac)" & congress %in% 104:108, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "Michael Allen (Mac)" & congress %in% 104:108, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "AVIS, Artur" & congress %in% 108:111, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "AVIS, Artur" & congress %in% 108:111, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "MURPHY, Scott" & congress %in% 111, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "MURPHY, Scott" & congress %in% 111, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "TURNER, Robert L." & congress %in% 112, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "TURNER, Robert L." & congress %in% 112, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "JONES, Brenda" & congress %in% 115, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "JONES, Brenda" & congress %in% 115, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "JONES, Ben" & congress %in% 101:102, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "JONES, Ben" & congress %in% 101:102, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "JOHNSON, Hank" & congress %in% 111:119, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "JOHNSON, Hank" & congress %in% 111:119, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "JOHNSON, Bill" & congress %in% 112:117, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "JOHNSON, Bill" & congress %in% 112:117, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "SCOTT, Austin" & congress %in% 112:119, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "SCOTT, Austin" & congress %in% 112:119, "404error", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "DAVIS, Artur" & congress %in% 108:111, "404error", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "DAVIS, Artur" & congress %in% 108:111, "404error", commoninitial_last)) |> 
  mutate(last_comma_initial = ifelse(bioname == "LEE, Susie", "404error", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "LEE, Susie", "404error", last_comma_commoninitial)) |> 
  mutate(last_comma_initial = ifelse(bioname == "LEE, Summer", "404error", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "LEE, Summer", "404error", last_comma_commoninitial)) |> 
  mutate(first_initial_last = ifelse(bioname == "COLLINS, Michael Allen (Mac)", "^M COLLINS\\b", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "COLLINS, Michael Allen (Mac)", "^M A COLLINS\\b", commoninitial_last)) |> 
  mutate(first_initial_last = ifelse(bioname == "SMITH, Tina", "Senator Smith, T\\b", first_initial_last)) |> 
  mutate(commoninitial_last = ifelse(bioname == "SMITH, Tina", "404error", commoninitial_last)) |> # christina smith --> smith, c is ambiguous. # FIXME this seems to have not worked 
  mutate(last_comma_commoninitial = ifelse(bioname == "SMITH, Tina", "Senator Smith, C\\b", last_comma_commoninitial)) |> # christina smith --> smith, c is ambiguous. # FIXME this seems to have not worked 
  mutate(last_comma_initial = ifelse(bioname == "SMITH, Christopher Henry" & congress > 114, "Representative Smith, C\\b", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "SMITH, Christopher Henry" & congress > 114, "Representative Smith, C H", last_comma_commoninitial)) |> 
  # last first initial that is ambigous 
  mutate(last_comma_initial = ifelse(bioname == "LEWIS, John R." & congress %in% 100:115, "LEWIS, J R", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "LEWIS, John R." & congress %in% 100:115, "404error", last_comma_commoninitial)) |> 
  mutate(last_comma_initial = ifelse(bioname == "LEWIS, Charles Jeremy (Jerry)" & congress %in% 100:115, "LEWIS, C J", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "LEWIS, Charles Jeremy (Jerry)" & congress %in% 100:115, "404error", last_comma_commoninitial)) |> 
  mutate(last_comma_initial = ifelse(bioname == "LEWIS, Jason Mark" & congress %in% 100:115, "LEWIS, J M", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "LEWIS, Jason Mark" & congress %in% 100:115, "404error", last_comma_commoninitial)) |> 
  mutate(last_comma_initial = ifelse(bioname == "KENNEDY, Joseph P. III" & congress %in% 115:116, "KENNEDY, J P", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "KENNEDY, Joseph P. III" & congress %in% 115:116, "404error", last_comma_commoninitial)) |>   
  mutate(last_comma_initial = ifelse(bioname == "KENNEDY, John Neely" & congress %in% 115:116, "KENNEDY, J N", last_comma_initial)) |> 
  mutate(last_comma_commoninitial = ifelse(bioname == "KENNEDY, John Neely" & congress %in% 115:116, "404error", last_comma_commoninitial)) |> 
  # same first and last but they go by different 
  mutate(chamber_last = ifelse(bioname == "MALONEY, Carolyn Bosher" & congress %in% 113:117, "Representative MALONEY, C\\b", chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MALONEY, Sean Patrick" & congress %in% 113:117, "Representative MALONEY, S\\b", chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "ALLEN, Thomas H.", paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "GORDON, Barton Jennings", paste0(title, " ", last_name, "$"), chamber_last)) |> 
  
  mutate(commoninitial_last = ifelse(bioname == "JOHNSON, Timothy Peter (Tim)" & congress %in% 100:115, "T P Johnson", commoninitial_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MOORE, Gwendolynne S. (Gwen)"  & congress %in% c(109:119), "representative MOORE.{1,4}(G|UT)", chamber_last)) |> # not moore capito
  mutate(chamber_last = ifelse(bioname == "HERRERA BEUTLER, Jaime", "Representative (HERRERA BEUTLER|HERRERA-BEUTLER|HERRERA|BEUTLER)", chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "SCOTT, David", "Representative Scott, D\\b", chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "SCOTT, Austin", "Representative Scott, A\\b", chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "KIM, Andy", paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "SANFORD, Mark", paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "JOYCE, David" & congress %in% 113:119, paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "TERRY, Lee Raymond" & congress %in% 106:110, paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "LEVIN, Andy" & congress %in% 116:117, paste0(title, " ", last_name, ", ", first_initial, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "JACKSON, Jesse L., Jr." & congress %in% 104:112, paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "JOHNSON, Timothy V." & congress %in% 107:112, paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MURPHY, Timothy" & congress %in% 113:114, paste0(title, " ", last_name, "$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MURPHY, Timothy" & congress %in% 110:111, paste0(title, " ", last_name, ", t$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MURPHY, Patrick" & congress %in% 110:111, paste0(title, " ", last_name, ", p$"), chamber_last)) |> 
  mutate(chamber_last = ifelse(bioname == "MCCARTHY, Kevin" & congress %in% 110:113, paste0(title, " ", last_name, "$"), chamber_last)) |> # not caroline
  mutate(chamber_last = ifelse(bioname == "TORRES, Norma Judith" & congress %in% 116, paste0(title, " ", last_name, "$"), chamber_last)) |> # 
  mutate(chamber_last = ifelse(bioname == "JOHN, Christopher" & congress %in% 106:108, paste0(title, " ", last_name, "$"), chamber_last)) |> # 
  mutate(chamber_last = ifelse(bioname == "LUJÁN, Ben Ray" & congress %in% 113:115, paste0(title, " ", last_name, "$"), chamber_last)) |> # 
  mutate(chamber_last = ifelse(bioname == "BLUNT, Roy" & congress %in% 119, paste0(title, " ", last_name, "$"), chamber_last)) |> # not lisa blunt Rochester 
  mutate(chamber_last = ifelse(bioname == "ALEXANDER, Rodney" & congress %in% 108:113, paste0(title, " ", last_name, "$"), chamber_last))



# Deduping before assembling pattern
df <- members 

library(knitr)
library(dplyr)
library(tidyr)
library(stringr)


dupe_name_rows <- df %>%
  group_by(congress) %>% 
  mutate(.row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(matches("_last|_first|comma|_maiden|^last$|_state|chamber_")),
    names_to = "name_field",
    values_to = "name_value"
  ) %>%
  mutate(
    name_value_clean = str_to_lower(str_squish(name_value))
  ) %>%
  filter(!is.na(name_value_clean), 
         name_value_clean != "", 
         name_value_clean != "404error") %>%
  distinct(bioname, name_value_clean, congress)  %>% 
  add_count(name_value_clean, congress, name = "dup_n") %>% 
  filter(dup_n > 1) %>% 
  arrange(-congress, name_value_clean) 

# Inspect 
# NOTE: SOME OF THIS SHOULD BE FIXED ABOVE BECAUSE THEY CORRECTLY MATCH TO ONE AND NOT THE OTHER
# OTHERS NEED TO BE DELETED BECAUSE THEY ARE AMBIGIOUS 
dupe_name_rows |> 
  filter(congress > 105) |> 
  head(1000) |> 
  kable()

write_csv(dupe_name_rows, file = here::here("data", "inspect", "dupe_name_rows.csv"))

name_cols <- df %>%
  ungroup() %>% 
  select(matches("_last|_first|comma|_maiden|^last$|_state|chamber_"), -any_of("last_means")) %>%
  names()

name_cols

dupe_keys <- dupe_name_rows %>%
  distinct(congress, name_value_clean) %>%
  mutate(is_duplicate_name_value = TRUE)

df_deduped <- df %>%
  mutate(.row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(name_cols),
    names_to = "name_field",
    values_to = "name_value",
    values_transform = list(name_value = as.character)
  ) %>%
  mutate(
    name_value_clean = str_to_lower(str_squish(name_value))
  ) %>%
  left_join(
    dupe_keys,
    by = c("congress", "name_value_clean")
  ) %>%
  mutate(
    name_value = if_else(
      is_duplicate_name_value %in% TRUE,
      NA_character_,
      name_value
    )
  ) %>%
  select(-name_value_clean, -is_duplicate_name_value) %>%
  pivot_wider(
    names_from = name_field,
    values_from = name_value
  ) %>%
  arrange(.row_id) %>%
  select(-.row_id) %>%
  select(all_of(names(df)))


## DEDUPED ## 
members <- df_deduped 


################### MAKE PATTERN ###############
members %<>%
  # FIXME # ? is it ok to group by congress? 
  # IT THINK WE MUST GROUP BY CONGRESS BECAUSE SOME FIXES ARE BY CONGRESS
  group_by(bioname, congress) %>%
  mutate(pattern = c(first_last,
                     first_maiden,
                     first_middle_last,
                     first_initial_last,
                     first_middle_initial_last,
                     first_maiden_last,
                     maiden_comma_first,
                     maiden_comma_firstinitial,
                     common_last,
                     common_middle_last,
                     common_middle_initial_last,
                     commoninitial_last,
                     common_maiden,
                     last,
                     last_state,
                     last_comma_common,
                     last_comma_first,
                     last_first,
                     last_comma_initial,  # I worry about this over-matching, but we could test it--needed for VA # FIXME 
                     last_comma_commoninitial, # needed for VA, common name initials like Goodlatte, B. and Durbin, D.
                     chamber_last, 
                     #chamber_last_state, 
                     #chamber_last_firstinitial,
                     #last_comma_first_maiden, # this seems redundent
                     firstinitial_middleinitial_last
                     # last_comma_firstinitial_middleinitial, seems redundent #FIXME check in CMS "Carter, E.L." and "Butterfield, G. K."
                     #first_addlast,
                     #first_last_addlast,
                     #last_addlast_comma_first
                     ) %>%
    unique() %>% # unique matches 
    str_subset("404error", negate = T) %>% # drop missing
    str_c(collapse = "|") %>% # sep with OR 
    tolower() # %>% 
      # FIXME are these needed? 
      #str_replace_all(" \\[..\\].", " ") %>% 
      #str_replace_all("\\)\\[..\\].", "\\)")
    ) %>% # lower case 
  ungroup() %>% 
# post - hoc corrections for * that over-matches 
  mutate() 


# look <- members |> 
#   filter(str_detect(pattern, " \\[|\\]|\\)|\\[|\\]")) |> 
#   distinct(pattern)
# 










############ SUBSETTING ################################

members %<>% select(chamber, congress, bioname, pattern, icpsr, state, state_abbrev, 
                    nominate_dim1, nominate_dim2, nominate_number_of_votes,
                    district_code, bioguide_id, born, died,
                    chamber_last, party_code,
                    contains("first"),  contains("common"), contains("middle"), contains("last"), contains("maiden"))


# mismatches between middle name and middle initial? 
suspect_middle_names <- members %>%
  drop_na(middle_name) %>% 
  filter(!str_detect(middle_name, middle_initial) & !is.na(middle_name))
suspect_middle_names

  # causes problems, but should eventually be used for more targeted matching
 #FIXME  members %<>% select(-congresses) 

# load(file = "data/members_all.rda")

  
members %<>% arrange(-congress)

  members_min <- members |>  dplyr::select(chamber, congress, bioname, pattern, icpsr, state, state_abbrev, party_code,
                      #nominate_dim1, nominate_dim2, nominate_number_of_votes,
                      district_code, bioguide_id, first_name, last_name)
  
  members_all <- members 
  
  members <- members_min
  
  save(members_all, file = "data/members_all.rda")
  save(members, file = "data/members.rda")
  
  write_csv(members, "data/members.csv")
  
  #archive
  write_csv(members, paste0("data/members", Sys.Date(), ".csv"))
  
  
  
  if(F){
    save(members, file = "../legislators/data/members.rda")
    
  }
  
  