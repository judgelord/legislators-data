devtools::install_github("judgelord/congressionalrecord")
library(congressionalrecord)


cr_metadata <- get_cr_df(as.Date("2007/03/01"), section = "senate-section")
