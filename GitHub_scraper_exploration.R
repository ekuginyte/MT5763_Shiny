library(dplyr)
library(gh)

req <- GET(paste0("https://api.github.com/repos/",
           "CSSEGISandData"))
