# Tranform json text into excel for BPA sample ID
# Jun 9, 2021
# will separate out this fun if I ever need in the future

# Learning/experimenting ------------------------------------------
library(rjson)
dat <- fromJSON(file = 'chemtrack-prod.epa.gov/api/sample_details/EX000597')
# Error in file(con, "r") : cannot open the connection
# In addition: Warning message:
#   In file(con, "r") :
#   cannot open file 'chemtrack-prod.epa.gov/api/sample_details/EX000597': No such file or directory
# hmm....

# Shows raw data which is not structured and readable
jsonRespText<-content(resp,as="text") 
fromJSON(jsonRespText)

# View all dependencies of package to install (making sure it doesn't require rJava)
view_all_dependencies <- function(pack, p, i) {
  print(i)
  depends_str <- p[rownames(p) == pack, 'Depends']
  depends_vals <- strsplit(depends_str, split = ', ')[[1]]
  depends_vals <- sub(' .*$','',depends_vals) # remove version in parentheses
  if (length(depends_vals) > 0) {
    i <- i+1
    if (i > 1) browser()
    additional_vals <- unlist(lapply(depends_vals, view_all_dependencies, p, i))
    depends_vals <- unique(c(depends_vals, additional_vals))
  }
  return(depends_vals)
}

p <- available.packages()
view_all_dependencies('httr', p, i = 1)

# Or just use packrat...
.libPaths()
# [1] "C:/Users/Administrator/OneDrive/Profile/Documents/R/win-library/4.0"
# [2] "C:/Program Files/R/R-4.0.3/library"    
# first value has more recent version of R installed
packrat:::recursivePackageDependencies('httr', lib.loc = .libPaths()[1])
# [1] "R6"       "askpass"  "curl"     "jsonlite" "mime"     "openssl"  "sys"
# cool, no rJava

install.packages('httr')

library(httr)
link <- 'http://chemtrack-prod.epa.gov/api/sample_details/EX000597'
resp <- GET(link) # could modify with add'l arguments e.g. query = list(page = "2")

# Basic checks
http_type(resp)
## [1] "application/json"
http_error(resp) # FALSE

jsonRespText<-content(resp,as="text")
jsonRespText
# [1] "{\"rack_plate_barcode\":\"NFA\",\"...
# This is basically just the raw text that I can see at the link in my web browser
resp2 <- fromJSON(jsonRespText)
typeof(resp2) # list

# another option:
jsonRespParsed<-content(resp,as="parsed")
jsonRespParsed
all.equal(resp2, jsonRespParsed)
# TRUE!

?content
# reading that as = 'parsed' is for convenience, shouldn't be used within a package bc there might be changes


# ANYHOW ------------------------------------------------
rm(list = ls())
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')
library(data.table)
library(httr)
library(rjson)

get_spid_info_from_json <- function(link) {
  resp <- GET(link)
  stopifnot(http_type(resp) == 'application/json', !http_error(resp))
  jsontext <- content(resp,as="text")
  resp2 <- fromJSON(jsontext)
  dat <- as.data.table(resp2)
  names(dat) <- toupper(names(dat))
  dat[, URL := link]
  return(dat)
}

link <- 'http://chemtrack-prod.epa.gov/api/sample_details/EX000597'
dat <- get_spid_info_from_json(link)

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, sheet = 'Sheet1')
writeData(wb, sheet = 1, x = dat)
saveWorkbook(wb, file = paste0('Sample IDs/EPA_Shafer',nrow(dat),'_',format(Sys.Date(), '%Y%m%d'),'.xlsx'))
             
             
