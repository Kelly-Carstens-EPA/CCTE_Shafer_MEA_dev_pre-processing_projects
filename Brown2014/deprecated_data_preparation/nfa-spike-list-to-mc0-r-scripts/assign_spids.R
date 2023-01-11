# add SPIDs and clean up the data
library(data.table)
library(xlsx)

alldat <- fread("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/Brown2016_longfile.csv")

# removing 
alldat[, unique(treatment)]

# c("Acet","Acetaminophen")
# c("Van","Sodium Orthovanadate")
# c("Bis 1","Bisindolymaleimide 1")
# c("Lop","Loperamide")
# c("Mev","Mevastatin")
# c("Domoic","Domoic Acid")

# I don't think I should remove Glyphosate, if I have a spid for it

spidmap1 <- read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/SPID_maps/Copy of EPA_ES202_EPA-Shafer_103_20191218_key.xlsx",1,stringsAsFactors = F)
spidmap2 <- read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/SPID_maps/Copy of EPA_ES203_EPA-Shafer_42_20200110_key.xlsx",1,stringsAsFactors = F)
setDT(spidmap1)
setDT(spidmap2)

check_for_spids <- function(cname) {
  spid1 <- spidmap1[CASRN == cname, EPA_SAMPLE_ID]
  spid2 <- spidmap2[CASRN == cname, EPA_SAMPLE_ID]
  spid <- c(spid1, spid2)
  if (length(spid) == 1) {
    return(spid)
  } else {
    stop(paste0("spids found: ",spid))
  }
}

# CASRN taken from Table 1 in Brown et al, 2016
# c("Acet","Acetaminophen")
alldat[treatment %in% c("Acet","Acetaminophen"), treatment := check_for_spids("103-90-2")]

# c("Van","Sodium Orthovanadate")
alldat[treatment %in% c("Van","Sodium Orthovanadate"), treatment := check_for_spids("13721-39-6")]

# c("Bis 1","Bisindolymaleimide 1")
# spidmap2[grepl("Bis",PREFERRED_NAME),] # Bisindolylmaleimide I
alldat[treatment %in% c("Bis 1","Bisindolymaleimide 1"), treatment := check_for_spids("133052-90-1")]

# c("Lop","Loperamide")
alldat[treatment %in% c("Lop","Loperamide"), treatment := check_for_spids("34552-83-5")]

# c("Mev","Mevastatin")
alldat[treatment %in% c("Mev","Mevastatin"), treatment := check_for_spids("73573-88-3")]

# c("Domoic","Domoic Acid")
alldat[treatment %in% c("Domoic","Domoic Acid"), treatment := check_for_spids("14277-97-5")]

# Glyphosate
spid1 <- spidmap1[PREFERRED_NAME == "Glyphosate", EPA_SAMPLE_ID] # one result here
spidmap2[PREFERRED_NAME == "Glyphosate", EPA_SAMPLE_ID] # no result here
alldat[treatment == "Glyphosate", treatment := spid1]

setnames(alldat, old = "treatment", new = "spid")

fwrite(alldat, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_old_data_for_tcpl/Brown2016/Intermediate_output/Brown2016_longfile_withspids.csv")
