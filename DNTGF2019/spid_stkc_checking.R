# searching all SPIDs tested in DNT
library(RMySQL)
spids_tested <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 NFA MEA/Supporting Documents/Key.xlsx",sheet = "Plate Links"))
spids_tested
setnames(spids_tested, old = grep("SAMPLE",names(spids_tested)), new = paste0("SAMPLE.ID",c(1:5)))
spids_tested[, preferred.name := ""]
for (chemical.ID in unique(spids_tested$Chemical.ID)) {
  all_names <- spids_tested[Chemical.ID == chemical.ID, .SD, .SDcols = grep("PREFERRED.NAME",names(spids_tested))]
  all_names <- unlist(all_names, use.names = F)
  use_name <- unique(all_names)[!is.na(unique(all_names))]
  spids_tested[Chemical.ID == chemical.ID, preferred.name := use_name]
}

all_spids <- unique(c(unique(spids_tested$SAMPLE.ID1), 
                    unique(spids_tested$SAMPLE.ID2),
                    unique(spids_tested$SAMPLE.ID3),
                    unique(spids_tested$SAMPLE.ID4),
                    unique(spids_tested$SAMPLE.ID5)))
all_spids <- all_spids[!is.na(all_spids)]

#now, for every compound name, let's find the stck conc. See if there are any differences
chem.info <- tcplLoadChem(field = "chnm", val = unique(spids_tested$preferred.name))

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='invitrodb',host = Sys.getenv('INVITRODB_HOST_RO'))
sample.info <- dbGetQuery(con, paste0("SELECT * FROM sample WHERE spid IN('",paste0(all_spids,collapse="','"),"');"))
setDT(sample.info)
nrow(sample.info) == length(all_spids) # TRUE, so nothing got dropped
sample.info[, length(unique(stkc)), by = "chid"]
# N = 1 for all!!! That means that even chem with multiple SPIDs all have the same stck
# so it is possibly OKAY if I select the 'wrong' spid
dbDisconnect(con)

# gut confirmation for one of these
# Buspirone EPAPLT0168A09
sample.info[spid == "EPAPLT0168A09", unique(chid)]
# [1] 22707
sample.info[chid == "22707"]
# spid  chid    stkc stkc_unit tested_conc_unit
# 1: EPAPLT0167A09 22707 19.9995        mM               uM
# 2: EPAPLT0168A09 22707 19.9995        mM               uM
# 3: EPAPLT0169C08 22707 19.9995        mM               uM
# 4: EPAPLT0170G06 22707 19.9995        mM               uM
# 5: EPAPLT0171A09 22707 19.9995        mM               uM
