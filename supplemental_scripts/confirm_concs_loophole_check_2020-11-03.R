library(data.table)
library(RMySQL)
library(tcpl)
library(openxlsx)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder should be located
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))

alldat <- get_latest_dat()

spids <- alldat[wllt == "t", unique(spid)]

con <- dbConnect(drv = RMySQL::MySQL(), dbname = "invitrodb", host = Sys.getenv('INVITRODB_HOST_RO'), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'))
all_sample_info <- dbGetQuery(con, paste0("SELECT * FROM sample WHERE spid IN ('",paste0(spids,collapse="','"),"');"))
all_sample_info
length(spids)
length(unique(all_sample_info$spid))
# both 411
dbDisconnect(con)

# get the expected stock conc's for all
spid_set <- alldat[wllt == "t", .(dataset = unique(dataset)), by = .(spid, treatment, mea_treatment_name)]
spid_set[dataset == 'Brown2014', expected_stock_conc := 20]
spid_set[dataset == 'DNTGF2019', expected_stock_conc := 20]
spid_set[dataset == 'NTP91', expected_stock_conc := 20]
# update expected_stock_conc for individual compouunds where needed 
spid_set[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
spid_set[treatment == "Chrysene", expected_stock_conc := 9.7]
spid_set[dataset == 'OPP2015', expected_stock_conc := 100]
spid_set[dataset == 'PFAS2018', expected_stock_conc := 30] # only exceptions are the control compounds, which will change
spid_set[dataset == 'ToxCast2016', expected_stock_conc := 20]
spid_set[dataset == 'Frank2017', expected_stock_conc := 20]
shafer_12 <- as.data.table(read.xlsx(file.path(root_output_dir,"Sample IDs","EPA_ES204_EPA-Shafer_12_20200117_key.xlsx")))
spid_set[dataset == 'Frank2017' & spid %in% shafer_12$EPA_SAMPLE_ID, expected_stock_conc := 100]
unique(spid_set$expected_stock_conc) # none are NA

# get the expected_target_concs
spid_set[dataset == 'Brown2014', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,20),collapse=",")]
spid_set[dataset == 'DNTGF2019', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,30),collapse=",")]
spid_set[dataset == 'NTP91', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,20),collapse=",")]
spid_set[dataset == 'OPP2015', expected_target_concs := paste0(c(0.1,0.3,1,3,10,30,100),collapse=",")]
spid_set[dataset == 'PFAS2018', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,30),collapse=",")] # only exceptions are the control compounds, which will change
spid_set[dataset == 'ToxCast2016', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,20),collapse=",")]
spid_set[dataset == 'Frank2017', expected_target_concs := paste0(c(0.03,0.1,0.3,1,3,10,30),collapse=",")]

# determine if there are any instances where
# - the expected stock conc != real stkc AND
# - source_concs != spidmap_guess_concs (since that is how need_to_update is determined in confirm_concs)

# need to get the original conc's...
# would need to scour the run_me's for all of the updates made to the MEA Conc's entered...

# getting the original conc's from the auc data
auc <- data.table()
for (dataseti in unique(alldat$dataset)) {
  add.dat <- as.data.table(read.csv(file.path(root_output_dir,dataseti,"output",paste0(dataseti,"_AUC.csv")),stringsAsFactors = F))
  add.dat[, dataset := dataseti]
  auc <- rbind(auc, add.dat)
}

# just extract the spid/conc info that we want
trt_tb <- auc[, unique(.SD), .SDcols = c("well","date","Plate.SN","treatment","dose","dataset")]
trt_tb[, `:=`(coli = as.numeric(sub("[[:alpha:]]","",well)),
              rowi = match(sub("[[:digit:]]","",well),LETTERS),
              apid = paste0(date,"_",Plate.SN),
              conc = dose)]
trt_tb[, c("well","date","Plate.SN","dose") := NULL]
trt_tb[, unique(.SD), .SDcols = c("apid","treatment","rowi","coli","conc")][, .N, by = .(apid, rowi, coli)][N != 1] # confirming this is empty



# make ajustments were you edited the conc before confirm_Conc's in each run_me --------------------------------------------------

# Brown - wherever cyto and auc conc's disagreed, used auc conc's
# updating DNTGF conc's
# NTP91 - no changes
# OPP2015 - no changes
# PFAS2018 - no changs
# ToxCast2016 - no changes
# Frank2017 - no prob comps
# DNGF2019 - see below
dataset_title <- "DNTGF2019"
scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)
dat[treatment == "Glufo", treatment := "L-Glufosinate Ammonium"]

# get a summary of the treatments by srcf
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
summary_dat <- dat[treatment %in% problem_comps, .(conc_shown = unique(conc)), by = .(apid, rowi, coli, treatment, srcf)]
summary_dat[, conc_round := signif(conc_shown, 1)]
summary_dat[, conc_source := ""]
summary_dat[grepl("(Calc)|(Summary)",srcf), conc_source := "Calc"]
summary_dat[grepl("AUC",srcf), conc_source := "AUC"]
summary_dat[grepl("DIV",srcf), conc_source := "DIV"]
summary_wide <- dcast(summary_dat, apid + treatment + rowi + coli ~ conc_source, value.var = "conc_shown")
summary_wide[, use_conc := Calc]
summary_wide[treatment == "46", use_conc := AUC]
summary_wide[, use_conc := as.numeric(use_conc)]

dat <- merge(dat, summary_wide[, .(apid, treatment, rowi, coli, use_conc)], all.x = T, by = c("apid","treatment","rowi","coli"))
dat[!is.na(use_conc), conc := use_conc]
dat[, use_conc := NULL]

# actually, for 34, I need to just update this manually
dat[treatment == "34", unique(conc)]
dat[treatment == "34", conc_test := signif(conc, 1)] # I can do this becuase teh stock conc is close enough to 20, so will round correctly
dat[treatment == "34", .(unique(conc_test)), by = "conc"]
dat[treatment == "34", conc := signif(conc, 1)]

dat[, unique(.SD), .SDcols = c("apid","treatment","rowi","coli","conc")][, .N, by = .(apid, rowi, coli)][N != 1] # oh dear
dat[, .(N = length(unique(signif(conc,3))), concs = paste0(sort(unique(conc)),collapse=",")), by = c("apid","rowi","coli")][N!=1] # ah, but this is empty
# so it's just a roudnign thing - so conc's are slightly diff from diff data sources
dat[, .(N = length(unique(signif(conc,4))), concs = paste0(sort(unique(conc)),collapse=",")), by = c("apid","rowi","coli")][N!=1] # okay there are some... but it doesn't matter
dat[, conc := signif(conc,3)]
dat[, unique(.SD), .SDcols = c("apid","treatment","rowi","coli","conc")][, .N, by = .(apid, rowi, coli)][N != 1][, .N, by = "apid"] # now this is empty
dat[, dataset := "DNTGF2019"]

# add this back to trt_Tb
trt_tb <- trt_tb[dataset != "DNTGF2019"]
trt_tb <- rbind(trt_tb, dat[, unique(.SD), .SDcols = c("apid","treatment","rowi","coli","conc","dataset")])
rm(dat)

# remove contorl wells
trt_tb <- trt_tb[conc != 0]

# get the corresponding spids for trt_Tb and spid_tb
trt_tb <- merge(trt_tb, alldat[, unique(.SD), .SDcols = c("spid","apid","coli","rowi","dataset")],
      by = c("apid","coli","rowi","dataset"))
spid_tb <- merge(all_sample_info, spid_set, by = "spid", all = T)
setDT(spid_tb)
spid_tb[signif(expected_stock_conc,3) != signif(stkc,3), .(spid, mea_treatment_name, stkc, expected_stock_conc)]


# get the spidmap_guess_concs for spid_tb
# spid_Tb has 1 row for each spid/dataset
spid_tb[, spidmap_guess_concs := NA_character_]
for (i in 1:nrow(spid_tb)) {
  expected_target_concs_vector <- as.numeric(unlist(strsplit(spid_tb[i, c(expected_target_concs)],split=",")))
  spid_tb[i, spidmap_guess_concs := paste0(signif(stkc/expected_stock_conc*expected_target_concs_vector,3),collapse=",")]
}
spid_tb[is.na(mea_treatment_name), mea_treatment_name := treatment]

compare_concs <- merge(spid_tb[, .(spid, mea_treatment_name, dataset, stkc, expected_stock_conc, spidmap_guess_concs)],
                       trt_tb[, .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=","), num_concs = length(unique(conc))), by = c("spid","dataset")], 
                       by = c("spid","dataset"), all.y = TRUE)
compare_concs[is.na(stkc)] # empty - evertying merged!

# I am concerned about these cases:
compare_concs[source_concs == spidmap_guess_concs & expected_stock_conc != stkc]
# most are from DNTGF2019 - where I know Kathleen already did a lot of conc-correction already. I believe this is correct
# note that these are the only spids whose conc's were corrected fo rdNtgf:
# The concentrations for the following compounds might need to be corrected:
#   spid    stkc expected_stock_conc                      spidmap_guess_concs              treatment               source_concs
# 1: EPAPLT0169A05 19.2673                  20 0.0289,0.0963,0.289,0.963,2.89,9.63,28.9                     34     0.03,0.1,0.3,1,3,10,30
# 2:      EX000374 20.0000                  20                   0.03,0.1,0.3,1,3,10,30 L-Glufosinate Ammonium 0.03,0.1,0.3,1,3,10,30,100
# NTP91 - looks correct~
# the 2 from OPP2015 - also slooks okay
