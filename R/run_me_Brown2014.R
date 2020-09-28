rm(list=ls())
graphics.off()
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "Brown2014" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName <- "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control

spidmap_file <- ""
spid_sheet <- ""

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)

# create a summary log file and store the 
if(save_notes_graphs) {
  sink(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_run_log_",as.character.Date(Sys.Date()),".txt")))
  cat("Output from the script run_me_",dataset_title,".R\n",sep="")
  cat("Date:",as.character.Date(Sys.Date()),"\n")
  cat("USER INPUT settings:\n")
  print(sapply(ls(), get, envir = .GlobalEnv))
  graphics.off()
  pdf(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_summary_plots_",as.character.Date(Sys.Date()),".pdf")))
}

# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)


# change untreated wells to Control Treatment ------------------------------------
dat[wllt == "n", treatment := default_ControlTreatmentName]
# update other control wells as needed, e.g.
dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "20190904", plates = paste0("MW69-381",7:9), control_rowi = which(LETTERS[1:6] %in% c("E","F")))
dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = c("20190710","20190724"), control_rowi = 1:6)
dat[wllt == "n", .N, by = "treatment"]


# Assign SPIDs ------------------------------------------------------------------
spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
head(spidmap)
unique(spidmap$Unit) # all mM? - yes
setnames(spidmap, old = c(2,5,3), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, spid := paste0("EPAPLT0",spid)]
spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
# update expected_stock_conc for individual compouunds where needed 
# for example, 
# spidmap[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
spidmap[, treatment := as.character(treatment)]
spidmap[, stock_conc := as.numeric(stock_conc)]
head(spidmap[, .(treatment, spid, stock_conc, expected_stock_conc)])

spidmap2 <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/glufosinates_spidmap.xlsx", sheet = 1))
setnames(spidmap2, old = c("Compound.name.in.MEA.data","SPID"), new = c("treatment","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap2[, stock_conc := NA_real_] # will get actual stock conc's from invitrodb
spidmap2[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
spidmap2[, treatment := as.character(treatment)]
spidmap2[, stock_conc := as.numeric(stock_conc)]
usecols <- c("spid","treatment","stock_conc","expected_stock_conc")
spidmap <- rbind(spidmap[, ..usecols], spidmap2[, ..usecols])
spidmap

# rename any compounds, if needed
dat[treatment == "Glufo", treatment := "L-Glufosinate Ammonium"]

# assign spids
dat <- check_and_assign_spids(dat, spidmap)


# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# if any, standardize those before continuing.
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
problem_comps
# [1] "100" "18"  "34"  "37"  "46"  "49"  "8"   "82"  "88"  "97" 
dat[treatment %in% problem_comps, .(paste0(sort(unique(signif(conc,3))),collapse=",")), by = .(treatment)]

# get a summary of the treatments by srcf
summary_dat <- dat[treatment %in% problem_comps, .(conc_shown = unique(conc)), by = .(apid, rowi, coli, treatment, srcf)]
summary_dat[, conc_round := signif(conc_shown, 1)]
summary_dat[, conc_source := ifelse(grepl("AUC",srcf),"AUC","Calc")]
summary_wide <- dcast(summary_dat, apid + treatment + rowi + coli ~ conc_source, value.var = "conc_shown")

# going to check out these treatments 1 by 1
summary_wide[treatment == "100"][order(AUC)] # it is clear that the issue is just due to rounding from the AUC data
# I wouldlike to just take the Calc values. see if same for other comopunds

summary_wide[treatment == "18"][order(AUC), .(AUC, round(Calc, 2))] # yep, these are all equal
summary_wide[treatment == "34"][order(AUC), .(AUC, round(Calc, 2))] # one difference remains: 9.64  9.63
summary_wide[treatment == "37"][order(AUC), .(AUC, round(Calc, 2))] # yep, these are all equal
# let's just check all at once for this
summary_wide[round(Calc, 2) != AUC]
# apid treatment rowi coli   AUC       Calc
# 1: 20190904_MW69-3817        34    3    4  0.30  0.2890500
# 2: 20190904_MW69-3817        34    3    7  9.64  9.6350000
# 3: 20190904_MW69-3818        34    1    4  0.30  0.2890500
# 4: 20190904_MW69-3818        34    1    7  9.64  9.6350000
# 5: 20190904_MW69-3819        34    2    4  0.30  0.2890500
# 6: 20190904_MW69-3819        34    2    7  9.64  9.6350000
# 7: 20190918_MW70-2406        46    4    4  0.30  0.2870715
# 8: 20190918_MW70-2406        46    4    5  1.00  0.9569050
# 9: 20190918_MW70-2406        46    4    6  3.00  2.8707150
# 10: 20190918_MW70-2406        46    4    7 10.00  9.5690500
# 11: 20190918_MW70-2406        46    4    8 30.00 28.7071500
# for these 2 compounds, what is "correct"?
spidmap[treatment == "34", signif(stock_conc/expected_stock_conc*10,3)] # 9.63
# so for 34, again I want to just use the values from the Calc file.
spidmap[treatment == "46", signif(stock_conc/expected_stock_conc*10,3)] # 10
# for this compound, I want to use the conc's listed from the AUC data instead

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
# conc    V1
# 1:  0.028905  0.03
# 2:  0.096350  0.10
# 3:  0.289050  0.30
# 4:  0.963500  1.00
# 5:  2.890500  3.00
# 6:  9.635000 10.00
# 7: 28.905000 30.00
dat[treatment == "34", conc := signif(conc, 1)]
dat[, conc_test := NULL]

# confirming this is empty now
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]

# finally, run this:
source(file.path(scripts.dir, 'confirm_concs.R'))
dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30))
rm(list=c("spidmap","spidmap2","summary_dat","summary_wide"))


# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?


# save dat and graphs
write.csv(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")), row.names = F)
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")


# EXTRA DATA CHECKS:
# determining which copy of the spike list files to use, and if they are different at all
filei <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423/ON_20140423_MW1007-38_02_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv"
data.raw<-read.csv(filei,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
data.info <- read.csv(filei,header=F,colClasses=c("character","character"), nrows = 100)
head(data.raw)
data.info
data.raw # this is it??
# V3        V4                   V5
# 1      Time (s) Electrode        Amplitude(mV)
# 2    56.2676800     A8_23 0.028872283887509825
# 3   126.3688800     A8_23 0.030162792900087505
# 4    730.171200     C2_24 0.021529744499527877
# 5    730.171200     C2_44 0.023524730659420935
# 6   730.1712800     C2_33 0.023111509512467321

# well, that was just DIV 2
filei <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423/ON_20140423_MW1007-38_05_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv"
data.raw<-read.csv(filei,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
data.info <- read.csv(filei,header=F,colClasses=c("character","character"), nrows = 100)
data.info
data.raw # okay, this looks good
data.raw_0 <- data.raw

# comparing with the other file
filei <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Specific Aim 1/Regenerated spikelist files SA1 compounds/20140423_1/ON_20140423_MW1007-38_05_00(000)_Spike Detector (8 x STD)(000)_spike_list.csv"
data.raw<-read.csv(filei,header=F,colClasses=c("NULL", "NULL", "character","character","character")) # make electrode column char, not factor
data.info <- read.csv(filei,header=F,colClasses=c("character","character"), nrows = 100)
data.info
data.raw # okay, this looks good
data.raw_1 <- data.raw

# comparison
nrow(data.raw_0)
# [1] 9914
nrow(data.raw_1)
# [1] 9843
tail(data.raw_0) # lots of empty rows
tail(data.raw_1)
data.raw_0 <- data.raw_0[data.raw_0$V3 != "",] # chopping off several rows at the end
tail(data.raw_0) # okay, still have well summary info here. snippet:
# 9841 913.8309600   D3_13 0.061697795920477749
# 9842  914.167600   A3_43 0.028635573871101324
# 9843  914.215600   F2_22 0.074398127943828149
# 9845          A2      A3                   A4
# 9846          A7      A8                   B1
# 9847          B4      B5                   B6
# 9848          C1      C2                   C3
data.raw_0 <- data.raw_0[1:9843,]
all.equal(data.raw_0, data.raw_1)
# TRUE!
# okay!
# so now the question is... which do I use? Do I need to check every other file now?

