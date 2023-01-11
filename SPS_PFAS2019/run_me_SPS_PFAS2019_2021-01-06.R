# *** NOT RUN ***************
# The output from this script as of 2021-01-06 has been saved in the folder:
# 'output_2021-01-06'
# if this script is run again, it will overwrite the more recent output in the folder 'output'
# (the only updates as of 5/18/2021 pertain to changes in the chem conc/names in source files,
# or changes in wllq. The data values as of 5/18/2021 and 01/06/2021 are the same)
# ****************************

rm(list=ls()) # clear environment
graphics.off() # clear plot history
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "SPS_PFAS2019" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- TRUE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

copy_maestro_exp_log_treatments <- FALSE # For cytotox data, keep treatment names from Calc/Summary files, or overwrite with maestro exp log treatment names?
default_ControlTreatmentName = "DMSO" # all compounds other than those listed below should have this vehicle control

spidmap_file <- ""
spid_sheet <- ""

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created

update_concs_without_prompt <- TRUE
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)

# create a summary log file and store the 
if(save_notes_graphs) {
  sink(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_run_log_",as.character.Date(Sys.Date()),".txt")), append = T)
  cat("Output from the script run_me_",dataset_title,".R\n",sep="")
  cat("Date Ran:",as.character.Date(Sys.Date()),"\n")
  cat(R.version.string,"\n")
  cat("USER INPUT settings:\n")
  print(sapply(ls(), get, envir = .GlobalEnv))
  graphics.off()
  pdf(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_summary_plots_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)
}


# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))


# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)


# change untreated wells to Control Treatment ------------------------------------
# wllt ==n wherever conc==0 currently
dat[wllt == 'n', unique(treatment)] # "DMSO"     "Media"    "3360 G12". Cool, most already labelled!
dat[treatment == '3360 G12', unique(.SD), .SDcols = c('apid','rowi','coli','srcf','conc')]
# apid rowi coli                                             srcf conc
# 1: 20201209_MW71-7211    1    6 20201209_NFA_PFAS_Group_2_SPS__Calculations.xlsx   30
# 2: 20201209_MW71-7213    2    6 20201209_NFA_PFAS_Group_2_SPS__Calculations.xlsx   30
# 3: 20201209_MW71-7214    3    6 20201209_NFA_PFAS_Group_2_SPS__Calculations.xlsx   30
# 4: 20201209_MW71-7211    1    6               SPS_PFAS2019_parameters_by_DIV.csv    0
# 5: 20201209_MW71-7213    2    6               SPS_PFAS2019_parameters_by_DIV.csv    0
# 6: 20201209_MW71-7214    3    6               SPS_PFAS2019_parameters_by_DIV.csv    0
# 7: 20201209_MW71-7211    1    6                             SPS_PFAS2019_AUC.csv    0
# 8: 20201209_MW71-7213    2    6                             SPS_PFAS2019_AUC.csv    0
# 9: 20201209_MW71-7214    3    6                             SPS_PFAS2019_AUC.csv    0
# From Seline 1/6 - this conc should be 30. Is not updated in maestroexplog
dat[treatment == '3360 G12',`:=`(conc = 30, wllt = 't')]
dat[wllt == 'n', unique(treatment)] #  "DMSO"  "Media"
dat[treatment %in% c('DMSO','Media'), unique(wllt)] # "n" for all

# From Seline 1/6: they couldn't test 5 compounds because of low stock availability
# See Readme's from the 5 compounds. 1 from G1, 4 form G4
# currently these 5 labelled as "Media" in mea data, but as treated in calc files
dat[treatment %in% c('3360 C09','3612 G02','3612 H01','3612 H02','3612 H03'), `:=`(treatment ='Media', conc = 0)]

# Check for any disagreements betweecompare treatment names in AUC and Calc data
(qry_wells <- dat[, .(length(unique(treatment))), by = .(apid, rowi, coli)][V1 > 1, .(apid, rowi, coli)])
setkey(dat, apid, rowi, coli)
dat[J(qry_wells), .(unique(treatment)), by = .(apid, srcf, rowi, coli, conc)]
# empty

# Set the control well concentration. Adjust as needed
dat[wllt == "n", conc := 0.001] # not goign to bother finding this right now

# I'm going to set wllt to p for Bisphenol (currently set as t, but I don't have a spid for it)
dat[treatment == 'Bisphenol', unique(conc)]
# [1] 30
dat[treatment == 'Bisphenol', wllt := 'p']

# Setting Media to 'b' for blank
dat[treatment == 'Media', `:=`(wllt = 'b', conc = NA_real_)]


# assign sample ID's -------------------------------------------------------------
spidmap <- as.data.table(read.xlsx("L:/Lab/NHEERL_Mundy/Project - PFAS 2019/Supporting Doc/EPA_27864_EPA-Shafer_134_20191001_key.xlsx", sheet = 1))
spidmap2 <- as.data.table(read.xlsx("L:/Lab/NHEERL_Mundy/Project - PFAS 2019/Supporting Doc/EPA_29885_EPA-Shafer_36_20191112_key.xlsx", sheet = 1))
spidmap <- rbind(spidmap, spidmap2)
spidmap[, short_rackplate := sub("SRACK0","",RACKPLATE_BARCODE)]
unique(spidmap$short_rackplate) # "3360" "3361"
spidmap[, Compound.Name := paste(short_rackplate, WELL_POSITION, sep = " ")]
spidmap[, unique(CONCENTRATION_UNIT)] # mM for all
setnames(spidmap, old = c('Compound.Name', 'EPA_SAMPLE_ID'), new = c("treatment","spid"))
spidmap[, treatment := as.character(treatment)]
head(spidmap[, .(treatment, spid)])

# Add additional spidmap's if needed and rbind into 1 spidmap

# check if every treatment name from the mea data maps to a unique sample in spidmap
setdiff(dat$treatment, spidmap$treatment) # "DMSO"      "Media"     "Bisphenol", that's fine
spidmap[treatment %in% unique(dat$treatment), .N, by = .(treatment)][N > 1] # checking for treatments that match multiple spid - empty
# if there is not a 1-to-1 correspondence, update treatment names in "supplemental_mea_treatment_name_map.csv"

# update treatment names with entries in "supplemental_mea_treatment_name_map.csv" corresponding to dataset
# (treatment -> "mea_treatment_name", "updated_treatment_name" column will match "PREFERRED_NAME"
dat <- update_treatment_names(dat, root_output_dir, dataset_title)

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
# character(0)
dat[treatment %in% problem_comps, unique(conc), by = .(spid, treatment, srcf)]
# Seline corrected the conc's for these in the calc files and maestroexplog
# I have not yet re-ran all with the updated maestroexplog, so I will use conc's from the calc files
dat[treatment %in% problem_comps, conc := conc[grep('Calculations',srcf)[1]], by = .(spid, treatment)]
dat[treatment %in% problem_comps, unique(conc), by = .(spid, treatment, srcf)]
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# all consistent now

# Even though the conc-correction is a bit unnecessary, 
# I do need to know if anything was tested at multiple conc's
dat <- merge(dat, spidmap[, .(spid, expected_stock_conc = CONCENTRATION)], by = 'spid', all.x = T)
(check.trts <- dat[wllt == 't', length(unique(conc)), by = .(treatment)][V1 > 1, unique(treatment)])
# empty

dat[conc != expected_stock_conc] # empty!

# skipping conc update for now
# finally, run this:
# source(file.path(scripts.dir, 'confirm_concs.R'))
# dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30), update_concs_without_prompt = update_concs_without_prompt)


# FINAL DATA CHECKS -------------------------------------------------------------
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?
dat[, well_id := paste(apid, rowi, coli, sep = "_")]
dat[, .(rep_count = length(unique(well_id))), by = .(treatment)][rep_count != 3]
# treatment rep_count
# 1: Bisphenol        10
# 2:      DMSO        60
# 3:     Media        17
# cool, only control wells
dat[, well_id := NULL]

dat[is.na(rval) & !grepl('DIV',acsn)]
# empty

# save dat and graphs
setkey(dat, NULL)
save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")
