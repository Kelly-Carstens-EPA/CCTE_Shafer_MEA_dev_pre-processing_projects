rm(list=ls()) # clear environment
graphics.off() # clear plot history
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "DNTFalseNegatives" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- TRUE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName <- "DMSO" # all compounds other than those listed below should have this vehicle control

spidmap_file <- ""
spid_sheet <- ""

scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R" # update to the folder where the scripts are located
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created

update_concs_without_prompt <- FALSE
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)

# create a summary log file and store the 
if(save_notes_graphs) {
  sink(file = file.path(root_output_dir, dataset_title, paste0(dataset_title,"_run_log_",as.character.Date(Sys.Date()),".txt")))
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
dat[, .N, by = .(treatment)]
dat[wllt == 'n' & treatment != 'DMSO', .N, by = .(srcf, conc)]
# srcf conc_srcf  N
# 1: 20210818_NFA_False_Negatice_Repeats__Calculations.xlsx         0 36
# Only not labelled as DMSO in the cytotox data. But I know should be solvent control bc treatment conc is 0
dat[wllt == "n", treatment := default_ControlTreatmentName]
# Manually update other wells where control treatment is not the default, or use teh function below
# dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "")

# Set the control well concentration. Adjust as needed
dat[wllt == "n", conc := 0.001]


# Assign sample ID's -------------------------------------------------------------
dat[, .N, by = .(treatment)]
#                treatment    N
# 1:                 6-PPD 1827
# 2:                  DMSO 1566
# 3:         6-PPD Quinone 1827
# 4:              Caffeine 1827
# 5: 5,5-Diphenylhydantoin 1827
# 6:         Dexamethasone 1827
# 7:                 Maneb 1827
# I don't have any spids for these compounds
# but the compound names are consistent! So we'll just stick with these for now
dat[, spid := treatment]

# Will run this later when have spids
# spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
# head(spidmap)
# unique(spidmap$Concentration_Unit) # all mM?
# unique(dat$units) # confirm these are all uM (this taken from maestroexperiment log file)
# setnames(spidmap, old = c(trt_col, spid_col), new = c("treatment","spid"))
# # for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
# spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
# # update expected_stock_conc for individual compouunds where needed 
# # for example, 
# # spidmap[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
# spidmap[, treatment := as.character(treatment)]
# head(spidmap[, .(treatment, spid, expected_stock_conc)])
# 
# # Add additional spidmap's if needed and rbind into 1 spidmap
# 
# # check if every treatment name from the mea data maps to a unique sample in spidmap
# setdiff(dat$treatment, spidmap$treatment) # checkign for missed treatments
# spidmap[treatment %in% unique(dat$treatment), .N, by = .(treatment)][N > 1] # checking for treatments that match multiple spid
# # if there is not a 1-to-1 correspondence, update treatment names in "supplemental_mea_treatment_name_map.csv"
# 
# # update treatment names with entries in "supplemental_mea_treatment_name_map.csv" corresponding to dataset
# # (treatment -> "mea_treatment_name", "updated_treatment_name" column will match "PREFERRED_NAME"
# dat <- update_treatment_names(dat, root_output_dir, dataset_title)
# 
# # assign spids
# dat <- check_and_assign_spids(dat, spidmap)


# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound
dat[, conc_srcf := conc] # save the original conc's in a column

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# if any, standardize those before continuing.
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
problem_comps
# character(0)

# Since these spids haven't been registered, just check out the conc's
for (treatmenti in unique(dat$treatment)) {
  cat(treatmenti, '\n')
  print(dat[treatment == treatmenti, table(conc)])
}
# 6-PPD 
# conc
# 0.03  0.1  0.3    1    3   10   30 
# 261  261  261  261  261  261  261 
# DMSO 
# conc
# 0.001 
# 1566 
# 6-PPD Quinone 
# conc
# 0.00267  0.0089  0.0267   0.089   0.267    0.89    2.67 
# 261     261     261     261     261     261     261 
# Caffeine 
# conc
# 0.1 0.3   1   3  10  30 100 
# 261 261 261 261 261 261 261 
# 5,5-Diphenylhydantoin 
# conc
# 1    3   10   30  100  300 1000 
# 261  261  261  261  261  261  261 
# Dexamethasone 
# conc
# 0.1 0.3   1   3  10  30 100 
# 261 261 261 261 261 261 261 
# Maneb 
# conc
# 0.01 0.03  0.1  0.3    1    3   10 
# 261  261  261  261  261  261  261 
# Looks good & consistent!

# # finally, run this:
# source(file.path(scripts.dir, 'confirm_concs.R'))
# con <- dbConnect(drv = RMySQL::MySQL(), user = "", pass = "", dbname='',host = "")
# dat <- confirm_concs(dat, spidmap, con, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30), update_concs_without_prompt = update_concs_without_prompt)
# dbDisconnect(con)


# > Concentration units ---------------------------------------------------

dat[, .N, by = .(treatment, units)]
# treatment units    N
# 1:                 6-PPD  <NA>   42
# 2:                  DMSO  <NA>   36
# 3:         6-PPD Quinone  <NA>   42
# 4:              Caffeine  <NA>   42
# 5: 5,5-Diphenylhydantoin  <NA>   42
# 6:         Dexamethasone  <NA>   42
# 7:                 Maneb  <NA>   42
# 8:                 6-PPD    uM 1785
# 9:                  DMSO    uM 1275
# 10:         6-PPD Quinone µg/ml 1785
# 11:                  DMSO µg/ml  255
# 12:              Caffeine    uM 1785
# 13: 5,5-Diphenylhydantoin    uM 1785
# 14:         Dexamethasone    uM 1785
# 15:                 Maneb    uM 1785

# Update DMSO
dat[treatment == 'DMSO', units := 'fraction']

# Fill where is NA
dat[is.na(units), .N, by = .(srcf)]
# srcf   N
# 1: 20210818_NFA_False_Negatice_Repeats__Calculations.xlsx 288
# units not defined in calculations files
# But I know same concentrations used in cyto assay as in NFA (bc same source wells!)
# So I will just update with the units from Meastro log file for MEA endpoints
dat[, .(length(unique(units[!is.na(units)]))), by = .(treatment, spid)][V1 > 1] # empty, good
dat[, units := unique(units[!is.na(units)]), by = .(treatment, spid)]

# Convert to uM
dat[treatment == '6-PPD Quinone', mol_weight_g_per_mol := 298.38] # see notebook "RE: MW for 6-ppd quinone"
dat[units == 'uM', conc_in_uM := conc]
dat[units == 'ug/mL', conc_in_uM := conc*1000/mol_weight_g_per_mol]


# FINAL DATA CHECKS -------------------------------------------------------------
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Check for the expected number of technical replicates
dat[wllt == 't', .(length(unique(paste0(apid,rowi,coli)))), by = .(spid, conc)][V1 != 3]
# do you except these cases to have more than or less than 3 replicates?
# Were some samples repeated, and only certain repeats meant to be included?

# Any other plots or things to check?


# Other checks ------------------------------------------------------------

# > Confirming DIV 12 estimated only for affected wells on plate ----------

# Of the AUC endpoints, which wells from this plate relied on estimated values?
dat[!grepl('DIV',acsn) & !grepl('(LDH)|(AB)',acsn) & grepl('estimated',wllq_notes), .N, by = .(apid, rowi, coli)][order(rowi, coli)]
#                  apid rowi coli  N
# 1: 20210818_MW75-9207    5    1 17
# 2: 20210818_MW75-9207    6    1 17
# cool, just these 2 wells

# And the DIV12?
dat[grepl('DIV12',acsn) & grepl('estimated',wllq_notes), .N, by = .(apid, rowi, coli)][order(rowi, coli)]
# apid rowi coli  N
# 1: 20210818_MW75-9207    5    1 17
# 2: 20210818_MW75-9207    6    1 17

# Note that wells F7 and F8 also contaminated on DIV12, 
# but wllq set to 0 for all DIV because of precipitate (so we did not estimate any values.)
# dat[apid == '20210818_MW75-9207' & rowi == 6 & coli %in% c(7,8), .N, by = .(apid, rowi, coli, wllq, wllq_notes)]
#                  apid rowi coli wllq                                                                        wllq_notes  N
# 1: 20210818_MW75-9207    6    7    0 compound precipitated out of dosing solution at this concentration; contamination  2
# 2: 20210818_MW75-9207    6    8    0 compound precipitated out of dosing solution at this concentration; contamination  2
# 3: 20210818_MW75-9207    6    7    0              compound precipitated out of dosing solution at this concentration;  85
# 4: 20210818_MW75-9207    6    8    0              compound precipitated out of dosing solution at this concentration;  85



# > Confirm all wllq assignments ------------------------------------------

# Confirm I identified the correct wells for the precipitate based on compound/conc
dat[grepl('precipitate',wllq_notes), .N, by = .(treatment, conc, wllq_notes)]
#                 treatment conc                                                                        wllq_notes   N
# 1: 5,5-Diphenylhydantoin  100                compound precipitated out of dosing solution at this concentration   6
# 2: 5,5-Diphenylhydantoin  100              compound precipitated out of dosing solution at this concentration;  255
# 3: 5,5-Diphenylhydantoin  300                compound precipitated out of dosing solution at this concentration   4
# 4: 5,5-Diphenylhydantoin  300 compound precipitated out of dosing solution at this concentration; contamination   2
# 5: 5,5-Diphenylhydantoin  300              compound precipitated out of dosing solution at this concentration;  255
# 6: 5,5-Diphenylhydantoin 1000                compound precipitated out of dosing solution at this concentration   4
# 7: 5,5-Diphenylhydantoin 1000 compound precipitated out of dosing solution at this concentration; contamination   2
# 8: 5,5-Diphenylhydantoin 1000              compound precipitated out of dosing solution at this concentration;  255
# yep, looks good.

# Confirm all looks okay
library(stringi)
dat[grepl('DIV',acsn), endpoint_type := 'DIV']
dat[grepl('(LDH)|(AB)',acsn), endpoint_type := 'cytotox']
dat[is.na(endpoint_type), endpoint_type := 'AUC']
dat[wllq == 0, .(length(unique(acsn))), by = .(apid, rowi, coli, wllq_notes)]
View(dat[wllq == 0, .(num_AUC_endpoints = length(unique(acsn[endpoint_type == 'AUC'])),
                      num_DIV_endpoints = length(unique(acsn[endpoint_type == 'DIV'])),
                      num_cyto_endpoints = length(unique(acsn[endpoint_type == 'cytotox']))), by = .(apid, rowi, coli, wllq_notes)][order(apid, rowi, coli)])
# My note re 5% CO2 didn't get saved for DIV 12... ugh I see where need to fix this
# But would take a minute

# Also "contamination"... that note didn't get saved either where there was an additional wllq note for the LTB and CTB
# For the sake of time, I'm just going to merge that in now
# Since this is an issue of a wllq note, not the wllq itself.

# Note that for MW75-9207 E1 and F1, only the CTB adn LDH are affected,
# because I estimated MEA values for the DIV 12 to replace the real values where there was a contamination.

# Adding in the 5% CO2 note
dat[apid == '20210818_MW75-9206' & grepl('DIV12',acsn), wllq_notes := paste0(wllq_notes,'5% CO2 ran out during recording DIV12; ')]
dat[apid == '20210818_MW75-9206' & endpoint_type == 'AUC', wllq_notes := paste0(wllq_notes,'5% CO2 ran out during recording DIV12; ')]
dat[apid == '20210818_MW75-9207' & grepl('DIV12',acsn), wllq_notes := paste0(wllq_notes,'5% CO2 ran out during recording DIV12; ')]
dat[apid == '20210818_MW75-9207' & endpoint_type == 'AUC', wllq_notes := paste0(wllq_notes,'5% CO2 ran out during recording DIV12; ')]

# Adding contamination info
wllq_info <- as.data.table(read.csv('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTFalseNegatives/wells_with_well_quality_zero.csv'))
wllq_info <- wllq_info[grepl('contamination',wllq_notes)]
wllq_info[, `:=`(coli = as.numeric(sub("[[:alpha:]]","",well)), rowi = match(sub("[[:digit:]]","",well), LETTERS), apid = paste0(date,'_',Plate.SN))]
wllq_info
# date  Plate.SN DIV well wllq    wllq_notes affected_endpoints coli rowi
# 1: 20210818 MW75-9207  12   E1    0 contamination        mea,CTB,LDH    1    5
# 2: 20210818 MW75-9207  12   F1    0 contamination        mea,CTB,LDH    1    6
# 3: 20210818 MW75-9207  12   F7    0 contamination        mea,CTB,LDH    7    6
# 4: 20210818 MW75-9207  12   F8    0 contamination        mea,CTB,LDH    8    6
check.wells <- wllq_info[, .(apid, rowi, coli)]
setkey(dat, apid, rowi, coli)
# Remember, this only affects the LDH and CTD, because I can estimate MEA values to fill in missing DIV
dat[.(check.wells)][grepl('(LDH)|(AB)',acsn), .N, by = .(rowi, coli, wllq, wllq_notes)]
# Update wllq_ntoe where needed
dat[apid == '20210818_MW75-9207' & grepl('(LDH)|(AB)',acsn) & rowi == 6 & coli == 7, wllq_notes := paste0(wllq_notes,'contamination DIV12; ')]
dat[apid == '20210818_MW75-9207' & grepl('(LDH)|(AB)',acsn) & rowi == 6 & coli == 8, wllq_notes := paste0(wllq_notes,'contamination DIV12; ')]



# > View where CO2 turned off, confirm these okay -------------------------

# Let's just compare the controls

library(ggplot2)
dat[, CO2_off_DIV12 := as.numeric(grepl('5% CO2 ran out',wllq_notes))]
dat[, .N, by = .(CO2_off_DIV12, apid)]

plotdat <- dat[grepl('firing_rate_mean_DIV12$',acsn)]

ggplot(plotdat[wllt == 'n'], aes(x = apid, y = rval)) +
  geom_jitter(aes(shape = factor(wllq), color = CO2_off_DIV12), width = 0.2, height = 0)+
  geom_boxplot(fill = 'transparent', outlier.shape = NA)+
  ggtitle('Comparison of Control wells by Plate, Mean Firing Rate DIV12')

plotdat <- dat[grepl('active_electrodes_number_DIV12$',acsn)]
ggplot(plotdat[wllt == 'n'], aes(x = apid, y = rval)) +
  geom_jitter(aes(shape = factor(wllq), color = CO2_off_DIV12), width = 0.2, height = 0)+
  geom_boxplot(fill = 'transparent', outlier.shape = NA)+
  ggtitle('Comparison of Control wells by Plate, # Active Electrodes DIV12')

plotdat <- dat[grepl('mutual_information_norm_DIV12$',acsn)]
ggplot(plotdat[wllt == 'n'], aes(x = apid, y = rval)) +
  geom_jitter(aes(shape = factor(wllq), color = CO2_off_DIV12), width = 0.2, height = 0)+
  geom_boxplot(fill = 'transparent', outlier.shape = NA)+
  ggtitle('Comparison of Control wells by Plate, Normalized Mutual Information DIV12')

# So I am seeing a real shift here between the first plate and teh other 2 plates.
# BUT, I think I need ot see this in a broader context to see if this magnitude of shift is significant


# save dat and graphs
dat[, endpoint_type := NULL]
dat[, CO2_off_DIV12 := NULL]
setkey(dat, NULL)
save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")
