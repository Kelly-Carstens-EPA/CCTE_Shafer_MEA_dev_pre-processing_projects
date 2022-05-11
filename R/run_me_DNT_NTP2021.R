rm(list=ls()) # clear environment
graphics.off() # clear plot history
# ------------------------------------------------------------------------ #
# USER INPUT
# ------------------------------------------------------------------------ #
dataset_title <- "DNT_NTP2021" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName <- "DMSO" # all compounds other than those listed below should have this vehicle control

spidmap_file <- "L:/Lab/NHEERL_Mundy/Project - DNT Test Set 2021 NTP/Coded Plate Map - Plate No. 4001207126 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx"
spid_sheet <- "Chemical List"
# May need to add this spid sheet as well:
# "Coded Plate Map - Plate No. 7000439163 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx"
# treatments in MEA data apppear at last 4 digits of plate + Plate position

project.dir <- "L:/Lab/NHEERL_MEA/Project - DNT_NTP_2021" # project main folder (where will look for README files)
scripts.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/nfa-spike-list-to-mc0-r-scripts/R" # update to the folder where the scripts are located
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl" # where the dataset_title folder will be created

update_concs_without_prompt <- FALSE
# ------------------------------------------------------------------------ #
# END USER INPUT
# ------------------------------------------------------------------------ #

library(data.table)
library(openxlsx)
library(RMySQL)


# Run main steps ----------------------------------------------------------

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

# Scan for readme's that might affect dosing, wllq
txt.files <- list.files(project.dir, pattern = '\\.txt', recursive = T, full.names = T)
readmes <- txt.files[grepl('read( )*me',tolower(txt.files))]
for (readme in readmes) {
  cat(dirname(readme),'\n')
  cat(scan(readme, what = character(), sep = '\n', quiet = T), sep = '\n')
  cat('\n')
}
length(readmes) # 21
readmes.list <- lapply(readmes, function(readme) scan(readme, what = character(), sep = '\n', quiet = T))
library(stringi)
# removing the string that apppears in every file
readmes.list.unique <- lapply(readmes.list, 
                              function(readme) setdiff(readme, 
                                                       c("The following wells have been discarded from the Alamar Blue assay due to returning results outside the acceptable bounds of the assay:",
                                                         "X",
                                                         "The following wells have been discarded from the LDH assay due to returning results outside the acceptable bounds of the assay:")))
names(readmes.list.unique) <- basename(dirname(readmes))
readmes.list.unique[which(lapply(readmes.list.unique, length) != 0)]
# $`20210915_NFA_DNT 2021_Group 1`
# [1] "7126 A3 prrecipitates in media (need to be repeated)"
# 
# $`20210915_NFA_DNT 2021_Group 2`
# [1] "7126 A12 precipitates in media, need to be repeated on lower concentration"
# [2] "7126 A9 turns the media yellow - may be acidic"
# 
# $`20210929_NFA_DNT 2021_Group 3`
# [1] "Chemical 7126 B2 precipitates in media, had a different dilution scheme"
# [2] "-5ul innto 495ul media, then 50ul into 450ul media in well"
# [3] "will be repeated with lower concentration"
# 
# $`20210929_NFA_DNT 2021_Group 4`
# [1] "Chemical 7126 B11 precipitates in media, had a different dilution scheme"
# [2] "-5ul innto 495ul media, then 50ul into 450ul media in well"
# [3] "Chemical 7126 B12 is yellow in color."
# 
# $`20211027_NFA_DNT 2021_Group 20`
# [1] "7126 H10 will be repeated on a lower concentration"
# 
# $`20211110_NFA_DNT 2021_Group 6`
# [1] "7126 C11 will be repeated on a lower concentration"
# 
# $`20211124_NFA_DNT 2021_Group 7`
# [1] "Chemical 7126 D4 and 7126 D5 will be repeated at a lower concentration."
# [2] "Column 4 and 5 on Plate 1 (78-6214) are switched - dosing error (Plate Map is updated)"
# 
# $`20211124_NFA_DNT 2021_Group 8`
# [1] "X All wells from plate 78-6217"                         "-Reasoning : CTB incubation time is less than one hour"
# [3] "7126 D7 need to be repeated on a lower concentration"
# 
# $`20211208_NFA_DNT 2021_Group 9`
# [1] "7126 E3 may be needed to be repeated on a lower concentration "
# 
# $`20220119_NFA_DNT 2021_Group 11`
# [1] "7126 F1, 7126 F2 need to be repeated on a lower concentration "
# 
# $`20220119_NFA_DNT 2021_Group 12`
# [1] "7126 F8 need to be repeated on a lower concentration"           "7126 F12 may be needed to be repeated on a lower concentration"
# 
# $`20220223_NFA_DNT 2021_Group 13`
# [1] "7126 G3 may need to be repeated at a lower concentration."
# 
# $`20220316_NFA_DNT 2021_Group 16`
# [1] "7126 H7 needs to be repeated (dosing error) "
# 
# $`20220330_NFA_DNT 2021_Group 19`
# [1] "X MW7118 A8 (cell debris)"

# $`20220413_NFA_DNT 2021_Group 18`
# [1] "X 78-7205 Well D7 (cell debris)"


# > Automated way to get files --------------------------------------------
source(file.path(scripts.dir, 'gather_files_functions.R'))

get_NFA_standard_structue <- function(culture.folderi) {
  cyto.files <- list.files(path = culture.folderi, pattern = '(Calculations)|(Summary)', full.names = T)
  plate.dirs <- list.files(path = culture.folderi, pattern = '^(MW)*[0-9\\-]{7}$', full.names = T)
  mfiles <- list.files(path = culture.folderi, pattern = 'MaestroExperimentLog', full.names = T, recursive = T)
  slists <- list.files(path = culture.folderi, pattern = '_spike_list\\.csv', full.names = T, recursive = T)
  filesi <- c(cyto.files, mfiles, slists)
  return(filesi)  
}

culture.folders <- list.files(path = project.dir, pattern = "[0-9]{8}", full.names = T, recursive = F)
all.files <- c()
for (culture.folderi in culture.folders) {
  all.files <- c(all.files, get_NFA_standard_structue(culture.folderi))
}

#Basic cleaning
all.files <- Filter(function(filei) !grepl('\\Q~$\\E',filei), all.files)
all.files <- Filter(function(filei) !grepl('deprecated',tolower(filei)), all.files)
length(all.files)
# should be 20 groups * (1 Calc file + 3 plates * (4 DIV + 1 maestro exp log file))
20*(1+3*(4+1))
# [1] 320
length(all.files)
# 320, cool!

# Save the files log
writeLogFile(all.files, output.dir = file.path(root_output_dir, dataset_title), dataset_title, files_type = '')
# Writing 320 files to DNT_NTP2021_files_log_2022-05-10.txt ...
# [1] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNT_NTP2021/DNT_NTP2021_files_log_2022-05-10.txt is ready."
rm(all.files, culture.folders, readmes, readmes.list, readmes.list.unique)

# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)
# cytotox data does not have units. Will fill with NA
# long-format data is ready.



# Updated treatment label for solvent control wells ------------------------------------
dat[, treatment_srcf := treatment]
dat[wllt == 'n', .N, by = .(treatment)] # wllt == 'n' determined where conc_original == 0
# Note: "Water" is currently used for other MEA NFA data
# So will change H2O to "Water" for consistency
dat[treatment == 'H2O', treatment := 'Water']

dat[wllt == 'n', .N, by = .(treatment, conc)][N > 6][order(-N)]
#        treatment conc     N
# 1:          DMSO    0 26265
# 2:           Water    0  1275
# 3:       7126 A3    0    12
# 4:   Bisphenol A    0    12
# 5: Acetamenophin    0    12
# Wait, why only 12 points for some of these? Shouldn't there be 1 for every plate adn acsn?
dat[wllt == 'n' & treatment %in% c('7126 A3','Bisphenol A','Acetamenophin'), .N, by = .(treatment, acsn)]
# treatment                    acsn N
# 1:       7126 A3  CCTE_Shafer_MEA_dev_AB 6
# 2:   Bisphenol A  CCTE_Shafer_MEA_dev_AB 6
# 3: Acetamenophin  CCTE_Shafer_MEA_dev_AB 6
# 4:       7126 A3 CCTE_Shafer_MEA_dev_LDH 6
# 5:   Bisphenol A CCTE_Shafer_MEA_dev_LDH 6
# 6: Acetamenophin CCTE_Shafer_MEA_dev_LDH 6
dat[wllt == 'n' & !grepl('(LDH)|(AB)',acsn), .N, by = .(treatment)]
# treatment     N
# 1:      DMSO 26265
# 2:       Water  1275
# ah okay! So in the maestro log files, Seline entered the control treatment name
# whereas in the past and now still for the cyto assays, the control wells are just indicated by a conc of 0
# but the treatment name for the given row is used

# For which chemicals is implied that Water was used, based on Water is same-row control column?
water.plate.rows <- dat[treatment == 'Water', unique(.SD), .SDcols = c('apid','rowi','srcf')]
setkey(dat, apid, rowi, srcf)
dat[.(water.plate.rows)][wllt != 'n', .N, by = .(treatment)]
dat[.(water.plate.rows)][wllt != 'n', .N, by = .(treatment)]
# treatment    N
# 1:   7126 H8 1785
# 2:   7126 H9 1785
# 3:  7126 H10 1785
# 4:  7126 H11 1785
# 5:  7126 G10 1785
treatments.same.row.water <- dat[.(water.plate.rows)][wllt != 'n', unique(treatment)]
check.treatment.rows <- dat[treatment %in% treatments.same.row.water, unique(.SD), .SDcols = c('apid','rowi','srcf')]
dat[J(check.treatment.rows)][wllt == 'n', .N, by = .(treatment)] # confirm all of these are Water, or same chem name as for LDH/AB
# treatment    N
# 1:   7126 H8    6
# 2:       Water 1275
# 3:   7126 H9    6
# 4:  7126 H10    6
# 5:  7126 H11    6
# 6:  7126 G10    6
# yep, looks okay!

# Standarize control treatment names for LDH/AB
dat[wllt == 'n' & treatment %in% treatments.same.row.water, treatment := 'Water']
dat[wllt == 'n' & !(treatment %in% treatments.same.row.water), treatment := 'DMSO']

# Should all of these treatments be default_ControlTreatmentName?
# If so, use below:
# dat[wllt == "n", treatment := default_ControlTreatmentName]
# Can manually update other wells where control treatment is not the default, or use teh function below
# dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "")

# Set the control well concentration. Adjust as needed
dat[wllt == "n", conc := 0.001]


# Wllq updates by treatment -----------------------------------------------

## ** implement wllq notes found in readme's by treatment!! 
# (maybe do this after finalize conc's/spids? We'll see...)
dat[, culture_date := sub('_.*$','',apid)]

dat[culture_date == '20210915' & treatment == "7126 A3", 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),
                             '7126 A3 prrecipitates in media (need to be repeated)'))]

dat[culture_date == '20210915' & treatment == '7126 A12',
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 A12 precipitates in media, need to be repeated on lower concentration"))]
dat[culture_date == '20210915' & treatment == '7126 A9', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 A9 turns the media yellow - may be acidic"))]

dat[culture_date == '20210929' & treatment == '7126 B2', 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),
                             "Chemical 7126 B2 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well\nwill be repeated with lower concentration"))]
dat[culture_date == '20210929' & treatment == '7126 B11', 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"Chemical 7126 B11 precipitates in media, had a different dilution scheme\n-5ul innto 495ul media, then 50ul into 450ul media in well"))]
dat[culture_date == '20210929' & treatment == '7126 B12', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')), "Chemical 7126 B12 is yellow in color."))]

dat[culture_date == '20211027' & treatment == '7126 H10', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 H10 will be repeated on a lower concentration"))]

dat[culture_date == '20211110' & treatment == '7126 C11', 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 C11 will be repeated on a lower concentration"))]


dat[culture_date == '20211124' & treatment %in% c('7126 D4','7126 D5'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"Chemical 7126 D4 and 7126 D5 will be repeated at a lower concentration."))]

dat[culture_date == '20211124' & treatment %in% c('7126 D7'),
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 D7 need to be repeated on a lower concentration"))]

dat[culture_date == '20211208' & treatment %in% c('7126 E3'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 E3 may be needed to be repeated on a lower concentration "))]
dat[culture_date == '20220119' & treatment %in% c('7126 F1','7126 F2'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F1, 7126 F2 need to be repeated on a lower concentration "))]
dat[culture_date == '20220119' & treatment %in% c('7126 F8'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F8 need to be repeated on a lower concentration"))]
dat[culture_date == '20220119' & treatment %in% c('7126 F12'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 F12 may be needed to be repeated on a lower concentration"))]

dat[culture_date == '20220223' & treatment %in% c('7126 G3'), 
    `:=`(wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 G3 may need to be repeated at a lower concentration."))]

dat[culture_date == '20220316' & treatment %in% c('7126 H7'), 
    `:=`(wllq = 0,
         wllq_notes = paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),"7126 H7 needs to be repeated (dosing error) "))]


# consistent implementation of what's wllq 0, what's jsut a note?
View(dat[wllq_notes != '', .N, by = .(wllq, wllq_notes)][order(wllq_notes, wllq)])
View(dat[wllq_notes != '', .N, by = .(treatment, wllq, wllq_notes)][order(wllq_notes, wllq)])

# general rule: preciptate -> wllq == 0
# dosing error, cell debris -> wllq == 0
# need to repeat at lower conc -> note, but wllq still == 1
# note about color of chemical/solution -> note, but wllq still == 1

# Do some checks to confirm I entered all readme ntoes (soem counts?)
# confirmed that all notes in readme's have been implemented.

# Save a copy for now, so can evaluate what needs to berepeated

setkey(dat, NULL)
description <- 'Saving a preliminary version of DNT NTP 2021
So that we can evalute which chemicals need to be repeated.
What is left to do:
- Confirm that all concentrations have been corrected to the exact conc consistently
- convert all concentration units to uM
- General data set checks (for NA, expected # of wells, etc)
Date Ran: May 10 2022'
save(dat, description, file = file.path(root_output_dir,dataset_title,'output','DNT_NTP2021_preliminary_longfile.RData'))


# SKIPPING THIS FOR NOW
# 
# # Assign sample ID's -------------------------------------------------------------
# spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
# head(spidmap)
# # 2nd col name is "Conc..(mM)"
# # unique(spidmap$Concentration_Unit) # all mM?
# unique(dat$units) # confirm these are all uM (this taken from maestroexperiment log file)
# 
# # Kathleen said that she used the last 4 digits of the plate No plus the plate position as the treatment names
# basename(spidmap_file) # "Coded Plate Map - Plate No. 4001207126 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx"
# spidmap[, treatment := paste0('7126 ',Plate.Position)]
# # don't have spids get, just the blind codes
# # setnames(spidmap, old = c(trt_col, spid_col), new = c("treatment","spid"))
# # for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
# spidmap[, expected_stock_conc := round(as.numeric(`Conc..(mM)`), digits = 0)]
# spidmap[is.na(expected_stock_conc)]
# # spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
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


# # Confirm Conc's ----------------------------------------------------------------
# # confirm that the conc's collected from master chem lists and Calc files match
# # and that the correct concentration-corrections has been done for each compound
# dat[, conc_srcf := conc] # save the original conc's in a column
# 
# # before dive in too much further, let's confirm consistent treatment labels by wells
# dat[, .(length(unique(treatment))), by = .(apid, rowi, coli)][V1 > 1]
# # empty, good
# 
# # check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# # Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
# dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# # if any, standardize those before continuing.
# problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
# problem_comps
# # [1] "7126 D3"  "7126 D1"  "7126 D2"  "7126 E11" "7126 G7" 
# check.rows <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# setkey(check.rows, treatment, apid, rowi, coli)
# setkey(dat, treatment, apid, rowi, coli)
# View(dat[J(check.rows)][, .N, by = .(treatment, apid, rowi, coli, conc, srcf)][order(treatment, rowi, coli, conc)])
# 
# # Okay, I think the maestro exp log might actually be off in some of these cases
# dat[, max_conc_by_srcf  := max(conc, na.rm = T), by = .(treatment, srcf, apid)]
# dat[treatment %in% problem_comps, .N, by = .(treatment, max_conc_by_srcf, srcf)]
# setkey(check.rows, apid, rowi, coli)
# setkey(dat, apid, rowi, coli)
# dat[J(check.rows)][, .N, by = .(apid, rowi, coli, treatment)][order(apid, rowi, coli)]
# 
# # so, it seems like there is an issue with the assignment of the conc's for in the maestro log files
# # Let's compare the max conctested with the spidmap file
# # as a third-party source of what's true
# # Do I need any add'l spidmaps?
# setdiff(dat$treatment, spidmap$treatment)
# # [1] "DMSO"          "Bisphenol A"   "Acetamenophin" "9163 A1"       "9163 A2"       "9163 A3"       "9163 A4"       "9163 A5"       "9163 B6"       "9163 B7"       "9163 B8"  
# 
# # Update conc's
# # Then consider how to prevent this in future, make things easier for everyone?
# 
# 
# # finally, run this:
# source(file.path(scripts.dir, 'confirm_concs.R'))
# con <- dbConnect(drv = RMySQL::MySQL(), user = "", pass = "", dbname='',host = "")
# dat <- confirm_concs(dat, spidmap, con, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30), update_concs_without_prompt = update_concs_without_prompt)
# dbDisconnect(con)
# 
# 
# 
# # Check conc units --------------------------------------------------------
# 
# dat[, .N, by = .(units)]
# # any not in uM? If so, convert to uM
# 
# 
# 
# # FINAL DATA CHECKS -------------------------------------------------------------
# # this section is to confirm that the data has been processed correctly
# source(file.path(scripts.dir, 'dataset_checks.R'))
# dataset_checks(dat)
# 
# # Check for the expected number of technical replicates
# dat[wllt == 't', .(length(unique(paste0(apid,rowi,coli)))), by = .(spid, conc)][V1 != 3]
# # do you except these cases to have more than or less than 3 replicates?
# # Were some samples repeated, and only certain repeats meant to be included?
# 
# # Any other plots or things to check?
# 
# # save dat and graphs
# setkey(dat, NULL)
# save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
# rm(dat)
# 
# if(save_notes_graphs) {
#   sink() # close the txt log file
#   graphics.off() # clear the plot history
# }
# 
# cat("\nDone!\n")
