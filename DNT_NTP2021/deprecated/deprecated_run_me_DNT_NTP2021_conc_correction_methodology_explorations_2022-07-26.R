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

get_new_files_log <- FALSE
# ------------------------------------------------------------------------ #
# END USER INPUT
# ------------------------------------------------------------------------ #


library(data.table)
library(openxlsx)
library(RMySQL)
library(stringi)


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
length(readmes) # 25
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
# 
# $`20220601_NFA_PFAS+ G3 & DNT Reps 1`
# [1] "X Plate 78-7217 for 7126 C11 0.00015 uM"
# 
# $`20220615_NFA_DNT 2021_Reps 3`
# [1] "X 78-7220 A1, 78-7220 B1, 78-7220 D4"


# > Automated way to get files --------------------------------------------
if (get_new_files_log) {
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
}

# run the main steps
source(file.path(scripts.dir, 'source_steps.R'))
rm(list = setdiff(ls(),c('dataset_title','pause_between_steps','save_notes_graphs','default_ControlTreatmentName','spidmap_file','spid_sheet','project.dir','scripts.dir','root_output_dir','update_concs_without_prompt','get_new_files_log')))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)
# cytotox data does not have units. Will fill with NA
# long-format data is ready.


# Updated treatment label for solvent control wells ------------------------------------
dat[, treatment_srcf := treatment]
dat[wllt == 'n', .N, by = .(treatment)][order(-N)] # wllt == 'n' determined where conc_original == 0
# Note: "Water" is currently used in other MEA NFA data in ToxCast
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
dat[wllt == 'n', treatment := ifelse(treatment %in% c('Water',treatments.same.row.water), 'Water', 'DMSO')]

# Note the solvent control sued for each substance
dat[wllt == 't', solvent := ifelse(treatment %in% treatments.same.row.water, 'Water', 'DMSO')]

# Set the control well concentration. Adjust as needed
# dat[wllt == "n", conc := 0.001]
dat[wllt == "n", conc := 0.1]  # previously I entered this as 0.001, as a fraction, but now I"m going to enter it as 0.1 with units "%"
dat[wllt == "n", units := '%']  # previously I entered this as 0.001, as a fraction, but now I"m going to enter it as 0.1 with units "%"



# Assign sample ID's -------------------------------------------------------------
# We don't have sample id's actually, instead I'm goign to assign the blind code
spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
head(spidmap)
# 2nd col name is "Conc..(mM)"
# unique(spidmap$Concentration_Unit) # all mM?
unique(dat$units) # confirm these are all uM (this taken from maestroexperiment log file)

# Kathleen said that she used the last 4 digits of the plate No plus the plate position as the treatment names
basename(spidmap_file) # "Coded Plate Map - Plate No. 4001207126 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx"
spidmap[, treatment := paste0('7126 ',Plate.Position)]
# don't have spids get, just the blind codes
# setnames(spidmap, old = c(trt_col, spid_col), new = c("treatment","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, sheet_stock_conc_units := 'mM']
spidmap[is.na(as.numeric(`Conc..(mM)`))]
spidmap[, conc_numeric_test := as.numeric(`Conc..(mM)`)]
spidmap[is.na(conc_numeric_test)]

# Remove rows that just contain notes, not chemical info
notes.tb <- spidmap[is.na(Plate.Position)]
spidmap <- spidmap[!is.na(Plate.Position)]

# extract the footnotes in another column
# then remove from main columns
spidmap[, footnote_chars := paste0(gsub('[0-9a-zA-Z \\.]*','',`Blind-Code`), ',', gsub('[0-9a-zA-Z \\.]*','',`Conc..(mM)`), ',', gsub('[0-9a-zA-Z \\.]*','',Plate.Position))]
spidmap[, c('Blind-Code','Conc..(mM)','Plate.Position') := lapply(.SD, function(coli) stri_extract(coli, regex = '[0-9a-zA-Z \\.]+')), .SDcols = c('Blind-Code','Conc..(mM)','Plate.Position')]
spidmap[, conc_numeric_test := as.numeric(`Conc..(mM)`)]
spidmap[is.na(conc_numeric_test)]
# empty, cool!

spidmap[, treatment := as.character(treatment)]
setnames(spidmap, old = 'Blind-Code', new = 'DNTP_blind_code')
setnames(spidmap, old = 'Conc..(mM)', new = 'sheet_stock_conc')


# Add additional spidmap's if needed and rbind into 1 spidmap
setdiff(dat$treatment, spidmap$treatment)
spidmap2 <- as.data.table(read.xlsx('L:/Lab/NHEERL_Mundy/Project - DNT Test Set 2021 NTP/Coded Plate Map - Plate No. 7000439163 DNT HEI EPA SHIP CHEM14214 MRI3110.xlsx', sheet = spid_sheet))
spidmap2[, treatment := paste0('9163 ',Plate.Position)]
spidmap2[, conc_numeric_test := as.numeric(`Conc..(mg/mL)1`)]
spidmap2[, sheet_stock_conc_units := 'mg/mL']
spidmap2[is.na(conc_numeric_test)] # several...
notes.tb <- rbind(notes.tb, spidmap2[is.na(Plate.Position)], fill = T) # remove rows that just contain notes
spidmap2 <- spidmap2[!is.na(Plate.Position)] # remove rows that just contain notes
spidmap2[, footnote_chars := paste0(gsub('[0-9a-zA-Z \\.]*','',`Blind-Code`), ',', gsub('[0-9a-zA-Z \\.]*','',`Conc..(mg/mL)1`), ',', gsub('[0-9a-zA-Z \\.]*','',Plate.Position))]
spidmap2[, c('Blind-Code','Conc..(mg/mL)1','Plate.Position') := lapply(.SD, function(coli) stri_extract(coli, regex = '[0-9a-zA-Z \\.]+')), .SDcols = c('Blind-Code','Conc..(mg/mL)1','Plate.Position')]
spidmap2[, conc_numeric_test := round(as.numeric(`Conc..(mg/mL)1`), digits = 0)]
spidmap2[is.na(conc_numeric_test)] # empty now, cool
spidmap2[, treatment := as.character(treatment)]
setnames(spidmap2, old = 'Blind-Code', new = 'DNTP_blind_code')
setnames(spidmap2, old = 'Conc..(mg/mL)1', new = 'sheet_stock_conc')

# Combine spidmaps
spidmap <- rbind(spidmap, spidmap2)
# Manually add assay controls to spidmap
spidmap <- rbind(spidmap, data.table('treatment' = c('Bisphenol A','Acetaminophen'),
                                     'DNTP_blind_code' = c('Bisphenol A','Acetaminophen')), fill = T)
setdiff(dat$treatment, spidmap$treatment)
# [1] "DMSO"          "Water"         "Acetamenophin" "7258 G01"      "7258 G1"       "7258 G02"      "7258 G2"       "7258 H01"      "7258 H1"      
# Correct spelling of assay control in data
dat[treatment %in% c('Acetamenophin'), treatment := 'Acetaminophen']
setdiff(dat$treatment, spidmap$treatment)
# [1] "DMSO"     "Water"    "7258 G01" "7258 G1"  "7258 G02" "7258 G2"  "7258 H01" "7258 H1" 
# huh, I though there were just 3 compounds from the PFAS Plus up set tested on these plates...
dat[!treatment %in% c(spidmap$treatment, c('DMSO','Water')), .N, by = .(treatment, apid)]
check.apids <- dat[!treatment %in% c(spidmap$treatment, c('DMSO','Water')), unique(apid)]
dat[apid %in% check.apids & wllt == 't', .N, by = .(treatment, apid)][order(apid, treatment)]
# wait, why some plates have 9 unique treatments?
# Oh, gotcha, this is probably a difference in how the treatments are represented (e.g. G01 vs G1)
# So I can safely assume that all treatments that begin with 7258 are some modification of the PFAS Plus up spid, adn cna be removed from this data set

# Is NTP going to want to use the 1st and 2nd lowest conc to normalize the data?
# Or are they going to use our methods, which only uses the control wells?
# I believe that they are going to try to mimic our methods. But I can add a note in case not.
dat <- dat[treatment %in% c(spidmap$treatment, c('DMSO','Water'))]

# Triage notes below for incorporation/needed checks, mostly to confirm solvent used and stock conc
notes.tb[, .(`Blind-Code`)]
# Blind-Code
# 1: Note: Blank DMSO, CAS No: 67-68-5, Supplier: Gaylord Chemical Company LLC. Lot # USP180404, CoA Purity: 100.0%, Amount: 50 mL, Suggested Storage: Ambient
# 2:                  Note: Blank Water, CAS No: 7732-18-5, Supplier: Labconco Water Pro PS. 18.2 M<U+2126>·cm, Amount: 50 mL, Suggested Storage: Refrigerate 
# 3:                                                                              Note: Argon gas headspace added to chemical vials prior to storage/shipment.
# 4:                           Vial Type: polypropylene, 1.4-mL alphanumeric screw-cap tubes, 96-vial plate rack, polypropylene screw-cap with silicon o-ring.
# 5:                                                                                                                a Water solution; max DMSO solubility 2 mM
# 6:                                                                                                                       ß Water solution; insoluble in DMSO
# 7:                                                                                                        <U+03B3> Water solution; max DMSO solubility 10 mM
# 8:                                                                                                                         <U+0394> Cholinesterase Inhibitor
# 9:                                                                              Note: Argon gas headspace added to chemical vials prior to storage/shipment.
# 10:                           Vial Type: polypropylene, 1.4-mL alphanumeric screw-cap tubes, 96-vial plate rack, polypropylene screw-cap with silicon o-ring.
# 11:                                                                                                         1 Complex mixture in DMSO, concentration in mg/mL
# 12:                                                                                                           ** Exact concentration could not be determined.

# translating this smoothly is getting tricky with the non-standard symbols (alpha, beta, delta, etc)
# Opening up spidmap 1 in excel, looks like:
# H8 = Water solution; max DMSO solubility 2 mM
# H9, H10 = Water solution; insoluble in DMSO
# H11 = Water solution; max DMSO solubility 10 mM
# Opening spidmap2: "1 Complex mixture in DMSO, concentration in mg/mL" applies to the entire column "Conc..(mg/mL)1"
# "Exact concentration could not be determined." applies to the 9 instances wherever "**" appears in conc column
# Conc for these substances is actually "<= 10"
spidmap[, .N, by = .(footnote_chars)]
spidmap[, footnote_chars := sub('^,', '', footnote_chars)]
spidmap[, footnote_chars := sub(',*$', '', footnote_chars)]
spidmap[, .N, by = .(footnote_chars)]

# from spidmap1:
spidmap[treatment %in% paste0("7126 H",c(8:11)), .(treatment, footnote_chars)]
# treatment footnote_chars
# 1:   7126 H8              a
# 2:   7126 H9              ß
# 3:  7126 H10              ß
# 4:  7126 H11       <U+03B3>
# cool, that lines up with findings
spidmap[treatment == '7126 H8', footnote := 'Water solution; max DMSO solubility 2 mM']
spidmap[treatment %in% c('7126 H9','7126 H10'), footnote := 'Water solution; insoluble in DMSO']
spidmap[treatment == '7126 H11', footnote := 'Water solution; max DMSO solubility 10 mM']

# From spidmap2
spidmap[grepl('\\*\\*',footnote_chars)] # yep, 9 cases, all from plate 9163
spidmap[grepl('\\*\\*',footnote_chars), footnote := 'Coded Plate Map stock conc <= 10. Exact concentration could not be determined.']

# All footnotes addressed?
spidmap[!is.na(footnote_chars) & footnote_chars != '' & is.na(footnote)]
spidmap[!is.na(footnote_chars) & footnote_chars != '' & is.na(footnote), .N, by = .(footnote_chars)]
#    footnote_chars  N
# 1:       <U+0394> 21
# ah, all of these are footnote char <U+0394>
# which corresponds to a note the that substances is a cholinesterase inhibitor. I dont' need to note that here
# but might as well for completeness
spidmap[grepl('U\\+0394',footnote_chars)] # this doesn't work... not sure how to reference!
spidmap[!is.na(footnote_chars) & footnote_chars != '' & is.na(footnote), footnote := 'Cholinesterase Inhibitor']

# How will I address these notes?
spidmap[, .N, by = .(footnote)]
# Confirm that substances with note of water solution match the solvent I assigned
# where exact conc couldn't be determined - I feel compelled to report that in the well quality, but they should know that
# will implement below after merge in spidmap

# Visually confirm the conc's look correct, that nothing weird happened with extracting the true values
View(spidmap)

# check if every treatment name from the mea data maps to a unique sample in spidmap
setdiff(dat$treatment, spidmap$treatment) # "DMSO"          "Water"  -> need to update spellign for acetaminophen
setdiff(dat$treatment, spidmap$treatment) # "DMSO"          "Water" 
spidmap[treatment %in% unique(dat$treatment), .N, by = .(treatment)][N > 1] # checking for treatments that match multiple spid
# if there is not a 1-to-1 correspondence, update treatment names in "supplemental_mea_treatment_name_map.csv"

# update treatment names with entries in "supplemental_mea_treatment_name_map.csv" corresponding to dataset
# (treatment -> "mea_treatment_name", "updated_treatment_name" column will match "PREFERRED_NAME"
# dat <- update_treatment_names(dat, root_output_dir, dataset_title)

# assign spids
# dat <- check_and_assign_spids(dat, spidmap)
# can't use this fun, since I'm not actually assinging spids
# adapted:
if (spidmap[!is.na(DNTP_blind_code) & treatment %in% unique(dat$treatment), .(length(unique(DNTP_blind_code))), by = "treatment"][,any(V1 !=1)]) {
  stop(paste0("The following treatments map to multiple DNTP_blind_codes: ",
              spidmap[!is.na(DNTP_blind_code) & treatment %in% unique(dat$treatment), .(length(unique(DNTP_blind_code))), by = "treatment"][V1 != 1,paste0(treatment,collapse=", ")]))
}
dat <- merge(dat, spidmap[, .(DNTP_blind_code, treatment, footnote_chars, footnote)], by = "treatment", all.x = TRUE)
dat[wllt != 't' & is.na(DNTP_blind_code), DNTP_blind_code := treatment]
if (dat[wllt == "t", any(is.na(DNTP_blind_code))]) {
  cat("The following treatments don't have a corresponding DNTP_blind_code in the spidmap:\n")
  print(dat[wllt == "t" & is.na(DNTP_blind_code), unique(treatment)])
  stop("Adjust input dat before continuing")
}

# Confirm that substances with note of water solution match the solvent I assigned
dat[grepl('Water',footnote), .N, by = .(treatment, solvent)]
# treatment solvent    N
# 1:  7126 H10   Water 3654
# 2:  7126 H11   Water 1827
# 3:   7126 H8   Water 1827
# 4:   7126 H9   Water 1827
# all water, cool
dat[solvent == 'Water', .N, by = .(treatment, solvent, footnote)]
# treatment solvent                                  footnote    N
# 1:  7126 G10   Water                                      <NA> 1827
# 2:  7126 H10   Water         Water solution; insoluble in DMSO 3654
# 3:  7126 H11   Water Water solution; max DMSO solubility 10 mM 1827
# 4:   7126 H8   Water  Water solution; max DMSO solubility 2 mM 1827
# 5:   7126 H9   Water         Water solution; insoluble in DMSO 1827
# Kathleen mentioned 7/25/22 that she was originally given G10 in DMSO, but before she tested it, she was given a new sample that was dissolved in water instead.

# Where exact conc couldn't be determined - I feel compelled to report that in the well quality, but they should know that
dat[grepl('could not be determined',footnote), .N, by = .(treatment, footnote)]
# 9 substances
dat[footnote == 'Coded Plate Map stock conc <= 10. Exact concentration could not be determined.', 
    wllq_notes := paste0(ifelse(is.na(wllq_notes) | wllq_notes == '', '', paste0(wllq_notes,'; ')),
                                footnote)]



# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound
dat[, conc_srcf := conc] # save the original conc's in a column

# before dive in too much further, let's confirm consistent treatment labels by wells
dat[, .(length(unique(treatment))), by = .(apid, rowi, coli)][V1 > 1]
# empty, cool!

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# if any, standardize those before continuing.
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
problem_comps
# [1] "7126 B11" "7126 D1"  "7126 D2"  "7126 D3"  "7126 E11" "7126 F8"  "7126 G7"  "9163 A12"
check.rows <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
setkey(check.rows, treatment, apid, rowi, coli)
setkey(dat, treatment, apid, rowi, coli)
View(dat[J(check.rows)][, .N, by = .(treatment, apid, rowi, coli, conc, srcf)][order(treatment, rowi, coli, conc)])

# I'm guessing that the conc's the calculations files were corrected in these cases, but not in the maestro exp log
# but there's always the question - was the conc corrected correctly?

# Okay, I think the maestro exp log might actually be off in some of these cases
dat[, max_conc_by_srcf  := max(conc, na.rm = T), by = .(treatment, srcf, apid)]
dat[treatment %in% problem_comps, .N, by = .(treatment, max_conc_by_srcf, srcf)]
setkey(check.rows, apid, rowi, coli)
setkey(dat, apid, rowi, coli)
dat[J(check.rows)][, .N, by = .(apid, rowi, coli, treatment)][order(apid, rowi, coli)]


# so, it seems like there is an issue with the assignment of the conc's for in the maestro log files
# Let's compare the max conctested with the spidmap file
# as a third-party source of what's true

# What if instead of the expected target conc's, I check with the same dilution approach that Seline uses, as indicated in the Dosing Prep sheet of the Calculatiosn files?
# Adn it's okay if it's different for a few substances - they will come up as different, then i can check
dilution.tb <- data.table(cndx = 7:1)
dilution.tb[cndx == 7, dilution_cndx_multiplier := 1]
dilution.tb[cndx == 6, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb[cndx == 5, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb[cndx == 4, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb[cndx == 3, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb[cndx == 2, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb[cndx == 1, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb[, dilution_factor := Reduce(f = `*`, dilution_cndx_multiplier, accumulate = TRUE)]
dilution.tb

# Are conc's at least consistent by srcf? So that I can assign cndx?
dat[wllt == 't', .(length(unique(conc))), by = .(treatment, apid, srcf)][V1 != 7]
# empty -> cool
dat[, cndx := frank(conc, ties.method = 'dense'), by = .(treatment, apid, srcf)]
dat[, summary(cndx)] # max is 7, cool
dat <- merge(dat, dilution.tb[, .(cndx, dilution_cndx_multiplier, dilution_factor)], by = 'cndx', all.x = T)

# Merge in spidmap info
dat <- merge(dat, spidmap[, .(DNTP_blind_code, treatment, sheet_stock_conc, sheet_stock_conc_units)], by = c('DNTP_blind_code','treatment'), all.x = T)
dat[, sheet_stock_conc := as.numeric(sheet_stock_conc)]

# Calculate my anticipated concentrations based on dilutions
dat[, top_conc := as.numeric(sheet_stock_conc)] # for starters, assume top_conc is same as sheet_stock_conc
dat[, my_anticipated_conc := top_conc*dilution_factor]
dat[signif(conc,4) != my_anticipated_conc, .N, by = .(treatment, sheet_stock_conc, conc, my_anticipated_conc, srcf)] # several cases... but I would think 3 sig figs is enough
dat[signif(conc,3) != signif(my_anticipated_conc,3), .N, by = .(treatment, sheet_stock_conc, cndx, conc, my_anticipated_conc, srcf)][order(treatment, conc)]

# Visually compare concentrations
dat[, conc_log10 := signif(log10(conc), 3)]
dat[, my_anticipated_conc_log10 := signif(log10(my_anticipated_conc), 3)]

library(ggplot2)
ggplot(dat, aes(x = conc_log10, y = my_anticipated_conc_log10)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

# Possible reasons for disagreement between conc and my anticipated conc:
# - substance was repeated at a lower conc, so sheet_stock_conc is not the base/max conc in uM
# - conc's were not corrected to sheet_stock_conc at all
# - conc's were "corrected" to the wrong sheet_stock_conc

# Identify substances that were likely tested at a lower starting concentration
dat[, culture := sub('_.*','',apid)]
dat[, max_conc_by_treatment_culture := max(conc), by = .(treatment, culture)]
dat[max_conc_by_treatment_culture < 0.1*as.numeric(sheet_stock_conc), .N, by = .(treatment, sheet_stock_conc, max_conc_by_treatment_culture)]
# treatment   sheet_stock_conc max_conc_by_treatment_culture    N
# 1:  7126 C11 50.015000000000001                      0.050015 1827
# 2:   7126 D5            100.121                      0.100121 1827
# 3:  7126 A12            100.026                      0.100030 1827
# 4:   7126 A4            100.032                      0.300100 1827
# 5:  7126 B11            101.024                      0.101024 1827
# 6:   7126 D7 100.18300000000001                      1.000000 1827
# 7:   7126 F1            100.089                      0.100090 1827
# 8:   7126 D4             100.02                      0.100020 1827
# 9:  7126 H10            100.394                      0.100030 1827
# 10:   7126 F8 25.013999999999999                      0.025014 1827
# Looks like all of these are 1000X lower than sheet stock conc
# Except for 7126 D7 -> from my notes with Seline, since this one was slighlty less extremely potent, we agreed to retest at with 1uM as new highest conc

# were all of these previously tested at lower conc's?
retested.diff.conc.substances <- dat[, .(length(unique(max_conc_by_treatment_culture))), .(treatment)][V1 > 1, treatment]
dat[wllt == 't', .(length(unique(treatment))), by = .(retested_at_diff_concs = treatment %in% retested.diff.conc.substances, 
                                           max_conc_below_stck = max_conc_by_treatment_culture < 0.1*sheet_stock_conc)]
#    retested_at_diff_concs max_conc_below_stck  V1
# 1:                  FALSE               FALSE 105
# 2:                   TRUE               FALSE   9
# 3:                   TRUE                TRUE   9
# 4:                  FALSE                TRUE   1
# 5:                  FALSE                  NA   2
# Makes sens that the first runs of the 9 retested.diff.conc.substances would be at teh usual conc,
# then the second run woudl be at a diff conc.

# What's going on where max conc below stck is NA?
dat[wllt == 't' & is.na(sheet_stock_conc), .N, by = .(treatment)]
# treatment    N
# 1: Acetaminophen 3654
# 2:   Bisphenol A 3654
# Confirm that the concentrations don't look like they need to be corrected or standardized across files
dat[wllt == 't' & is.na(sheet_stock_conc), .N, by = .(treatment, cndx, conc)][order(treatment, cndx)]
# hmm.. some inconsistencies in BPA
# What is true? For files that are wrong, what went wrong?
dat[treatment == 'Bisphenol A', .N, by = .(cndx, conc, srcf)][order(cndx)]
# ah, so BPA Was tested in 2 different cultures.
# Looking at the calculations files from groups 19 and 20, looks like a different dilution scheme was used in the different cultures
# Are the conc's at least consistent from within the same culture?
dat[treatment == 'Bisphenol A', .(length(unique(conc))), by = .(cndx, srcf, culture)][V1 > 1]
# empty cool -> I think I can assume that the concentrations are correct!
# For Acetminophen and Bisphenol A

# What's goign on with teh 1 substance that was not retested in multiple cultures, but max conc is still below stkc?
# intential or meta data error?
dat[wllt == 't' & !treatment %in% retested.diff.conc.substances & max_conc_by_treatment_culture < 0.1*sheet_stock_conc, .N, by=.(culture, treatment, sheet_stock_conc, cndx, conc, my_anticipated_conc)][order(cndx)]
# culture treatment    sheet_stock_conc cndx        conc my_anticipated_conc   N
# 1: 20210915   7126 A4 100.032    1 0.000300096            0.100032   6
# 2: 20210915   7126 A4 100.032    1 0.000300000            0.100032 255
# 3: 20210915   7126 A4 100.032    2 0.001000320            0.300096   6
# 4: 20210915   7126 A4 100.032    2 0.001000000            0.300096 255
# 5: 20210915   7126 A4 100.032    3 0.003000960            1.000320   6
# 6: 20210915   7126 A4 100.032    3 0.003000000            1.000320 255
# 7: 20210915   7126 A4 100.032    4 0.010003200            3.000960   6
# 8: 20210915   7126 A4 100.032    4 0.010000000            3.000960 255
# 9: 20210915   7126 A4 100.032    5 0.030009600           10.003200   6
# 10: 20210915   7126 A4 100.032    5 0.030010000           10.003200 255
# 11: 20210915   7126 A4 100.032    6 0.100032000           30.009600   6
# 12: 20210915   7126 A4 100.032    6 0.100030000           30.009600 255
# 13: 20210915   7126 A4 100.032    7 0.300096000          100.032000   6
# 14: 20210915   7126 A4 100.032    7 0.300100000          100.032000 255
# this is quite a bit different. Intentional or mistake?

# do all conc's agree, across srcfs?
dat[treatment == '7126 A4', .N, by = .(culture, treatment, cndx, conc, srcf)]
# calculatiosn file has more decimal places, but otherwise is the same
# also note that only 1 culture tested, so that simplifies

# Ah, I see the note from the lab notebook! A different dilution was used for chemical 4 from G1.
# Specifically: 
# Separate dilution for G1 Chemical 4
# - Dilute 10:1 twice to get 1mm
# - 15uL of 1mm into 35uL of DMSO to get 0.3mm
# - Set 0.3mm as highest dose
# Conc range: 0.3mm - 0.0003mm
dat[treatment == '7126 A4' & culture == '20210915', top_conc := (sheet_stock_conc/100)*15/(35+15)] # based on above notes, copied from lab notebook
dat[treatment == '7126 A4', .N, by = .(top_conc, sheet_stock_conc, max_conc_by_treatment_culture)] 
# top_conc    sheet_stock_conc max_conc_by_treatment_culture    N
# 1: 0.300096 100.032                        0.3001 1827
# cool!
# dilution scheme also altered to follow 0.1, 0.3 pattern
dilution.tb.treatmentA4 <- data.table(treatment = '7126 A4', cndx = 7:1)
dilution.tb.treatmentA4[cndx == 7, dilution_cndx_multiplier := 1]
dilution.tb.treatmentA4[cndx == 6, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb.treatmentA4[cndx == 5, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb.treatmentA4[cndx == 4, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb.treatmentA4[cndx == 3, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb.treatmentA4[cndx == 2, dilution_cndx_multiplier := 15/(15+30)]
dilution.tb.treatmentA4[cndx == 1, dilution_cndx_multiplier := 15/(35+15)]
dilution.tb.treatmentA4[, dilution_factor := Reduce(f = `*`, dilution_cndx_multiplier, accumulate = TRUE)]
dat <- merge(dat, dilution.tb.treatmentA4, by = c('treatment','cndx'), all.x = T, suffixes = c('','.add'))
dat[treatment == '7126 A4', dilution_factor := dilution_factor.add]
dat[treatment == '7126 A4', dilution_cndx_multiplier := dilution_cndx_multiplier.add]
dat[, dilution_factor.add := NULL]
dat[, dilution_cndx_multiplier.add := NULL]

# Update the top_conc for chem retested at lower conc
dat[treatment %in% retested.diff.conc.substances & max_conc_by_treatment_culture < 0.1*sheet_stock_conc, .N, by = .(treatment, sheet_stock_conc, max_conc_by_treatment_culture)]
# looks like new top_conc in these cultures is approximately 1/1000 of the sheet_stock_conc -> I'll based my guesses on that, then see how it agrees
# Except for 7126 D7, which matches my notes
dat[treatment %in% setdiff(retested.diff.conc.substances,'7126 D7') & max_conc_by_treatment_culture < 0.1*sheet_stock_conc, top_conc := sheet_stock_conc/1000]
dat[treatment %in% c('7126 D7') & max_conc_by_treatment_culture < 0.1*sheet_stock_conc, top_conc := sheet_stock_conc/100]


# update anticipated conc's, check for remaining differences
dat[, my_anticipated_conc := top_conc*dilution_factor]
dat[signif(conc,4) != my_anticipated_conc, .N, by = .(treatment, sheet_stock_conc, conc, my_anticipated_conc, srcf)] # still several cases
dat[signif(conc,3) != signif(my_anticipated_conc,3), .N, by = .(culture, treatment, sheet_stock_conc, top_conc, dilution_factor, cndx, conc, my_anticipated_conc, srcf)][order(treatment, conc)]
dat[, conc_deviation_pct := (my_anticipated_conc - conc)/((my_anticipated_conc + conc)*0.5)*100]


# 7126 B8 -> differences in rounding
# we'll get back to this

# 7126 D1 -> this seems like potentially an incorrect stkc used?
# hmm, stkc entered in calculations file looks correct.
# Just opened first maestro log file - it looks fine
# Maybe just a specific plate was affected?
dat[treatment == '7126 D1', .N, by = .(signif(conc,3) != signif(my_anticipated_conc,3), srcf, apid)]
#    signif                                                 srcf               apid   N
# 1:  FALSE 20211124_NFA_DNT_NTP 2021_Group 7_ Calculations.xlsx 20211124_MW78-6214  14
# 2:  FALSE                                  DNT_NTP2021_AUC.csv 20211124_MW78-6214 119
# 3:  FALSE                    DNT_NTP2021_parameters_by_DIV.csv 20211124_MW78-6214 476
# 4:  FALSE 20211124_NFA_DNT_NTP 2021_Group 7_ Calculations.xlsx 20211124_MW78-6215  14
# 5:   TRUE                                  DNT_NTP2021_AUC.csv 20211124_MW78-6215 119
# 6:   TRUE                    DNT_NTP2021_parameters_by_DIV.csv 20211124_MW78-6215 476
# 7:  FALSE 20211124_NFA_DNT_NTP 2021_Group 7_ Calculations.xlsx 20211124_MW78-6216  14
# 8:   TRUE                                  DNT_NTP2021_AUC.csv 20211124_MW78-6216 119
# 9:   TRUE                    DNT_NTP2021_parameters_by_DIV.csv 20211124_MW78-6216 476
# ah, yep, looks liek doses in second version of maestro exp log file are incorrect.

# Maybe I should just default to the calculations files... hmm.
# Get all meta data from there?
# But MCL files are so deeply integrated into the pipeline...
# What if I read from calculations files first?

# Like, it's really not feasible to expect Selien to hand-create the MCL files
# It's jsut a waste of time when we have such better methods
# I coudl even automate reading in the dilution formulas from teh Dosing Prep!! -> weekend project :)
# But might still want to confirm where formula is non-standard, rather than just read in?

# So long term: MCL files will be eliminated
# I'll include that functionality for the legacy code, but won't depend on it going forward.

# options:
# - just update th eMCL file based on what I think it should be
# - just update the conc's here, again based on what I think (e.g. default to calculations files)
# - wait and talk to Selien about it

# I really dont' think anyone is going to look at the MCL files besides me
# So I'm just goign to do what is easiest for me, but still produces correct results

# So I will just default to the concentration in the calc file, with a note.
dat[culture == '20211124' & apid %in% c('20211124_MW78-6215','20211124_MW78-6216'), 
    `:=`(conc_correction_note = 'stock-conc corrected concentrations were not manually rotated correctly to match the treatments in the master chemical list files. Defaulting to concentrations in Calculations file',
         conc = ifelse(signif(conc,3) == signif(my_anticipated_conc,3),
                       conc,
                       unique(conc[grepl('Calculations',srcf)]))),
    by = .(culture, apid, treatment, cndx)]

# Next problem chem:
dat[signif(conc,3) != signif(my_anticipated_conc,3), .N, by = .(culture, treatment, sheet_stock_conc, top_conc, dilution_factor, cndx, conc, my_anticipated_conc, srcf)][order(treatment, conc)]
dat[signif(conc,3) != signif(my_anticipated_conc,3) & abs(conc_deviation_pct) > 0.1, .N, by = .(culture, treatment, sheet_stock_conc, top_conc, dilution_factor, cndx, conc, my_anticipated_conc, conc_deviation_pct, srcf)][order(treatment, conc)]


# 7126 H10
# most concerned about calculatiosn file
# calcfile cndx 6: 0.030007800
# 

dat[treatment == '7126 H10' & grepl('Calculations',srcf) & culture  == '20220601', .N, by = .(sheet_stock_conc, dilution_factor, cndx, conc, my_anticipated_conc, conc_deviation_pct)][order(cndx)]
# sheet_stock_conc dilution_factor cndx        conc my_anticipated_conc conc_deviation_pct N
# 1:          100.394           0.001    1 0.000100026         0.000100394           0.367229 6
# 2:          100.394           0.003    2 0.000300078         0.000301182           0.367229 6
# 3:          100.394           0.010    3 0.001000260         0.001003940           0.367229 6
# 4:          100.394           0.030    4 0.003000780         0.003011820           0.367229 6
# 5:          100.394           0.100    5 0.010002600         0.010039400           0.367229 6
# 6:          100.394           0.300    6 0.030007800         0.030118200           0.367229 6
# 7:          100.394           1.000    7 0.100026000         0.100394000           0.367229 6
# ya know what - I think the max conc is off in these cases
# usually th etop cndx is set equal to the stock conc
# but here, Seline had to manually update conc and cdnx 7 (col 8)
# notable, the conc for 7126 H10 at highest cndx (0.100026000)
# is the same as it is for 7126 A12, in the same file
# So likley Seline jsut copied and pasted.
# Seems like a good idea

# So am i totally being an ass for trackign it at this level of detail?
# the conc's are goign to differ by 0.367%
# What does that translate to on a log scale?
# like, if AC501 = x, and AC502 = 1.00367*x, 
# what's teh difference of the logs?
# log(x) - log(1.000367x) = log(x/1.00367x) = log(1/1.00367) = 
log(1/1.00367) # -0.00366328
log(1/(1-0.00367)) #[1] 0.00367675 (the inverse)
# At a really high level, agreement with 0.5 log is usually considered pretty good.
# Actually, from Carstens 2021, 
# "The variability in potency across repeats (measured by computing the average standard deviation of bioactivity at each endpoint) was < 0.6 log10-µM 
# for each chemical, except for 2,2’,4,4’-tetrabromodiphenyl ether and di(2-ethylhexyl) phthalate where there was more uncertainty in the potency values 
# (Supplementary Table 4), suggesting generally that AC50 values in the NFA might reasonably vary by +/- 0.5 log10-µM, or approximately ± 1 SD."
# So I think i'm being way too detail-oriented if I'm trackign differences in conc that ammount to 0.003 on a log scale

# Okay, so how can i implement this?
# options:
# - say I don't want the log of the conc's to differ by more than 0.005 (2 orders of mag below variability of the assay -> way sufficient)

dat[, conc_log10 := log10(conc)]
dat[, my_anticipated_conc_log10 := log10(my_anticipated_conc)]

dat[abs(conc_log10 - my_anticipated_conc_log10) >= 0.005]
# empty, cool!!
# there ya have it

# So now the consideration - there are still cases where the conc's dont' line up 100%
# even if I take 3 sig fig's.
# Will this get people's undies in a bunch? -> actually it might get my undies in a bunch, bc I need the replicate ID!!

# ugh

# options:
# - just default to the conc from teh calculations file, as long as it's never too different. That way it's at least standardized
# - default to my anticipated conc, by I think I'm more right.
# the choice is quantitatively NOT IMPORTANT
# So I'll just default to the values from the calc file!!

dat[, conc_calc_file := unique(conc[grepl('Calculations',srcf)]), by = .(culture, apid, treatment, rowi, coli)]
dat[, conc_calc_file_log10 := log10(conc_calc_file)]
dat[abs(conc_log10 - conc_calc_file_log10) >= 0.005]
# empty -> so we can default to the conc from the calc file for standardization!!



# END HERE:
# continue to resolve all differences in conc
# Ultimately, may have to correct some conc's, but want to document/know why (srcf conc was not corrected, or corrected to wrong stkc, etc.)

# Below: adapted from confirm_concs

spidmap[, .(stkc, expected_stock_conc, spidmap_guess_concs = paste0(signif(stkc/expected_stock_conc*expected_target_concs,3),collapse=",")), by = "DNTP_blind_code"]

# check if any conc's are NA
if (dat[wllt == 't', any(is.na(conc))]) {
  stop(paste("\nThe following treatments have conc NA:",paste0(dat[is.na(conc), unique(treatment)],collapse=",")))
}

# get the stck 
spidmap[, stkc := as.numeric(sheet_stock_conc)]
spidmap[, stkc_unit := sheet_stock_conc_units]
# there can be multiple stock_concs listed for a given spid in file, so eliminating duplicates here and using invitrodb stkc
spidmap <- spidmap[, unique(.SD), .SDcols = c("DNTP_blind_code","treatment","stkc","stkc_unit","expected_stock_conc")] 
# I am trusting that there is only 1 stkc for each spid lsited in invitrodb, so I won't check for that

# compare the concentrations
cat("\nAll compounds are assumed to have conc's",expected_target_concs,"\n(You can change this by updating the argument 'expected_target_concs' of the function confirm_concs()).\n")
compare_concs <- merge(spidmap[, .(stkc, expected_stock_conc, spidmap_guess_concs = paste0(signif(stkc/expected_stock_conc*expected_target_concs,3),collapse=",")), by = "DNTP_blind_code"],
                       dat[wllt == "t", .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=","), num_concs = length(unique(conc))), by = c("DNTP_blind_code","treatment")], 
                       by = "DNTP_blind_code", all.y = TRUE)
cat('\nFYI, the following stock conc\'s pulled from invitrodb do not match the expected stock conc:\n')
print(compare_concs[signif(stkc,4) != signif(expected_stock_conc,4), .(DNTP_blind_code, treatment, stkc, expected_stock_conc, spidmap_guess_concs, source_concs, num_concs)])

  cat("The concentrations for the following compounds might need to be corrected:\n")
  # removing this feature for now -> user can do manually if needed, and check that it is correct
  # compare_concs$probably_partially_conc_corrected <- sapply(strsplit(compare_concs$source_concs,split=","), function(x) length(x) > length(expected_target_concs))
  print(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)][order(num_concs)])
  
  # if (update_concs_without_prompt) response <- "y"
  # else response <- readline(prompt = "Update conc's where source_concs != spidmap_guess_concs with conc := signif(stkc/expected_stock_conc*source_concs, 3)? (y/n): ")
  # 
  # if (response %in% c("y","Y","yes","Yes")) {
    
    compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs), need_to_update_concs := TRUE]
    
    dat[, conc_org := conc]
    dat <- merge(dat, compare_concs[, .(DNTP_blind_code, need_to_update_concs, stkc, expected_stock_conc)], by = "DNTP_blind_code", all.x = TRUE)
    
    # for DNTP_blind_code's with 'probably_partially_conc_corrected', standardize the conc's first:
    # cat("Standardizing concs where 'probably_partially_conc_corrected'==TRUE...\n")
    # dat[probably_partially_conc_corrected == TRUE, conc := signif(conc, digits = 1)]
    # dat[, conc_standardized := conc]
    
    # now correct the conc's
    cat("Correcting conc's...\n")
    dat[need_to_update_concs == TRUE, conc := signif(stkc/expected_stock_conc*conc, 3)]
    update_summary <- dat[need_to_update_concs == TRUE, .(stkc, concs_in_source_dat = paste0(unique(conc_org),collapse=", ")),
                          by = c("DNTP_blind_code","treatment","conc","stkc","expected_stock_conc")][order(DNTP_blind_code,conc), .(treatment, DNTP_blind_code, stkc, expected_stock_conc, 
                                                                                                              concs_in_source_dat, conc_updated = format(conc,digits=4,scientific=F))]
    # if (update_concs_without_prompt) {
    #   cat("conc's that changed:\n")
      print(update_summary[signif(as.numeric(concs_in_source_dat),3) != signif(as.numeric(conc_updated),3)]) # display conc's that were actually updated
      dat[is.na(conc),.N, by = .(treatment)]
      # treatment    N
      # 1: Bisphenol A 3654
      # }
    # else {
    #   # assign("update_summary",update_summary, envir = .GlobalEnv)
    #   cat("View the table 'update_summary' to confirm that the concentration-corrections are correct.\n")
    #   cat("If it looks correct, enter c to continue. Else Q to quit and fix.\n")
    #   browser()
    #   # response <- readline(prompt = "Does conc correction look correct for each compound and dose? (y/n): ")
    #   # if (!(response %in% c("y","Y","yes","Yes"))) browser()
    # }
    
    dat[conc_org != conc, .N, by = .(treatment)]
    dat[, c("conc_org","stkc","need_to_update_concs","expected_stock_conc") := list(NULL)] # remove added columns
  

# Wllq updates by treatment -----------------------------------------------


## New method ----------------------------

#*** Need to confirm this code, just wrote without actually implementing

# add needed columns to cndx
dat[, cndx := match(conc, unique(conc)), by = .(spid, treatment)]
dat[grepl('LDH',acnm), endpoint_type := 'LDH']
dat[grepl('AB',acnm), endpoint_type := 'CTB'] # this is the abbreviation I'm using in the wllq tables... could change
dat[is.na(endpoint_type), endpoint_type := 'mea']

# Read in and clean wllq table
wllq.tb.by.trt <- as.data.table(read.xlsx(file.path(dataset_title,'well_quality_notes_per_culture_treatment_cndx.xlsx'), sheet = 1))

# Expand the sections where cndx == 'all'
cndx_list <- unique(dat$cndx)
add.tb <- rbindlist(lapply(cndx_list, 
                           function(cndxi) cbind(wllq.tb.by.trt[cndx %in% c('all') | is.na(cndx), .SD, .SDcols = setdiff(names(wllq.tb.by.trt),'cndx')], 
                                                 'cndx' = cndxi)))
wllq.tb.by.trt <- wllq.tb.by.trt[!(cndx %in% 'all' | is.na(cndx))]
wllq.tb.by.trt <- rbind(wllq.tb.by.trt, add.tb)

# split by MEA, CTB, LDH, etc
wllq.tb.by.trt <- rbindlist(lapply(unique(unlist(stri_split(wllq.tb.by.trt$affected_endpoints, regex = '[,; ]'))),
                                   function(endpoint_typei) cbind(wllq.tb.by.trt[grepl(endpoint_typei,affected_endpoints), 
                                                                                 .SD, .SDcols = setdiff(names(wllq.tb.by.trt),'affected_endpoints')], 'endpoint_type' = endpoint_typei)))

# Make sure there is just 1 row per well
check.ids <- wllq.tb.by.trt[, .N, by = .(culture_date, treatment, cndx)][N > 1, .(culture_date, treatment, cndx, endpoint_type)]
setkey(wllq.tb.by.trt, culture_date, treatment, cndx)
wllq.tb.by.trt[.(check.ids)] # these notes are similar, can just collapse
wllq.tb.by.trt[, .(wllq_note = paste0(sort(unique(wllq_note)),collapse = ", "),
                   source = paste0(sort(unique(source)),collapse = ", ")),
               by = c(setdiff(names(wllq.tb.by.trt), c('wllq_note','source')))]


# *** How to merge in with existing wllq_prelim, wllq_notes?

# Merge in!
dat[, in_dat := 1]
dat <- merge(dat, wllq.tb.by.trt, by = c('culture_date','treatment','cndx','endpoint_type'), all = T)

# Check for rows in wllq.tb.by.trt that did not match a row in dat
dat[is.na(in_dat)]


## OLD method -----------------------

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


# 
# 
# # Check conc units --------------------------------------------------------
# 
# dat[, .N, by = .(units)]
# # any not in uM? If so, convert to uM

# note solvents are %
# note spidmap expected ssotck conc units



# FINAL DATA CHECKS -------------------------------------------------------------
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'), echo = FALSE)
dataset_checks(dat)

# Check for the expected number of technical replicates
dat[wllt == 't', .(length(unique(paste0(apid,rowi,coli)))), by = .(spid, conc)][V1 != 3]
# do you except these cases to have more than or less than 3 replicates?
# Were some samples repeated, and only certain repeats meant to be included?

# Any other plots or things to check?

# Save a copy for now, so can evaluate what needs to berepeated

setkey(dat, NULL)
description <- 'Saving a preliminary version of DNT NTP 2021
So that we can evalute which chemicals need to be repeated.
What is left to do:
- Confirm that all concentrations have been corrected to the exact conc consistently
- convert all concentration units to uM
- Assign sample ids/blind codes
Date Ran: May 17 2022'
save(dat, description, file = file.path(root_output_dir,dataset_title,'output','DNT_NTP2021_preliminary_longfile.RData'))


# # save dat and graphs
# setkey(dat, NULL)
# save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
# rm(dat)
# 
if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")
