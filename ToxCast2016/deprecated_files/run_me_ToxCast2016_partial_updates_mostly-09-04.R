rm(list=ls()) # clear environment
graphics.off() # clear plot history
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "ToxCast2016" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probs want to be true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName <- "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control

spidmap_file1 <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/ToxCast Compounds/EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx"
spid_sheet1 <- 1
spidmap_file2 <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/ToxCast tcpl prep/Pre-Processing_Second_Attempt/EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx"
spid_sheet2 <- 1
spidmap_file3 <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/NTP tcpl prep/SPID map/Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"
spid_sheet3 <- "NeuroTox 91 Cmpds"

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
  cat("Date Ran:",as.character.Date(Sys.Date()),"\n")
  cat(R.version.string,"\n")
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
# dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "20190904", plates = paste0("MW69-381",7:9), control_rowi = which(LETTERS[1:6] %in% c("E","F")))
# since I know the conc of DMSO, I am going to input that as well
dat[treatment == "DMSO", conc := 0.001] # 0.1% DMSO by volume


# Assign SPIDs ------------------------------------------------------------------
spidmap1 <- as.data.table(read.xlsx(spidmap_file1, sheet = spid_sheet1))
head(spidmap1)
unique(spidmap1$ALIQUOT_CONC_UNIT) # all mM
unique(spidmap1$ALIQUOT_SOLVENT) # DMSO
setnames(spidmap1, old = c("preferred_name","ALIQUOT_CONC", "EPA_SAMPLE_ID"), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap1[, treatment := as.character(treatment)]
head(spidmap1[, .(treatment, spid, stock_conc)])

# add the second spidmap
spidmap2 <- as.data.table(read.xlsx(spidmap_file2, sheet = spid_sheet2))
head(spidmap2)
unique(spidmap2$ALIQUOT_CONC_UNIT) # all mM?
spidmap2[ALIQUOT_CONC_UNIT != "mM"] # C10-21 sulfonic acids phenyl esters. we aren't using this compound right now, so no worries 
unique(spidmap2$ALIQUOT_SOLVENT) # DMSO
setnames(spidmap2, old = c("dsstox_preferred_name","ALIQUOT_CONC", "EPA_SAMPLE_ID"), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap2[, treatment := as.character(treatment)]
spidmap2 <- spidmap2[treatment %in% c("1,1,2,2-Tetrahydroperfluoro-1-decanol","1H,1H,2H,2H-Perfluorooctyl iodide","Perfluoroundecanoic acid")]
head(spidmap2[, .(treatment, spid, stock_conc)])

# third spidmap -> Just for Valinomycin from NTP compounds
spidmap3 <- as.data.table(read.xlsx(spidmap_file3, sheet = spid_sheet3))
head(spidmap3)
setnames(spidmap3, old = c("Chemical.Name","Conc..(mM)", "SPID"), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap3[, treatment := as.character(treatment)]
spidmap3 <- spidmap3[treatment == "Valinomycin"] # don't want other compounds, which could have diff spids
head(spidmap3[, .(treatment, spid, stock_conc)])
spidmap <- rbind(spidmap1[, .(treatment, spid, stock_conc)], spidmap2[, .(treatment, spid, stock_conc)], spidmap3[, .(treatment, spid, stock_conc)])
# spidmap[, length(unique(spid)), by = "treatment"][V1 != 1]
# spidmap[, length(unique(treatment)), by = "spid"][V1 != 1]

# finalize spidmap
spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
# update expected_stock_conc for individual compouunds where needed 
# for example, 
# spidmap[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
spidmap[, stock_conc := as.numeric(stock_conc)]
head(spidmap[, .(treatment, spid, stock_conc, expected_stock_conc)])

# rename any compounds, if needed
auc[treatment == "TBHQ", treatment := "tert-Butylhydroquinone"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
auc[treatment %in% c("6-propyl-2-thiouracil","6 Propyl 2 thiouracil"), treatment := "6-Propyl-2-thiouracil"]
auc[treatment == "Methadone", treatment := "Methadone hydrochloride"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
auc[treatment == "IPBC", treatment := "3-Iodo-2-propynyl-N-butylcarbamate"] # these names are listed in the same chemical row in the Shafer 2019 paper Table 1 as the PREFERRED_NAME and Common name
auc[treatment %in% c("Pravastatin","Pravastin sodium"), treatment := "Pravastatin sodium"] # Pravastatin was not used in this dataset, only Pravastain sodium
auc[treatment == "HPTE", treatment := "2,2-Bis(4-hydroxyphenyl)-1,1,1-trichloroethane"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
auc[treatment == "Clothiandin", treatment := "Clothianidin"]
auc[treatment == "Diphenhydramine", treatment := "Diphenhydramine hydrochloride"]# these names are listed in the same chemical row in the Shafer 2019 paper as the PREFERRED_NAME and Common name
auc[treatment == "17beta Estradiol", treatment := "17beta-Estradiol"]
auc[treatment == "MGK 264", treatment := "MGK-264"]
auc[treatment == "Tributyltin methylacrylate", treatment := "Tributyltin methacrylate"] # "Tributyltin methylacrylate" does not seem to be a chemical at all
# renaming compounds in auc ontogeny data
auc[treatment == "Rotenone - ToxCast G-8", treatment := "Rotenone"] # this compound was tested in both the NTP and Toxcast groups.
auc[treatment == "Disulfiram - ToxCast G-8", treatment := "Disulfiram"] # this compound was tested in both the NTP and Toxcast groups. 
auc[treatment == "Valinomycin - NTP", treatment := "Valinomycin"]
# these compounds were added from another batch - making names standardized
auc[treatment == "1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411", treatment := "1,1,2,2-Tetrahydroperfluoro-1-decanol"]
auc[treatment == "1H,1H,2H,2H-Perfluorooctyl iodide - TP0001413", treatment := "1H,1H,2H,2H-Perfluorooctyl iodide"]
auc[treatment == "Perfluoroundecanoic acid - TP0001411", treatment := "Perfluoroundecanoic acid"]

cyto[treatment == "TBHQ", treatment := "tert-Butylhydroquinone"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
cyto[treatment %in% c("6-propyl-2-thiouracil","6 Propyl 2 thiouracil"), treatment := "6-propyl-2-thiouracil"]
cyto[treatment == "Methadone", treatment := "Methadone hydrochloride"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
cyto[treatment == "IPBC", treatment := "3-Iodo-2-propynyl-N-butylcarbamate"] # these names are listed in the same chemical row in the Shafer 2019 paper Table 1 as the PREFERRED_NAME and Common name
cyto[treatment %in% c("Pravastatin","Pravastin sodium"), treatment := "Pravastatin sodium"] # Pravastatin was not used in this dataset, only Pravastain sodium
cyto[treatment == "HPTE", treatment := "2,2-Bis(4-hydroxyphenyl)-1,1,1-trichloroethane"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
cyto[treatment == "Clothiandin", treatment := "Clothianidin"]
cyto[treatment == "Diphenhydramine", treatment := "Diphenhydramine hydrochloride"]# these names are listed in the same chemical row in the Shafer 2019 paper as the PREFERRED_NAME and Common name
cyto[treatment == "17beta Estradiol", treatment := "17beta-Estradiol"]
cyto[treatment == "MGK 264", treatment := "MGK-264"]
cyto[treatment == "Tributyltin methylacrylate", treatment := "Tributyltin methacrylate"] # "Tributyltin methylacrylate" does not seem to be a chemical at all
cyto[treatment == "Valinomycin - NTP", treatment := "Valinomycin"]
# these compounds were added from another batch - making names standardized
cyto[treatment == "1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411", treatment := "1,1,2,2-Tetrahydroperfluoro-1-decanol"]
cyto[treatment == "1H,1H,2H,2H-Perfluorooctyl iodide - TP0001413", treatment := "1H,1H,2H,2H-Perfluorooctyl iodide"]
cyto[treatment == "Perfluoroundecanoic acid - TP0001411", treatment := "Perfluoroundecanoic acid"]

# assign spids
dat <- check_and_assign_spids(dat, spidmap)


# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# empty
# if any, standardize those before continuing.
# problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
# problem_comps

# finally, run this:
source(file.path(scripts.dir, 'confirm_concs.R'))
dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.1,0.3,1,3,10,30,100))


# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?


# save the data and graphs
setkey(dat, NULL)
save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")
