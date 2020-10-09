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
dat[treatment == "TBHQ", treatment := "tert-Butylhydroquinone"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
dat[treatment %in% c("6-propyl-2-thiouracil","6 Propyl 2 thiouracil"), treatment := "6-Propyl-2-thiouracil"]
dat[treatment == "Methadone", treatment := "Methadone hydrochloride"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
dat[treatment %in% c("IPBC","3 Iodo 2 propynyl N butylcarbamate"), treatment := "3-Iodo-2-propynyl-N-butylcarbamate"] # these names are listed in the same chemical row in the Shafer 2019 paper Table 1 as the PREFERRED_NAME and Common name
dat[treatment %in% c("Pravastatin","Pravastin sodium"), treatment := "Pravastatin sodium"] # Pravastatin was not used in this dataset, only Pravastain sodium
dat[treatment == "HPTE", treatment := "2,2-Bis(4-hydroxyphenyl)-1,1,1-trichloroethane"] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
dat[treatment == "Clothiandin", treatment := "Clothianidin"]
dat[treatment == "Diphenhydramine", treatment := "Diphenhydramine hydrochloride"]# these names are listed in the same chemical row in the Shafer 2019 paper as the PREFERRED_NAME and Common name
dat[treatment == "17beta Estradiol", treatment := "17beta-Estradiol"]
dat[treatment == "MGK 264", treatment := "MGK-264"]
dat[treatment == "Tributyltin methylacrylate", treatment := "Tributyltin methacrylate"] # "Tributyltin methylacrylate" does not seem to be a chemical at all
dat[treatment == "op-DDT", treatment := "o,p'-DDT"]
dat[treatment == "pp-DDD", treatment := "p,p'-DDD"]
dat[treatment == "pp-DDE", treatment := "p,p'-DDE"]
dat[treatment == "pp-DDT", treatment := "p,p'-DDT"]
# renaming compounds in auc ontogeny data
dat[treatment == "Rotenone - ToxCast G-8", treatment := "Rotenone"] # this compound was tested in both the NTP and Toxcast groups.
dat[treatment == "Disulfiram - ToxCast G-8", treatment := "Disulfiram"] # this compound was tested in both the NTP and Toxcast groups. 
dat[treatment == "Valinomycin - NTP", treatment := "Valinomycin"]
# these compounds were added from another batch - making names standardized
dat[treatment == "1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411", treatment := "1,1,2,2-Tetrahydroperfluoro-1-decanol"]
dat[treatment == "1H,1H,2H,2H-Perfluorooctyl iodide - TP0001413", treatment := "1H,1H,2H,2H-Perfluorooctyl iodide"]
dat[treatment == "Perfluoroundecanoic acid - TP0001411", treatment := "Perfluoroundecanoic acid"]

# assign spids
dat <- check_and_assign_spids(dat, spidmap)


# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
# treatment               apid rowi coli num_unique_concs_in_well
# 1:    Azoxystrobin 20170201_MW1145-25    4    8                        2
# 2:          Captan 20170201_MW1145-25    3    8                        2
# 3:      Carbofuran 20170201_MW1145-25    1    8                        2
# 4:       Cymoxanil 20170201_MW1145-25    6    8                        2
# 5:  Pyraclostrobin 20170201_MW1145-25    2    8                        2
# 6: Trifloxystrobin 20170201_MW1145-25    5    8                        2
# if any, standardize those before continuing.
problem_comps <- dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1, unique(treatment)]
problem_comps # all from the same apid

# get a summary of the treatments by srcf
summary_dat <- dat[treatment %in% problem_comps, .(conc_shown = unique(conc)), by = .(apid, rowi, coli, treatment, srcf)]
summary_dat[, conc_round := signif(conc_shown, 1)]
summary_dat[, conc_source := ""]
summary_dat[grepl("(Calc)|(Summary)",srcf), conc_source := "Calc"]
summary_dat[grepl("AUC",srcf), conc_source := "AUC"]
summary_dat[grepl("DIV",srcf), conc_source := "DIV"]
summary_wide <- dcast(summary_dat, apid + treatment + rowi + coli ~ conc_source, value.var = "conc_shown")
summary_wide
head(summary_wide, n = 7)

summary_wide[treatment == "Captan" & AUC != Calc]
# apid treatment rowi coli AUC Calc DIV
# 1: 20170201_MW1145-25    Captan    3    8  10  0.1  10
dat[treatment == "Captan", unique(conc)]

summary_wide[Calc != AUC]
# apid       treatment rowi coli AUC Calc DIV
# 1: 20170201_MW1145-25    Azoxystrobin    4    8  10 20.0  10
# 2: 20170201_MW1145-25          Captan    3    8  10  0.1  10
# 3: 20170201_MW1145-25      Carbofuran    1    8  10  0.3  10
# 4: 20170201_MW1145-25       Cymoxanil    6    8  10 20.0  10
# 5: 20170201_MW1145-25  Pyraclostrobin    2    8  10  0.1  10
# 6: 20170201_MW1145-25 Trifloxystrobin    5    8  10 20.0  10
# according to lab notebook, column 8 should be 10uM for all compounds
# Looking at the Calculatiosn files, i think these conc's jsut got flipped around
# see e.g. first compound - 20uM is listed for 2 columns
dat[treatment == "Azoxystrobin" & grepl("Calc",srcf) & apid == "20170201_MW1145-25", unique(conc), by = (coli)]
# coli    V1
# 1:    1 20.00
# 2:    3  0.03
# 3:    4  0.10
# 4:    5  0.30
# 5:    6  1.00
# 6:    7  3.00
# 7:    8 20.00
dat[treatment %in% problem_comps & apid == "20170201_MW1145-25" & coli == 8, unique(conc), by = "treatment"]
# set conc to 10uM for all of these compounds in column 8
dat[treatment %in% problem_comps & apid == "20170201_MW1145-25" & coli == 8, conc := 10.0]
# I am correcting this in the Calculations file as well

# for all of these... stkc is signficantly off from 20
# Can I be confident that Kathleen/someone didn't already correct for this in their dilutions?
#            spid    stkc expected_stock_conc                         spidmap_guess_concs                          treatment                                        source_concs num_concs
# 2: TP0001413A04  5.0000                  20          0.0075,0.025,0.075,0.25,0.75,2.5,5  1H,1H,2H,2H-Perfluorooctyl iodide                              0.03,0.1,0.3,1,3,10,20         7
# 3: TP0001649B02 10.0000                  20                0.015,0.05,0.15,0.5,1.5,5,10                           Mancozeb                              0.03,0.1,0.3,1,3,10,20         7
# 4: TP0001649B03 10.0000                  20                0.015,0.05,0.15,0.5,1.5,5,10                          Tamoxifen                              0.03,0.1,0.3,1,3,10,20         7
# 5: TP0001649D07  5.0000                  20          0.0075,0.025,0.075,0.25,0.75,2.5,5                       Erythromycin                              0.03,0.1,0.3,1,3,10,20         7
# 6: TP0001649D10 14.5000                  20    0.0217,0.0725,0.218,0.725,2.17,7.25,14.5            Methadone hydrochloride                              0.03,0.1,0.3,1,3,10,20         7
# 7: TP0001649E02  5.2175                  20 0.00783,0.0261,0.0783,0.261,0.783,2.61,5.22                     Clove leaf oil                              0.03,0.1,0.3,1,3,10,20         7
# 9: TP0001649E12 10.0000                  20                0.015,0.05,0.15,0.5,1.5,5,10                 Pravastatin sodium                              0.03,0.1,0.3,1,3,10,20         7
# 12: TP0001649G07 14.9000                  20    0.0223,0.0745,0.223,0.745,2.23,7.45,14.9                Cariporide mesylate                              0.03,0.1,0.3,1,3,10,20         7


# finally, run this:
source(file.path(scripts.dir, 'confirm_concs.R'))
dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,20))


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