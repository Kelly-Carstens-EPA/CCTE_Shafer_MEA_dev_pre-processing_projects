rm(list=ls()) # clear environment
graphics.off() # clear plot history
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "NTP91" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probably leave this as true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName = "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control
# Enter the names of the compounds as they appear in the MEA data that have a vehicle control other than the default
different_vehicleControlCompounds = c() # e.g. c("Sodium Orthovanadate", "Amphetamine")
# Enter the names of the vehicle controls as they correspond to the compounds in the previous list
different_vehicleControls = c() # e.g. c("Water", "Water")

spidmap_file <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/NTP tcpl prep/SPID map/Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"
spid_sheet <- "NeuroTox 91 Cmpds"

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

# prepare spidmap
spidmap <- as.data.table(read.xlsx(spidmap_file, sheet = spid_sheet))
head(spidmap)
unique(spidmap$Concentration_Unit) # all mM?
setnames(spidmap, old = c("Chemical.Name", "Conc..(mM)", "SPID"), new = c("treatment","stock_conc","spid"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
spidmap[, treatment := as.character(treatment)]
spidmap[, stock_conc := as.numeric(stock_conc)]
spidmap[, expected_stock_conc := 20] # initialize expected_stock_conc. Usually this is 20mM. Change as needed.
# update expected_stock_conc for individual compouunds where needed 
spidmap[treatment %in% c("2,2',4,4',5,5'-Hexabromodiphenyl ether","Dibenz(a,h)anthracene"), expected_stock_conc := 10.0]
spidmap[treatment == "Chrysene", expected_stock_conc := 9.7]
head(spidmap[, .(treatment, spid, stock_conc, expected_stock_conc)])

# rename any compounds, if needed
auc <- fread(file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_AUC.csv")))
cyto <- fread(file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_cytotox.csv")))
auc[treatment == "1Ethyl3methylimidazolium diethylphosphate", treatment := "1-Ethyl-3-methylimidazolium diethylphosphate"]
auc[treatment == "2244 Tetrabromodiphenyl ether", treatment := "2,2',4,4'-Tetrabromodiphenyl ether"]
auc[treatment == "22445 Pentabromodiphenyl ether", treatment := "2,2',4,4',5-Pentabromodiphenyl ether"]
auc[treatment == "224455 Hexabromodiphenyl ether", treatment := "2,2',4,4',5,5'-Hexabromodiphenyl ether"]
auc[treatment == "2Ethylhexyl 2345 tetrabromobenzoate", treatment := "2-Ethylhexyl-2,3,4,5-tetrabromobenzoate"]
auc[treatment == "2Ethylhexyl diphenyl phosphate", treatment := "2-Ethylhexyl diphenyl phosphate"]
auc[treatment == "4HCyclopenta def phenanthrene", treatment := "4-H-Cyclopenta(d,e,f)phenanthrene"]
auc[treatment == "6 Hydroxydopamine hydrochloride", treatment := "6-Hydroxydopamine hydrochloride"]
auc[treatment == "Auramine", treatment := "Auramine O"]
auc[treatment == "Benzo a pyrene", treatment := "Benzo(a)pyrene"]
auc[treatment == "Benzo e pyrene", treatment := "Benzo(e)pyrene"]
auc[treatment == "Benzo ghi perylene", treatment := "Benzo[g,h,i]perylene"]
auc[treatment == "Benzo k fluoranthene", treatment := "Benzo(k)fluoranthene"]
auc[treatment == "Bis 2 ethylhexyl tetrabromophthalate", treatment := "Bis(2-ethylhexyl) tetrabromophthalate"]
auc[treatment == "Carbamic acid", treatment := "Dibenz[a,c]anthracene"]
auc[treatment == "Carbamic acid butyl 3iodo2propynyl ester", treatment := "Carbamic acid, butyl-, 3-iodo-2-propynyl ester"]
auc[treatment == "D Glucitol", treatment := "D-Glucitol"]
auc[treatment == "Di 2ethylhexyl phthalate", treatment := "Di(2-ethylhexyl) phthalate"]
auc[treatment == "Dibenz ac anthracene", treatment := "Dibenz[a,c]anthracene"]
auc[treatment == "Dibenz ah anthracene", treatment := "Dibenz(a,h)anthracene"]
auc[treatment == "Firemaster", treatment := "Firemaster 550"]
auc[treatment == "L Ascorbic acid", treatment := "L-Ascorbic acid"]
auc[treatment == "Manganese tricarbonyl 12345 eta 1 methyl 24 cyclopentadien 1 yl", treatment := "Manganese, tricarbonyl[(1,2,3,4,5-.eta.)-1-methyl-2,4-cyclopentadien-1-yl]-"]
auc[treatment == "Tris 2chloroisopropyl phosphate", treatment := "Tris(2-chloroethyl) phosphate"]
auc[treatment == "tert Butylphenyl diphenyl phosphate", treatment := "tert-Butylphenyl diphenyl phosphate"]

cyto[treatment == "1Ethyl3methylimidazolium diethylphosphate", treatment := "1-Ethyl-3-methylimidazolium diethylphosphate"]
cyto[treatment == "2244 Tetrabromodiphenyl ether", treatment := "2,2',4,4'-Tetrabromodiphenyl ether"]
cyto[treatment == "22445 Pentabromodiphenyl ether", treatment := "2,2',4,4',5-Pentabromodiphenyl ether"]
cyto[treatment == "224455 Hexabromodiphenyl ether", treatment := "2,2',4,4',5,5'-Hexabromodiphenyl ether"]
cyto[treatment == "2Ethylhexyl 2345 tetrabromobenzoate", treatment := "2-Ethylhexyl-2,3,4,5-tetrabromobenzoate"]
cyto[treatment == "2Ethylhexyl diphenyl phosphate", treatment := "2-Ethylhexyl diphenyl phosphate"]
cyto[treatment == "4HCyclopenta def phenanthrene", treatment := "4-H-Cyclopenta(d,e,f)phenanthrene"]
cyto[treatment == "6 Hydroxydopamine hydrochloride", treatment := "6-Hydroxydopamine hydrochloride"]
cyto[treatment == "Auramine", treatment := "Auramine O"]
cyto[treatment == "Benzo a pyrene", treatment := "Benzo(a)pyrene"]
cyto[treatment == "Benzo e pyrene", treatment := "Benzo(e)pyrene"]
cyto[treatment == "Benzo ghi perylene", treatment := "Benzo[g,h,i]perylene"]
cyto[treatment == "Benzo k fluoranthene", treatment := "Benzo(k)fluoranthene"]
cyto[treatment == "Bis 2 ethylhexyl tetrabromophthalate", treatment := "Bis(2-ethylhexyl) tetrabromophthalate"]
cyto[treatment == "Carbamic acid", treatment := "Dibenz[a,c]anthracene"]
cyto[treatment == "Carbamic acid butyl 3iodo2propynyl ester", treatment := "Carbamic acid, butyl-, 3-iodo-2-propynyl ester"]
cyto[treatment == "D Glucitol", treatment := "D-Glucitol"]
cyto[treatment == "Di 2ethylhexyl phthalate", treatment := "Di(2-ethylhexyl) phthalate"]
cyto[treatment == "Dibenz ac anthracene", treatment := "Dibenz[a,c]anthracene"]
cyto[treatment == "Dibenz ah anthracene", treatment := "Dibenz(a,h)anthracene"]
cyto[treatment == "Firemaster", treatment := "Firemaster 550"]
cyto[treatment == "L Ascorbic acid", treatment := "L-Ascorbic acid"]
cyto[treatment == "Manganese tricarbonyl 12345 eta 1 methyl 24 cyclopentadien 1 yl", treatment := "Manganese, tricarbonyl[(1,2,3,4,5-.eta.)-1-methyl-2,4-cyclopentadien-1-yl]-"]
cyto[treatment == "Tris 2chloroisopropyl phosphate", treatment := "Tris(2-chloroethyl) phosphate"]
cyto[treatment == "tert Butylphenyl diphenyl phosphate", treatment := "tert-Butylphenyl diphenyl phosphate"]
write.csv(auc, file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_AUC.csv")), row.names = FALSE)
write.csv(cyto, file.path(root_output_dir,dataset_title, "output", paste0(dataset_title, "_cytotox.csv")), row.names = FALSE)
rm(list = c("auc","cyto"))

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
source(file.path(scripts.dir, 'confirm_concs.R'))
tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title, spidmap, default_ControlTreatmentName,
                 different_vehicleControlCompounds = different_vehicleControlCompounds, different_vehicleControls = different_vehicleControls,
                 expected_target_concs = c(0.03,0.1,0.3,1,3,10,20))

# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
dat <- read.csv(file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")))
setDT(dat)
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?

rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}
