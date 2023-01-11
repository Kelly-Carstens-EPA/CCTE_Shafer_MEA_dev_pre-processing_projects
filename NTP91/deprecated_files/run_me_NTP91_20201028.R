rm(list=ls()) # clear environment
graphics.off() # clear plot history
###################################################################################
# USER INPUT
###################################################################################
dataset_title <- "NTP91" # the name for the current dataset, e.g. "name2020" (this should match the name of the folder under 'pre-process_mea_nfa_for_tcpl', e.g. 'Frank2017' or 'ToxCast2016')
pause_between_steps <- TRUE # probably leave this as true when you first run
save_notes_graphs <- FALSE # Do this after have run thru once, to save a log of the steps. Set pause_between_steps to FALSE if saving notes and graphs for speed

default_ControlTreatmentName = "DMSO" # usually DMSO. all compounds other than those listed below should have this vehicle control

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

# run tcpl_MEA_dev_AUC
source(file.path(scripts.dir, 'tcpl_MEA_dev_AUC.R'))
dat <- tcpl_MEA_dev_AUC(basepath = file.path(root_output_dir,dataset_title), dataset_title)


# change untreated wells to Control Treatment ------------------------------------
dat[wllt == "n", treatment := default_ControlTreatmentName]
# update other control wells as needed, e.g.
# dat <- update_control_well_treatment(dat, control_compound = "Water",culture_date = "20190904", plates = paste0("MW69-381",7:9), control_rowi = which(LETTERS[1:6] %in% c("E","F")))
# all are dissolved in DMSO
dat[wllt == "n", conc := 0.001]


# Assign SPIDs ------------------------------------------------------------------
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
dat[treatment == "1Ethyl3methylimidazolium diethylphosphate", treatment := "1-Ethyl-3-methylimidazolium diethylphosphate"]
dat[treatment == "2244 Tetrabromodiphenyl ether", treatment := "2,2',4,4'-Tetrabromodiphenyl ether"]
dat[treatment == "22445 Pentabromodiphenyl ether", treatment := "2,2',4,4',5-Pentabromodiphenyl ether"]
dat[treatment == "224455 Hexabromodiphenyl ether", treatment := "2,2',4,4',5,5'-Hexabromodiphenyl ether"]
dat[treatment == "2Ethylhexyl 2345 tetrabromobenzoate", treatment := "2-Ethylhexyl-2,3,4,5-tetrabromobenzoate"]
dat[treatment == "2Ethylhexyl diphenyl phosphate", treatment := "2-Ethylhexyl diphenyl phosphate"]
dat[treatment == "4HCyclopenta def phenanthrene", treatment := "4-H-Cyclopenta(d,e,f)phenanthrene"]
dat[treatment == "6 Hydroxydopamine hydrochloride", treatment := "6-Hydroxydopamine hydrochloride"]
dat[treatment == "Auramine", treatment := "Auramine O"]
dat[treatment == "Benzo a pyrene", treatment := "Benzo(a)pyrene"]
dat[treatment == "Benzo e pyrene", treatment := "Benzo(e)pyrene"]
dat[treatment == "Benzo ghi perylene", treatment := "Benzo[g,h,i]perylene"]
dat[treatment == "Benzo k fluoranthene", treatment := "Benzo(k)fluoranthene"]
dat[treatment == "Bis 2 ethylhexyl tetrabromophthalate", treatment := "Bis(2-ethylhexyl) tetrabromophthalate"]
dat[treatment == "Carbamic acid", treatment := "Carbamic acid, butyl-, 3-iodo-2-propynyl ester"]
dat[treatment == "Carbamic acid butyl 3iodo2propynyl ester", treatment := "Carbamic acid, butyl-, 3-iodo-2-propynyl ester"]
dat[treatment == "D Glucitol", treatment := "D-Glucitol"]
dat[treatment == "Di 2ethylhexyl phthalate", treatment := "Di(2-ethylhexyl) phthalate"]
dat[treatment == "Dibenz ac anthracene", treatment := "Dibenz[a,c]anthracene"]
dat[treatment == "Dibenz ah anthracene", treatment := "Dibenz(a,h)anthracene"]
dat[treatment == "Firemaster", treatment := "Firemaster 550"]
dat[treatment == "L Ascorbic acid", treatment := "L-Ascorbic acid"]
dat[treatment == "Manganese tricarbonyl 12345 eta 1 methyl 24 cyclopentadien 1 yl", treatment := "Manganese, tricarbonyl[(1,2,3,4,5-.eta.)-1-methyl-2,4-cyclopentadien-1-yl]-"]
dat[treatment == "Tris 2chloroisopropyl phosphate", treatment := "Tris(2-chloroisopropyl) phosphate"]
dat[treatment == "tert Butylphenyl diphenyl phosphate", treatment := "tert-Butylphenyl diphenyl phosphate"]
dat[treatment == "Phenol isopropylated phosphate", treatment := "Phenol, isopropylated, phosphate (3:1)"] # this is the only phosphate in the spidmap that would make sense

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

# finally, run this:
source(file.path(scripts.dir, 'confirm_concs.R'))
dat <- confirm_concs(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,20))


# FINAL DATA CHECKS
# this section is to confirm that the data has been processed correctly
source(file.path(scripts.dir, 'dataset_checks.R'))
dataset_checks(dat)

# Any other plots or things to check?


# save dat and graphs
save(dat, file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
rm(dat)

if(save_notes_graphs) {
  sink() # close the txt log file
  graphics.off() # clear the plot history
}

cat("\nDone!\n")

# load(file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.RData")))
# testdat <- as.data.table(read.csv(file = file.path(root_output_dir, dataset_title, "output", paste0(dataset_title,"_longfile.csv")),stringsAsFactors = F))
# testdat[, .SD := lapply(.SD, as.character), .SDcols = c("spid","treatment","apid")]
# all.equal(dat, testdat, check.attributes = F)
# [1] "Column 'conc': Mean relative difference: 1"
# cool, so that is the only thing that is different - since I just updated the control well conc
