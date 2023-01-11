# comprehensive SPID assignment

library(data.table)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))
spidmap <- as.data.table(read.csv(file.path(root_output_dir,"master_spidmap_mea_nfa.csv"), stringsAsFactors = F))


# # check if any trt's don't have any match in the spidmap -------------------------------------------
alldat <- get_latest_dat()
# Loading longfile.Rdata for...
# Brown2014
# DNTGF2019
# Frank2017
# NTP91
# OPP2015
# PFAS2018
# ToxCast2016

# while I am transitition the run_me setup, 
# I will restore the original mea treatment names before I updated them in the run_me
name_map1 <- as.data.table(read.csv(file.path(root_output_dir, "supplemental_mea_treatment_name_map.csv"), stringsAsFactors = F))

# checking for unintentional multiple matching...
alldat[treatment %in% name_map1$spid_treatment_name, .(length(unique(dataset)), paste0(sort(unique(dataset)),collapse=","), paste0(sort(unique(treatment)),collapse=",")), by = .(spid)][V1 != 1]
# spid V1                V2                                                                  V3
# 1: EX000361  2   Frank2017,NTP91 Tris (2-chloroethyl) phosphate (TCEP),Tris(2-chloroethyl) phosphate
# 2: EX000362  2 NTP91,ToxCast2016                                                         Valinomycin
# TCEP - separate names, this is all good
# Valinomycin was "Valinomycin - NTP" in ToxCast dataset only. I will change this back after do the switch

# get the mea_treatment_name
alldat[, mea_treatment_name := name_map1[match(alldat$treatment, name_map1$spid_treatment_name), mea_treatment_name]]
alldat[!is.na(mea_treatment_name)]
alldat[dataset == "NTP91" & grepl("Valinomycin",mea_treatment_name), mea_treatment_name := "Valinomycin"]
alldat[!is.na(mea_treatment_name), treatment := mea_treatment_name]
alldat[, mea_treatment_name := NULL]

# I could just stop this, and say
# - I am fairly confident that I have teh correct spids,
# I will just update loperamide, toxcast clotrimazole

# RESUME HERE:
# - either remove all compound renaming from run_me's and recreate each dat,
# - OR add original mea treatment names back to alldat
# - then proceed with checking that each treatment in alldat has a treatment in spidmap
# - then get creative with how to assign where there are multiple spids for a treatment! (thinking use "dataset".. but will that be enough?)
# - then make the final summary data file you were thinking of

trts <- alldat[wllt == "t", unique(treatment)]
(missing_trts <- setdiff(trts, spidmap$treatment))
# [1] "Bisindolylmaleimide I"
# [2] "L-Domoic acid"
# [3] "Sodium orthovanadate"
# [4] "Triphenyl phosphate (TPHP)"
# [5] "Tris (2-chloroethyl) phosphate (TCEP)"
# [6] "4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride"
alldat[treatment %in% missing_trts, unique(dataset), by = "treatment"]
# treatment        V1
# 1:                 Bisindolylmaleimide I Brown2014
# 2:                         L-Domoic acid Brown2014
# 3:                  Sodium orthovanadate Brown2014
# 4:            Triphenyl phosphate (TPHP) Frank2017
# 5: Tris (2-chloroethyl) phosphate (TCEP) Frank2017
spidmap[PREFERRED_NAME %in% missing_trts]
# may have to update once integrate all updates that were done in the run_me files ...........................
alldat[treatment == "4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride", treatment := "Loperamide"]
alldat[treatment == "L-Domoic acid", treatment := "Domoic acidLoperamide"]

# options:
# - rename these compounds in MEA data
# - add row in spidmap for these compounds...
# I'm going with option 2 for now.

# it would be nice if every

alldat[updated_trt %in% setdiff(trts, unique(spidmap$treatment)), .(length(unique(updated_trt))), by = .(dataset)]
# dataset V1
# 1: DNTGF2019  4
alldat[updated_trt %in% setdiff(trts, unique(spidmap$treatment)), .(unique(updated_trt))]
# V1
# 1: 3 Glufosinate-P Technical
# 2: 4 Glufosinate-P Technical
# 3:      Glufosinate Ammonium
# 4:    L-Glufosinate Ammonium
# these will be addressed separately
gf_spids <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/glufosinates_spidmap.xlsx", sheet = 1))

# Flag SPIDs that we won't need, to simplify next step
use_trts <- c(alldat$updated_trt, gf_spids$Compound.name.in.SPID.map.file)
spidmap[, using := treatment %in% use_trts]

# note where there are multiple spids for 1 compound in a given dataset
spidmap[, .(length(unique(EPA_SAMPLE_ID))), by = .(dsstox_substance_id)][V1 != 1]


# sort out mapping where there are multiple spids for a given chem
spidmap[treatment %in% treatment, .(.N), by = .(dsstox_substance_id, casn, chnm, treatment)][N != 1]



# -	Then generate a big reference table: name in MEA, preferred name, casrn, spid, spid_source, dataset, cultures, documentation/notes?
# plus can even add dilution sheet info and references here (manually, whenever you happen to do that.)


# how deal with compounds that are named differently in diff datasets?
# I think add another row to spidmap file with same spid, etc., but diff treatment naem and dataset


# deprecated --------------------------------------------
# Fixes to alldat ------------------------------------------------------------
# Frank2017 - ignore treatment names, use CASN to MEA compound name map from table 1
table1 <- as.data.table(read.xlsx(file.path(root_output_dir,"Frank2017","Table1_CompoundList_dtxsid_updated.xlsx"),sheet = 1))
setnames(table1, old = c("CAS.No.","Compound.name.(abbreviation)"), new = c("casn","treatment"))
table1 <- table1[!is.na(treatment)]
table1[treatment == "Acetaminophen", Solvent.used := "Water"] # fixing this, based on lab notebook 20141203
alldat <- merge(alldat, table1[, .(treatment, Solvent.used, casn)], by = "treatment", all.x = T)
alldat[, updated_trt := spidmap[match(alldat$casn, spidmap$casn),PREFERRED_NAME]]

# From ToxCast, the Plate ID indicates that Clotrimazole is the correct treatment
alldat[treatment == "1,1,2,2-Tetrahydroperfluoro-1-decanol", updated_trt := "Clotrimazole"]

# for all others, assume treatment name from MEA data will map correctly
alldat[is.na(updated_trt), updated_trt := treatment]


