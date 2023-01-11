# comprehensive SPID assignment

library(data.table)
library(openxlsx)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))

# Gathering all SPIDs -----------------------------------------------------------------------------------
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs")
(all_spid_files <- list.files(getwd(), pattern = "\\.xlsx", full.names = T, recursive = F))
# [1] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/All Assays_list_toxcast_OECD 20190524.xlsx"        
# [2] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/Copy of EPA_ES204_EPA-Shafer_5_20200114_key.xlsx"  
# [3] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"   
# [4] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx"      
# [5] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_11118_EPA-Mundy_27FR_100mM_20150701_cg.xlsx"   
# [6] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx"
# [7] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_9238_EPA-Shafer_75_20180511.xlsx"              
# [8] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_ES202_EPA-Shafer_103_20191218_key.xlsx"        
# [9] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_ES203_EPA-Shafer_42_20200110_key.xlsx"         
# [10] "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs/EPA_ES204_EPA-Shafer_12_20200117_key.xlsx"

spidmap <- data.table()
# 
spid_filei <- "All Assays_list_toxcast_OECD 20190524.xlsx"
spidmapi <- as.data.table(read.xlsx(spid_filei,sheet="NFA Groups"))
head(spidmapi)
setnames(spidmapi, old = c(2,3), new = c("treatment","EPA_SAMPLE_ID"))
spidmapi[, EPA_SAMPLE_ID := paste0("EPAPLT0",EPA_SAMPLE_ID)]
spidmapi <- spidmapi[, .(treatment,EPA_SAMPLE_ID)]
spidmapi[, `:=`(filename = basename(spid_filei), primary_dataset = "DNTGF2019")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# 
spid_filei <- "Copy of EPA_ES204_EPA-Shafer_5_20200114_key.xlsx"
getSheetNames(spid_filei) # Sheet1
spidmapi <- as.data.table(read.xlsx(spid_filei))
# use_cols <- c('ALIQUOT_PLATE_BARCODE','ALIQUOT_WELL_ID','ALIQUOT_CONCENTRATION','ALIQUOT_CONCENTRATION_UNIT','TARGET_CONCENTRATION','TARGET_CONCENTRATION_UNIT','EPA_SAMPLE_ID','ALIQUOT_VOLUME','ALIQUOT_VOLUME_UNIT','ALIQUOT_SOLVENT','ALIQUOT_DATE','BOTTLE_ID','DTXSID','CASRN','PREFERRED_NAME')
head(spidmapi)
setnames(spidmapi, old = grep("CONCENTRATION",names(spidmapi), val = T), new = sub("CONCENTRATION","CONC",grep("CONCENTRATION",names(spidmapi),val=T)))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "Frank2017")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx
spid_filei <- "EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx"
getSheetNames(spid_filei) # "05-14-2015 183448"
spidmapi <- as.data.table(read.xlsx(spid_filei))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONC) # not all 20 - so this is probably real
setnames(spidmapi, old = c("dsstox_casrn","dsstox_preferred_name","dsstox_gsid"), new = c("CASRN","PREFERRED_NAME","gsid"))
spidmapi <- spidmapi[PREFERRED_NAME %in% c("Clotrimazole","1H,1H,2H,2H-Perfluorooctyl iodide","Perfluoroundecanoic acid")] # only want these 3 chem. The rest only used for MEA Acute
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "ToxCast2016")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_11118_EPA-Mundy_27FR_100mM_20150701_cg.xlsx
spid_filei <- "EPA_11118_EPA-Mundy_27FR_100mM_20150701_cg.xlsx"
getSheetNames(spid_filei) # "Mundy corrected map" "grulke_db_data"
cat(spid_filei)
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = "Mundy corrected map"))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONC) # not all 100 - so this is probably real?
setnames(spidmapi, old = c("casrn","preferred_name"), new = c("CASRN","PREFERRED_NAME"))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "OPP2015")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx
spid_filei <- "EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx"
cat(spid_filei)
getSheetNames(spid_filei) # "EPA_19563_12088"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = 1))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONC) # all 20 - from email, I know this is the target concentration
setnames(spidmapi, old = c("ALIQUOT_CONC","ALIQUOT_CONC_UNIT","casrn","preferred_name"), new = c("TARGET_CONC","TARGET_CONC_UNIT","CASRN","PREFERRED_NAME"))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "ToxCast2016")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_9238_EPA-Shafer_75_20180511.xlsx
spid_filei <- "EPA_9238_EPA-Shafer_75_20180511.xlsx"
cat(spid_filei)
getSheetNames(spid_filei) # "Worksheet1"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = 1))
head(spidmapi)
unique(spidmapi$Conc) # all 30. On the safe side, we'll say this is target conc
setnames(spidmapi, old = c("Conc","Conc_Unit","Amount","Amount_Unit"), new = c("TARGET_CONC","TARGET_CONC_UNIT","ALIQUOT_VOLUME","ALIQUOT_VOLUME_UNIT"))
setnames(spidmapi, old = names(spidmapi), new = toupper(names(spidmapi)))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = EPA_SAMPLE_ID, primary_dataset = "PFAS2018")] # PFAS compounds were left as spid in mea data
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_ES202_EPA-Shafer_103_20191218_key.xlsx
spid_filei <- "EPA_ES202_EPA-Shafer_103_20191218_key.xlsx"
cat(spid_filei)
getSheetNames(spid_filei) # "Sheet1"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = 1))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONCENTRATION) # all 20. But, target conc col is present as well, so I assume these are different
setnames(spidmapi, old = grep("CONCENTRATION",names(spidmapi), val = T), new = sub("CONCENTRATION","CONC",grep("CONCENTRATION",names(spidmapi),val=T)))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "Frank2017")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_ES203_EPA-Shafer_42_20200110_key.xlsx
spid_filei <- "EPA_ES203_EPA-Shafer_42_20200110_key.xlsx"
cat(spid_filei)
getSheetNames(spid_filei) # "Sheet1"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = 1))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONCENTRATION) # all 100. But, target conc col is present as well, so I assume these are different
setnames(spidmapi, old = grep("CONCENTRATION",names(spidmapi), val = T), new = sub("CONCENTRATION","CONC",grep("CONCENTRATION",names(spidmapi),val=T)))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "Frank2017")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# EPA_ES204_EPA-Shafer_12_20200117_key.xlsx
spid_filei <- "EPA_ES204_EPA-Shafer_12_20200117_key.xlsx"
cat(spid_filei)
getSheetNames(spid_filei) # "Sheet1"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = 1))
head(spidmapi)
unique(spidmapi$ALIQUOT_CONCENTRATION) # all 100. But, target conc col is present as well, so I assume these are different
setnames(spidmapi, old = grep("CONCENTRATION",names(spidmapi), val = T), new = sub("CONCENTRATION","CONC",grep("CONCENTRATION",names(spidmapi),val=T)))
spidmapi[, `:=`(filename = basename(spid_filei), treatment = PREFERRED_NAME, primary_dataset = "Frank2017")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

# Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx
spid_filei <- "Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"
getSheetNames(spid_filei) # "NeuroTox 91 Cmpds" "Sheet1"
spidmapi <- as.data.table(read.xlsx(spid_filei, sheet = "NeuroTox 91 Cmpds"))
head(spidmapi)
unique(spidmapi$`Conc..(mM)`) # not all 20, so I think this is real
setnames(spidmapi, old = c("Conc..(mM)","Molecular.Weight","CAS","Plate.Position","SPID","Chemical.Name"), new = c("ALIQUOT_CONC","MW","CASRN","LOCATION","EPA_SAMPLE_ID","treatment"))
spidmapi <- spidmapi[!is.na(EPA_SAMPLE_ID)] # removing empty rows at the bottom
spidmapi[, `:=`(filename = basename(spid_filei), primary_dataset = "NTP91")]
spidmap <- rbind(spidmap, spidmapi, fill = T)

rm(spidmapi)

# clean up
spidmap[, CASRN := sub("'","",CASRN)]
spidmap[, CASRN := sub("'","",CASRN)]

# I want to fill in teh missing preferred name, casn, and dtxsid
# can get that all from "chemical" table. But need chid first
require(RMySQL)
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='invitrodb',host = Sys.getenv('INVITRODB_HOST_RO'))
query_term <- paste0("SELECT * FROM sample WHERE spid IN('",paste(unique(spidmap$EPA_SAMPLE_ID),collapse="','",sep=""),"');")
sample_info <- dbGetQuery(con, query_term)
query_term <- paste0("SELECT * FROM chemical WHERE chid IN('",paste(unique(sample_info$chid),collapse="','",sep=""),"');")
chemical_info <- dbGetQuery(con, query_term)
dbDisconnect(con)
sample_info <- merge(sample_info, chemical_info, by = "chid")
write.csv(sample_info, file.path(root_output_dir,"sampleinfo_invitrodb_oct22_2020.csv"), row.names = F)

spidmap <- merge(spidmap, sample_info, by.x = "EPA_SAMPLE_ID", by.y = "spid", all.x = T)
spidmap[!is.na(PREFERRED_NAME) & chnm != PREFERRED_NAME, .(chnm, PREFERRED_NAME)] # 6 cases...
spidmap[!is.na(CASRN) & casn != CASRN, .(casn, CASRN, chnm, PREFERRED_NAME)] # empty
spidmap[, CASRN := NULL]
spidmap[!is.na(DTXSID) & dsstox_substance_id != DTXSID, .(casn, chnm, PREFERRED_NAME)] # empty, cool!
spidmap[, DTXSID := NULL]
spidmap[is.na(PREFERRED_NAME), PREFERRED_NAME := chnm]

# names consistent for each spid?
spidmap[, .(length(unique(PREFERRED_NAME))) ,by = .(EPA_SAMPLE_ID)][V1 !=1] # empty, cool

# Assign SPIDs ------------------------------------------------
# # first, confirm I have a spidmap for all spid's preset in alldat
# alldat[spid %in% setdiff(alldat[wllt == "t", unique(spid)], unique(spidmap$EPA_SAMPLE_ID)), .(unique(treatment)), by = c("spid","dataset")]
# # empty

# check for duplicated spids
duplicated_spids <- spidmap[, .N, by = .(EPA_SAMPLE_ID)][ N != 1, c(EPA_SAMPLE_ID)]
spidmap[EPA_SAMPLE_ID %in% duplicated_spids]
# duplicates from NT91 have multiple Locations. I'll condense those
spidmap[, LOCATION := paste0(sort(unique(LOCATION)),collapse=","), by = .(EPA_SAMPLE_ID)]
# all 5 compounds from Copy of EPA_ES204_EPA-Shafer_5_20200114_key.xlsx are duplicated in EPA_ES204_EPA-Shafer_12_20200117_key.xlsx
spidmap <- spidmap[filename != "Copy of EPA_ES204_EPA-Shafer_5_20200114_key.xlsx"]
# EX000323 has multiple conc's listed. I will let invitrodb sort that out
spidmap[EPA_SAMPLE_ID == "EX000323", ALIQUOT_CONC := NA_real_]
spidmap <- unique(spidmap)
spidmap[, .N, by = .(EPA_SAMPLE_ID)][ N != 1, c(EPA_SAMPLE_ID)] # character()


# Prepare "treatment" col in spidmap to match MEA treatment names ----------------
spidmap[, notes := ""]

# Frank2017 - use the treatment names from Table1 (these casn's reflect what was actually tested, even where some treatment naems are wrong)
table1 <- as.data.table(read.xlsx(file.path(root_output_dir,"Frank2017","Table1_CompoundList_dtxsid_updated.xlsx"),sheet = 1))
setnames(table1, old = c("CAS.No.","Compound.name.(abbreviation)"), new = c("casn","treatment"))
table1 <- table1[!is.na(treatment)]
table1[treatment == "Acetaminophen", Solvent.used := "Water"] # fixing this, based on lab notebook 20141203
spidmap[primary_dataset == "Frank2017" & casn %in% unique(table1$casn), c("treatment") := .(table1[match(spidmap[primary_dataset == "Frank2017" & casn %in% unique(table1$casn),casn], table1$casn), c(treatment)])]
spidmap[primary_dataset == "Frank2017" & casn %in% unique(table1$casn), c("Solvent.used") := .(table1[match(spidmap[primary_dataset == "Frank2017" & casn %in% unique(table1$casn),casn], table1$casn), c(Solvent.used)])]

# From ToxCast, the Plate ID indicates that Clotrimazole is the correct treatment
spidmap[treatment == "Clotrimazole", `:=` (treatment = "1,1,2,2-Tetrahydroperfluoro-1-decanol",
                                           notes = paste0("Lab Notebook ToxCast Plate ID indicates TP0001411 E01"))] # matching how the compound is named in MEA data

# DNTGF2019
# I manually created this file to map the spids, because of the multiple Glufosinate-P's and other diff's in treatment names.
(gf_spids <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/glufosinates_spidmap.xlsx", sheet = 1)))
# Compound.name.in.MEA.data                                                                          Compound.name.in.SPID.map.file     SPID
# 1:      Glufosinate Ammonium                                                                                    Glufosinate-ammonium EX000371
# 2: 4 Glufosinate-P Technical                                                                                           Glufosinate-P EX000372
# 3: 3 Glufosinate-P Technical                                                                                           Glufosinate-P EX000373
# 4:    L-Glufosinate Ammonium                                                                                  Glufosinate-P ammonium EX000374
# 5:                Glyphosate                                                                                              Glyphosate EX000408
# 6:                Loperamide 4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride EX000411
# replace the "treatment" column in spidmap with the "Compound.name.in.MEA.data" from gf_spids
spidmap <- merge(spidmap, gf_spids, by.x = c("PREFERRED_NAME","EPA_SAMPLE_ID"), by.y = c("Compound.name.in.SPID.map.file","SPID"), all.x = TRUE)
spidmap[!is.na(Compound.name.in.MEA.data), treatment := Compound.name.in.MEA.data]
spidmap[, Compound.name.in.MEA.data := NULL]

# a few more mea_treatment_names to udpate from all datasets
# update mea_treatment_name and notes notes from table to master spid map
name_map1 <- as.data.table(read.csv(file.path(root_output_dir, "supplemental_mea_treatment_name_map.csv"), stringsAsFactors = F))
spidmap_test <- merge(spidmap, name_map1, by.x = c("treatment"), by.y = c("spid_treatment_name"), all = T, suffixes = c("",".add"))
# not sure why there are 4 added rows now (516 to 523)
spidmap_test[is.na(mea_treatment_name), mea_treatment_name := treatment]
spidmap_test[!is.na(notes.add), notes := paste0(notes, notes.add)]
spidmap_test[, notes.add := NULL]

# spidmap[, mea_treatment_name_updates := name_map1[match(spidmap$PREFERRED_NAME, name_map1$spid_treatment_name), mea_treatment_name]]
# spidmap[!is.na(mea_treatment_name_updates), treatment := mea_treatment_name_updates]
# spidmap[, notes := paste0(notes, name_map1[match(spidmap$PREFERRED_NAME, name_map1$spid_treatment_name), ifelse(is.na(notes),"",notes)])]


# save the spidmap
write.csv(spidmap, file.path(root_output_dir, "master_spidmap_mea_nfa.csv"), row.names = F)
