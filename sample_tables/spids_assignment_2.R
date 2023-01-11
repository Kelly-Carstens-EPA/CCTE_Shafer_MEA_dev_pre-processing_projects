library(data.table)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))
alldat <- get_latest_dat()

# a few quick checks-------------------------------
summ <- alldat[wllt == "n", .(.N), by = .(spid, conc, apid, rowi, coli)]
summ[, .N, by = .(spid, conc)]
alldat[wllt == "n", .(.N/length(unique(alldat$acsn))), by = .(spid, conc)]

# this plate should not be included, based Archive/../extra notes 2.pdf
alldat[grepl("1141-22",apid)] # empty, check.

# did I assign a conc for all, esp controls?
alldat[is.na(conc)]
alldat[length(conc) == 0]
alldat[conc == ""]
# all empty

# create the trt_table to show all treatments, spids, etc.---------------------------------------------------
trt_table <- alldat[wllt == "t", .(cultures = paste0(sort(unique(sub("_.*$","",apid))),collapse=",")), by = .(treatment, spid, dataset)]

# step 1: Get the original mea treatment names
# (for most datasets, I already updated the treatment name to what I think is in the spidmap. I am getting the original treatment names back)
trt_table[, mea_treatment_name := treatment]
setnames(trt_table, old = "treatment", new = "updated_treatment_name")

# # this one compound from ToxCast
# trt_table[mea_treatment_name == "1,1,2,2-Tetrahydroperfluoro-1-decanol", `:=`(mea_treatment_name = "1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411",
#                                                                               updated_treatment_name = "1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411")]

# map from several datasets
name_map1 <- as.data.table(read.csv(file.path(root_output_dir, "supplemental_mea_treatment_name_map.csv"), stringsAsFactors = F))
trt_table <- merge(trt_table, name_map1, by.x = c("updated_treatment_name","dataset"), by.y = c("spid_treatment_name","dataset"), all.x = T, suffixes = c("",".file"))
trt_table[!is.na(mea_treatment_name.file), mea_treatment_name := mea_treatment_name.file]
trt_table[, mea_treatment_name.file := NULL]

# map for glufosinate compounds
(gf_spids <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/DNTGF2019/glufosinates_spidmap.csv", stringsAsFactors = F)))
# Compound.name.in.MEA.data                                                                          Compound.name.in.SPID.map.file     SPID
# 1:      Glufosinate Ammonium                                                                                    Glufosinate-ammonium EX000371
# 2: 4 Glufosinate-P Technical                                                                                           Glufosinate-P EX000372
# 3: 3 Glufosinate-P Technical                                                                                           Glufosinate-P EX000373
# 4:    L-Glufosinate Ammonium                                                                                  Glufosinate-P ammonium EX000374
# 5:                Glyphosate                                                                                              Glyphosate EX000408
# 6:                Loperamide 4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride EX000411
# add the compound.name.in.spid.map.file as teh updated_treatment_name
gf_spids[, dataset := "DNTGF2019"]
trt_table[mea_treatment_name %in% gf_spids$Compound.name.in.MEA.data]
trt_table <- merge(trt_table, gf_spids[, .(Compound.name.in.MEA.data,Compound.name.in.SPID.map.file,dataset)],
                   by.x = c("mea_treatment_name","dataset"), by.y = c("Compound.name.in.MEA.data","dataset"), all.x = T)
trt_table[!is.na(Compound.name.in.SPID.map.file), updated_treatment_name := Compound.name.in.SPID.map.file]
trt_table[, Compound.name.in.SPID.map.file := NULL]

# So with Frank (and with the 2 GF compounds),
# the updated treatment name will not actually map to a unique, correct preferred_name in any spidmap
# for frank, I could set the updated_treatment_name to the preferred_name corresponding to the CASN collected in table1
# I'm going to see if I can avoid doing that for now
# let this be mroe descriptive than a new way to assign the spids


# Step 2: Where the treatment name has not been corrected, get the updated_treatment_name (brown)
trts <- unique(trt_table$updated_treatment_name)
intersect(trts, name_map1$mea_treatment_name)
# [1] "Bisindolymaleimide 1" "Domoic Acid"          "Loperamide"           "Sodium Orthovanadate" "Valinomycin"
trt_table <- merge(trt_table, name_map1, by.x = c("updated_treatment_name","dataset"), by.y = c("mea_treatment_name","dataset"), suffixes = c("",".add"), all.x = T)
trt_table[!is.na(spid_treatment_name)]
trt_table[!is.na(spid_treatment_name), `:=`(updated_treatment_name = spid_treatment_name,
                                            notes = paste0(ifelse((is.na(notes)|notes==notes.add),"",notes),notes.add))]
trt_table[, c("spid_treatment_name","notes.add") := NULL]
trt_table

# check loperamide
trt_table[mea_treatment_name == "Loperamide"] # all 4 datasets present.

# check clotrimazole
trt_table[updated_treatment_name == "Clotrimazole"] # check, present


# Step 3: get the spids for Brown2014(since I did not rename in the run_me's)
spidmap <- as.data.table(read.csv(file.path(root_output_dir, "master_spidmap_mea_nfa.csv"),stringsAsFactors = F))
spidmap[primary_dataset == "NTP91", PREFERRED_NAME := treatment] # want PREFERRED_NAME to correspond to how the name appears in spidmap file, not in invitrodb
trt_table[spid == mea_treatment_name & dataset != "PFAS2018", spid := NA_character_]
get_spids_for_treatments <- trt_table[is.na(spid), unique(updated_treatment_name)]
(use_spid_info <- spidmap[PREFERRED_NAME %in% get_spids_for_treatments, .(PREFERRED_NAME, EPA_SAMPLE_ID, filename, ALIQUOT_PLATE_BARCODE, Supplier, Lot.Number, Solvent.used)])
use_spid_info[, .N, by = .(PREFERRED_NAME)][N > 1] # 2 spids for Acetaminophen present
use_spid_info <- use_spid_info[!(PREFERRED_NAME == "Acetaminophen" & filename == "Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx")]
trt_table <- merge(trt_table, use_spid_info[, .(PREFERRED_NAME, EPA_SAMPLE_ID)], by.x = "updated_treatment_name", by.y = "PREFERRED_NAME", all.x = T)
trt_table[is.na(spid)] # just the 6 that I want to replace
trt_table[is.na(spid), spid := EPA_SAMPLE_ID]
trt_table[, EPA_SAMPLE_ID := NULL]

# clotrimazole spid confirmation
trt_table[updated_treatment_name == "Clotrimazole"]

# get all the desired columns (preferred name, casn, relevant info, notes, etc.)
trt_table2 <- merge(trt_table, spidmap[, .(PREFERRED_NAME, EPA_SAMPLE_ID, filename, ALIQUOT_PLATE_BARCODE, ALIQUOT_WELL_ID, ALIQUOT_SOLVENT, 
                             ALIQUOT_DATE, BOTTLE_ID, ALIQUOT_VIAL_BARCODE,LOCATION, MW, Supplier, Lot.Number, chid, casn, chnm, 
                             dsstox_substance_id, notes, Solvent.used)], by.x = "spid", by.y = "EPA_SAMPLE_ID", all.x = T, suffixes = c("",".add"))
trt_table2[notes.add != "", notes := paste0(ifelse(is.na(notes),"",notes), "; ",notes.add)]
trt_table2[, notes.add := NULL]

# confirm spid-mapping, add notes where needed
# (where new spid matches current), or doesn't...
trt_table2[PREFERRED_NAME != updated_treatment_name & is.na(as.numeric(updated_treatment_name)), .(updated_treatment_name, mea_treatment_name, PREFERRED_NAME, dataset)]
# these are all from Frank2017
# let's bring back table1, verify that the casn's match
table1 <- as.data.table(read.csv(file.path(root_output_dir,"Frank2017","Table1_CompoundList_dtxsid_updated.csv"),stringsAsFactors = F))
setnames(table1, old = c("CAS.No.","Compound.name..abbreviation.","Changes.Notes"), new = c("casn","treatment","notes"))
table1 <- table1[!is.na(treatment)]
table1[treatment == "Acetaminophen", Solvent.used := "Water"] # fixing this, based on lab notebook 20141203
trt_table2 <- merge(trt_table2, table1[, .(treatment, casn, dataset = "Frank2017", notes)], 
                    by.x = c("updated_treatment_name","dataset"), by.y = c("treatment","dataset"), suffixes = c("",".tb1"), all.x = T)
trt_table2[!is.na(casn.tb1) & casn != casn.tb1] # empty - so all casn's match!
trt_table2[!is.na(casn.tb1), notes := paste0("Table1 links updated_treatment_name to correct casn; ", notes.tb1, ifelse(is.na(notes),"",notes))]
trt_table2[, c("casn.tb1","notes.tb1") := NULL]

# clean it up a bit before save
trt_table2[Solvent.used != ALIQUOT_SOLVENT]

# save this!
write.csv(trt_table2[, .(dataset, mea_treatment_name, updated_treatment_name, PREFERRED_NAME, casn, dsstox_substance_id, spid, ALIQUOT_PLATE_BARCODE, filename, notes)][order(dataset, mea_treatment_name)], 
          file.path(root_output_dir, "sample_tables","mea_nfa_treatment_spid_map_oct23_2020.csv"), row.names = F)
write.csv(trt_table2[, .(dataset, mea_treatment_name, updated_treatment_name, PREFERRED_NAME, casn, dsstox_substance_id, spid, ALIQUOT_PLATE_BARCODE, filename, notes,
                       cultures, ALIQUOT_WELL_ID, ALIQUOT_SOLVENT, ALIQUOT_DATE, BOTTLE_ID, ALIQUOT_VIAL_BARCODE, LOCATION, MW, Supplier, Lot.Number, chid, chnm, Solvent.used)][order(dataset, mea_treatment_name)],
          file = file.path(root_output_dir,"sample_tables","mea_nfa_treatment_spid_map_verbose_oct23_2020.csv"), row.names = F)


# GOAL #2: Tabulate where there are multiple spid options --------------------------------
# and document:
# Acetaminophen for Brown2014: Checked that Supplier for NTP91 did not match my info
# also where there are salt options, e.g.
# naloxone, Naloxone hydrochloride dihydrate
# cadmium, cadmium(II)...
dtxsid_multi_spid <- spidmap[, .(length(unique(EPA_SAMPLE_ID))), by = .(dsstox_substance_id)][V1 != 1, unique(dsstox_substance_id)]
rep_info <- merge(spidmap[dsstox_substance_id %in% intersect(dtxsid_multi_spid, trt_table2$dsstox_substance_id), .(dsstox_substance_id, EPA_SAMPLE_ID, filename, ALIQUOT_PLATE_BARCODE)],
      trt_table2[dsstox_substance_id %in% dtxsid_multi_spid, .(mea_treatment_name, dataset, dsstox_substance_id, spid)], by = "dsstox_substance_id", all.x = T)
rep_info <- rep_info[order(spid)]
setnames(rep_info, old = c("spid","EPA_SAMPLE_ID"), new = c("spid_chosen","available_spids"))
rep_info <- rep_info[, .(mea_treatment_name, dataset, dsstox_substance_id, spid_chosen, available_spids, filename, ALIQUOT_PLATE_BARCODE)]
rep_info[, .(length(unique(available_spids))), by = .(spid_chosen)][V1 != 1] # huh, ... okay
rep_info2 <- rep_info[is.na(as.numeric(mea_treatment_name))][order(spid_chosen, available_spids)]

# wherever there only 2 options are NTP and EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx, it is fine
rep_info2[, verification := ""]

# OPP, NTP, and ToxCast are fairly straightforward
rep_info2[verification == "" & dataset == "OPP2015", verification := "all OPP2015 spids from 'EPA_11118_EPA-Mundy_27FR_100mM_20150701_cg.xlsx'"]
rep_info2[verification == "" & dataset == "NTP91", verification := "all NTP91 spids taken from 'Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx'"]
rep_info2[dataset == "ToxCast2016" & filename == "EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx" & spid_chosen != available_spids]
rep_info2[verification == "" & dataset == "ToxCast2016", verification := "all ToxCast2016 spids default to 'EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx'"]

# for 4 Glufosinate-P Technical adn 3 Glufosinate-P Technical
rep_info2[verification == "" & dsstox_substance_id == "DTXSID401020544", verification := "correct spid identified by aliquot well"] 

# FRank2017
rep_info2[dataset == "Frank2017" & grepl("(Shafer_103)|(Shafer_12)",filename), length(unique(filename)), by = .(spid_chosen)] # all only 1
rep_info2[dataset == "Frank2017", length(unique(filename)), by = .(spid_chosen)][V1 > 2, unique(spid_chosen)]
rep_info2[spid_chosen %in% c("EX000384", "EX000400", "EX000421", "EX000522")] # def not OPP2015 or DNT spid's
rep_info2[dataset == "Frank2017" & filename == "EPA_12088_EPA-Shafer_96misc_75ul_20160826_key.xlsx"] # only Thiouracil
rep_info2[dataset == "Frank2017" & grepl("(Shafer_103)|(Shafer_12)|(Shafer_42)",filename), length(unique(available_spids)), by = .(spid_chosen, mea_treatment_name)][V1 != 1]
# spid_chosen mea_treatment_name V1
# 1:    EX000378  Chlorpyrifos oxon  5
# 2:    EX000395      Dexamethasone  2
# 3:    EX000445       Methotrexate  2
rep_info2[mea_treatment_name == "Chlorpyrifos oxon" & dataset == "Frank2017", verification := "taken from 4th sample in 'EPA_ES202_EPA-Shafer_103_20191218_key.xlsx'"]
rep_info2[mea_treatment_name == "Dexamethasone" & dataset == "Frank2017", verification := "our best guess is 2nd sample in 'EPA_ES202_EPA-Shafer_103_20191218_key.xlsx'"]
rep_info2[mea_treatment_name == "Methotrexate" & dataset == "Frank2017", verification := "our best guess is 2nd sample in 'EPA_ES202_EPA-Shafer_103_20191218_key.xlsx'"]
rep_info2[verification == "" & dataset == "Frank2017", verification := "all Frank2017 spids default to 'EPA_ES202_EPA-Shafer_103_20191218_key.xlsx' or 'EPA_ES204_EPA-Shafer_12_20200117_key.xlsx'"]
# verify that none from Frank2017 should be the NTP91 list, or Thiouracil could also be from 96misc toxcast list

# Brown, PFAS
rep_info2[verification == "" & dataset == "Brown2014", verification := "Lot # in lab notebook does not match lot # in Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"]
rep_info2[verification == "" & dataset == "PFAS2018", verification := "Lot # in lab notebook does not match lot # in Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx"]

# verify these general rules.
write.csv(rep_info2, file.path(root_output_dir,"sample_tables","all_compounds_with_multiple_spid_options.csv"), row.names = F)

# checkout salts...
spidmap[, salt_guess := sub("[ \\(-].*$","",chnm)]
potential_salts <- spidmap[, .(length(unique(dsstox_substance_id))), by = .(salt_guess)][V1 != 1 & !grepl("[[:digit:]]",salt_guess) & nchar(salt_guess) > 2, unique(salt_guess)]
spidmap[salt_guess %in% potential_salts]
for (potential_salt in potential_salts) {
  cat("\n",potential_salt,"\n")
  cat(spidmap[salt_guess %in% potential_salt, c(chnm)], sep = " ")
}
# relevant results:
# Cadmium 
# Cadmium chloride Cadmium chloride Cadmium(II) chloride hydrate (2:5)
# Chlordiazepoxide 
# Chlordiazepoxide Chlordiazepoxide hydrochloride
# Chlorpromazine 
# Chlorpromazine Chlorpromazine hydrochloride
# Cytarabine 
# Cytarabine Cytarabine hydrochloride
# Fluoxetine 
# Fluoxetine Fluoxetine hydrochloride
# Heptachlor 
# Heptachlor Heptachlor Heptachlor Heptachlor epoxide B
# Lead 
# Lead acetate Lead chloride Lead(II) acetate trihydrate Lead(II) acetate trihydrate
# Manganese 
# Manganese Manganese dichloride Manganese(II) acetate
# Naloxone 
# Naloxone Naloxone hydrochloride dihydrate
# Phenobarbital 
# Phenobarbital Phenobarbital sodium Phenobarbital sodium
# Terbutaline 
# Terbutaline Terbutaline hemisulfate

# might have salts the other way...
# Methyl 
# Methyl 2H,2H,3H,3H-perfluoroheptanoate Methyl mercury(II) cation Methyl parathion Methyl perfluoroethyl ketone Methyl perfluorohexanoate
# Potassium 
# Potassium perfluorobutanesulfonate Potassium perfluorohexanesulfonate Potassium perfluorohexanesulfonate Potassium perfluorooctanesulfonate Potassium perfluorooctanesulfonate
# Sodium 
# Sodium L-glutamate hydrate Sodium arsenite Sodium benzoate Sodium chlorite Sodium fluoride Sodium orthovanadate Sodium saccharin hydrate Sodium saccharin hydrate Sodium valproate Sodium valproate

filtered_salts <- text2[seq(1,21, by=2)] # from first paragraph of "relevant results"
filtered_salts <- sub("# ","",filtered_salts)
filtered_salts <- sub(" ","",filtered_salts)
filtered_salt_forms <- text2[seq(2,22, by=2)]

# see if these in trt_table2
trt_table2[, salt_guess := sub("[ \\(-].*$","",chnm)]
trt_table2[salt_guess %in% filtered_salts, unique(dsstox_substance_id)] # 16 instances
salts_table <- merge(trt_table2[, .(mea_treatment_name, chnm_chosen = chnm, dataset, updated_treatment_name, spid, salt_guess, cultures)], 
              spidmap[salt_guess %in% filtered_salts, .(salt_guess, chnm, PREFERRED_NAME, EPA_SAMPLE_ID, filename, casn, dsstox_substance_id)],
              by = c("salt_guess"))
salts_table[dataset != "DNTGF2019"]
salts_table <- salts_table[dataset != "DNTGF2019", .(dataset, mea_treatment_name, chnm_chosen, spid_chosen = spid, salt_guess, similar_compounds = chnm, spid = EPA_SAMPLE_ID, casn, dsstox_substance_id, filename, cultures = sub(",",", ",cultures))]
write.csv(salts_table, file.path(root_output_dir,"sample_tables","potential_alternative_salt_forms.csv"), row.names = F)

# these gives 3 options as well
spidmap[grepl("[Vv]alpro",chnm)] # after chat with Tim - the compounds named 'Valproate' and 'Valproic acid' are both refering to 'Sodium valproate' with cas number 1069-66-5
spidmap[grepl("[Mm]ercur",chnm)] # but, I have verified the CASN for Methylmercurcy in the lab notebook for Frank2017, so no worries!!

# deal:
# I think that the general rules that I used a pretty good
# I think just send an FYI to Tim that there are other salt forms out there... try ot make it clear what you did map.
# the hitch:
# I want to be able to say: you want this compound, okay! give me the mea_treatment_name, and I will get it!
# but right now, the treatment name in alldat is the updated_treatment_name for some, and the mea_Treatment_name for other
# need to make that homogeneous (probs have both!!)
# update here or in run_me'S?

# spids used in multiple datasets
trt_table2[, .(length(unique(dataset))), by = .(spid)][V1 > 1] # 10 spids

# # checking for unintentional multiple matching...
# alldat[treatment %in% name_map1$spid_treatment_name, .(length(unique(dataset)), paste0(sort(unique(dataset)),collapse=","), paste0(sort(unique(treatment)),collapse=",")), by = .(spid)][V1 != 1]
# # spid V1                V2                                                                  V3
# # 1: EX000361  2   Frank2017,NTP91 Tris (2-chloroethyl) phosphate (TCEP),Tris(2-chloroethyl) phosphate
# # 2: EX000362  2 NTP91,ToxCast2016                                                         Valinomycin
# # TCEP - separate names, this is all good
# # Valinomycin was "Valinomycin - NTP" in ToxCast dataset only. I will change this back after do the switch
# 
# # get the mea_treatment_name
# alldat[, mea_treatment_name := name_map1[match(alldat$treatment, name_map1$spid_treatment_name), mea_treatment_name]]
# alldat[!is.na(mea_treatment_name)]
# alldat[dataset == "NTP91" & grepl("Valinomycin",mea_treatment_name), mea_treatment_name := "Valinomycin"]
# alldat[!is.na(mea_treatment_name), treatment := mea_treatment_name]
# alldat[, mea_treatment_name := NULL]