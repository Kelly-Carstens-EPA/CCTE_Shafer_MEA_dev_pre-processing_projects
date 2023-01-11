library(data.table)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"

trt_table <- read.csv(file = file.path(root_output_dir,"sample_tables","mea_nfa_treatment_spid_map_oct23_2020.csv"), stringsAsFactors = F)
setDT(trt_table)
htp_log <- read.csv(file.path(root_output_dir,"Frank2017","Copy of HTP_Chem_Log_cg2.csv"), stringsAsFactors = F)
setDT(htp_log)
htp_log <- htp_log[, .(SPID, Chemical, X.internal.tracking..., CAS..., Source, Catalog.., Lot.., Solvent, Notes)]

trt_table[dataset == "Frank2017" & mea_treatment_name %in% htp_log$Chemical] # 32 match
trt_table[dataset == "Frank2017" & updated_treatment_name %in% htp_log$Chemical] # 34

setdiff(trt_table[dataset == "Frank2017", unique(spid)], unique(htp_log$SPID))
# [1] "EX000516" "EX000521" "EX000475" "EX000522" "EX000523" "EX000487" "EX000524" "EX000525" "EX000518" "EX000526" "EX000520" "EX000527" "EX000517"
# [14] "EX000498" "EX000499" "EX000519" "EX000361" "EX000360"
setdiff(unique(htp_log$SPID), trt_table[dataset == "Frank2017", unique(spid)])
# [1] "EX000375" "EX000376" "EX000377" "EX000379" "EX000380" "EX000387" "EX000394" "EX000397" "EX000398" "EX000401" "EX000405" "EX000406" "EX000407"
# [14] "EX000414" "EX000415" "EX000425" "EX000426" "EX000428" "EX000430" "EX000433" "EX000434" "EX000435" "EX000436" "EX000438" "EX000442" "EX000443"
# [27] "EX000444" "EX000448" "EX000450" "EX000451" "EX000452" ""

setdiff(trt_table[dataset == "Frank2017", unique(mea_treatment_name)], unique(htp_log$Chemical))
setdiff(unique(htp_log$Chemical), trt_table[dataset == "Frank2017", unique(mea_treatment_name)])

trt_table[dataset == "Frank2017" & updated_treatment_name %in% htp_log$Chemical, .(mea_treatment_name, updated_treatment_name, casn, spid, dataset)] # 34

table2 <- merge(trt_table[grepl("Shafer_103",filename)], htp_log, by.x = "spid", by.y = "SPID", all.x = T)
table2[, .(spid, mea_treatment_name, updated_treatment_name, Chemical)]
table2[is.na(Chemical)]
# just the 4 GF compounds, this is okay

table3 <- merge(trt_table[dataset == "Frank2017"], htp_log, by.x = "spid", by.y = "SPID", all.x = T)
table3[, .(spid, mea_treatment_name, updated_treatment_name, Chemical)]
table3[is.na(Chemical)]
c("TPP","Triphenyl phosphate (TPP)","TCEP","Tris(2-chloroethyl) phosphate (TCEP)") # ya, I know these are from NTP
c("Bis1","Domoic Acid","Mevastatin","Sodium Orthovanadate") # makes sense, these were same source as in Brown
# plus the 12 we registered later in Shafer_12

table2[updated_treatment_name != Chemical, .(updated_treatment_name, Chemical)]
table2[updated_treatment_name != Chemical, updated_treatment_name := Chemical]

# Goal: updated the updated_treamtment_name col in supplemental treatment name map

name_map1 <- read.csv(file = file.path(root_output_dir,"supplemental_mea_treatment_name_map.csv"), stringsAsFactors = F)
name_map1
setnames(name_map1, old = "spid_treatment_name", new = "updated_treatment_name")
merge(name_map1, table2[, .(mea_treatment_name, updated_treatment_name, dataset)])

# other checks
library(openxlsx)
shafer_103 <- as.data.table(read.xlsx(file.path(root_output_dir,"Sample IDs","not_using","EPA_ES202_EPA-Shafer_103_20191218_key.xlsx")))
test <- merge(shafer_103, htp_log[SPID != ""], all = T, by.x = "EPA_SAMPLE_ID",by.y = "SPID")
test[is.na(Chemical)] # checking for spid in shafer_103 not in ntp_log
# just the 4 Glufosinate compounds
test[is.na(ALIQUOT_PLATE_BARCODE)] # checking for spid in htp_log not in Shafer_103
# empty

test[CASRN != CAS...] # empty!!
# confirm treatment name updates in run_me were correct, particularly now that I see 2 options for e.g. lead

# do all casn from table1 agree with casn's in this file?
table1 <- as.data.table(read.xlsx(file.path(root_output_dir,"Frank2017","Table1_CompoundList_dtxsid_updated.xlsx"), sheet = 1))
(check_cas <- setdiff(table1$CAS.No., htp_log$CAS...)) # 18 are not present
table1[is.na(CAS.No.)] # just the end of the file
table1[CAS.No. %in% check_cas] # 21
shafer_12 <- as.data.table(read.xlsx(file.path(root_output_dir,"Sample IDs","EPA_ES204_EPA-Shafer_12_20200117_key.xlsx")))
shafer_12[CASRN %in% check_cas] # 12 are from this list
table1[CAS.No. %in% check_cas & !(CAS.No. %in% shafer_12$CASRN)]
# 2 are from NTP list
# 4 are from the Shafer_42 list, where the same compounds tested from specific Aim 1
# 3 NA rows
# So yes, all CASN in table 1 are in the HTP_LOG spid map.
