# 11/25/2020
# Background:
# for some MEA NFA datasets, the compounds are tracked by name
# the name must be mapped to the appropriate spid in the spidmap files.
# creating a table to show the relationship between
# the treatment names in the mea data, teh treatment name used to match something in the spidmap file,
# and teh spidmapfile used for each spid/dataset
# refer to 'supplemental_mea_treatment_name_map.csv' and the dataset run_me's for notes on the spid assignment
library(data.table)
library(openxlsx)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
setwd(root_output_dir)
load('lvl0_snapshots/mea_nfa_lvl0_extra_cols_2020-11-12.RData')
head(mea_nfa_lvl0)

mea_nfa_lvl0[, culture_date := sub("_.*","",apid)]
trt_table <-mea_nfa_lvl0[wllt == "t", .(culture_date_range = paste0(min(culture_date)," - ",max(culture_date))), 
                         by = c("treatment","mea_treatment_name","spid","dataset")]
rm(mea_nfa_lvl0)

# get the spid's and corresponding files
spidmap_files <- list.files(path = "Sample IDs", pattern = "\\.xlsx", full.names = T)
sapply(spidmap_files, getSheetNames) # confirmign if I want to default to sheet 1 for each
use_sheets <- unlist(lapply(spidmap_files, function(x) getSheetNames(x)[1]))
use_sheets[grep("All Assays_list_toxcast_OECD 20190524",spidmap_files)] <- "NFA Groups"
use_spid_col <- rep(NA_character_, length(spidmap_files)) # will assume epa_sample_id or spid
use_spid_col[grep("All Assays_list_toxcast_OECD 20190524",spidmap_files)] <- "NCCT\\.ID"
use_spid_col[grep("Shafer_sample_info_to_register_20201110_afc",spidmap_files)] <- "NEW\\.SPID"
# all_tables <- lapply(spidmap_files, read.xlsx, sheet = use_sheets) # so sad this doesn't work...
all_tables <- data.table()
for (i in 1:length(spidmap_files)) {
  tbi <- as.data.table(read.xlsx(spidmap_files[i], use_sheets[i]))
  tbi_use_names <- names(tbi)
  tbi_use_names <- tbi_use_names[!grepl('existing_spid',tbi_use_names)]
  setnames(tbi, old = grep(ifelse(is.na(use_spid_col[i]),"(EPA_SAMPLE_ID)|(SPID)",use_spid_col[i]),toupper(tbi_use_names))[1], 
           new = "EPA_SAMPLE_ID")
  tbi <- tbi[, .SD, .SDcols = grep("(EPA_SAMPLE_ID)|(PREFERRED_NAME)",names(tbi))]
  tbi <- tbi[!is.na(EPA_SAMPLE_ID)]
  tbi$spidmap_file <- basename(spidmap_files[i])
  all_tables <- rbind(all_tables, tbi, fill = T)
  rm(tbi)
}

# clean up
check.spid.duplicates <- all_tables[, .N, by = .(EPA_SAMPLE_ID)][N > 1, c(EPA_SAMPLE_ID)]
all_tables[EPA_SAMPLE_ID %in% check.spid.duplicates] # duplicates in Copy of NTP91_Compounds_4NHEERL_MEA_dev_cg.xlsx. whatever
all_tables <- unique(all_tables)

all_tables[spidmap_file == 'All Assays_list_toxcast_OECD 20190524.xlsx', EPA_SAMPLE_ID := paste0("EPAPLT0",EPA_SAMPLE_ID)]
trt_table <- merge(trt_table, all_tables, all.x = T, by.x = "spid", by.y = "EPA_SAMPLE_ID")
trt_table <- trt_table[order(as.numeric(sub(" - .*$","",culture_date_range)))]

# save it
write.csv(trt_table, file = file.path("sample_tables",paste0("mea_nfa_treatment_spid_map_",as.character.Date(Sys.Date()),".csv")), row.names = F)
