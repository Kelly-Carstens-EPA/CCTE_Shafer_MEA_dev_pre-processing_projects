# early investigations, approx. 10/16/2020
library(data.table)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))
spidmap <- as.data.table(read.csv(file.path(root_output_dir,"master_spidmap_mea_nfa.csv"), stringsAsFactors = F))


# # check if any trt's don't have any match in the spidmap -------------------------------------------
alldat <- get_latest_dat()
alldat[, culture := sub("_.*$","",apid)]

rep_spids <- alldat[wllt == "t", .(length(unique(dataset))), by = .(spid)][V1 > 1, c(spid)]
alldat[spid %in% rep_spids, .(paste0(sort(unique(treatment)),collapse=","), cultures = unique(culture)), by = .(spid)][order(V1, cultures)]
alldat[spid %in% rep_spids, .(paste0(sort(unique(treatment)),collapse=","), dataset = unique(dataset)), by = .(spid, culture)][order(spid, culture)]

# what about compounds tested in multiple culture dates anywhere?
alldat[wllt == "t", .(length(unique(culture))), by = .(spid)][V1 > 1]
# ya know what, no. I'm not going to go down this path. 
# We don't need this much detail.

# clean up list, let's go by treatment
alldat[treatment == "4-(4-Chlorophenyl)-4-hydroxy-N,N-dimethyl-alpha,alpha-diphenylpiperidine-1-butyramide monohydrochloride", treatment := "Loperamide"]
rep_trts <- alldat[wllt == "t", .(length(unique(dataset))), by = .(treatment)][V1 > 1, c(treatment)]
alldat[treatment %in% rep_trts, .(dataset = unique(dataset)), by = .(treatment, culture)][order(treatment, culture), .(treatment, dataset, dates_tested = culture)]

# okay, we need to get spids for the brown compounds
mea_nfa_map <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/sample_tables/mea_nfa_treatment_spid_map_oct23_2020.csv", stringsAsFactors = F))
test <- merge(alldat, mea_nfa_map[, .(mea_treatment_name, dataset, spid)], by.x = c("spid","dataset"), by.y = c("mea_treatment_name","dataset"), suffixes = c("",".update"), all.x = T)
test[!is.na(spid.update) & spid != spid.update, .N, by = .(treatment, dataset)]
# treatment   dataset    N
# 1:        Acetaminophen Brown2014 2064
# 2: Bisindolymaleimide 1 Brown2014 4128
# 3:          Domoic Acid Brown2014 2064
# 4:           Loperamide Brown2014 2064
# 5:           Mevastatin Brown2014 2064
# 6: Sodium Orthovanadate Brown2014 4472
alldat <- merge(alldat, mea_nfa_map[, .(mea_treatment_name, dataset, spid)], by.x = c("spid","dataset"), by.y = c("mea_treatment_name","dataset"), suffixes = c("",".update"), all.x = T)
alldat[!is.na(spid.update) & spid != spid.update, spid := spid.update]
alldat[, spid.update := NULL]

# now group by replicate spid again
# so this is all compounds where I am currently assigning the same SPID across multiple datasets
rep_spids <- alldat[wllt == "t", .(length(unique(dataset))), by = .(spid)][V1 > 1, c(spid)]
alldat[spid %in% rep_spids, .(paste0(sort(unique(treatment)),collapse=","), dataset = unique(dataset)), by = .(spid, culture)][order(spid, culture)]
tb1 <- alldat[spid %in% rep_spids, .(treatment = paste0(sort(unique(treatment)),collapse=",")), by = .(spid, dataset, culture)][order(spid), .(currently_assigned_spid = spid, mea_treatment_name = treatment, dataset, dates_tested = culture)]
write.csv(tb1, file.path(root_output_dir, "control_compounds_dates_tested.csv"), row.names= F)

# create table of unique casn, supplier, cat, etc.
tb2 <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Shafer_compounds_to_register_20201016.csv"))
tb2_filter <- tb2[Supplier != "" & SPID == "", .(dates_tested = paste0(range(as.numeric(dates_tested)), collapse=" - "), sample_info_source = paste0(sort(unique(sample_info_source)),collapse=";")), by = .(mea_treatment_name, CAS, Supplier, Catalogue, Lot)]
write.csv(tb2_filter, file.path(root_output_dir, "Shafer_6_compound_info_to_register_20201016.csv"), row.names= F)


trt_table <- alldat[wllt == "t", .(cultures = paste0(sort(unique(sub("_.*$","",apid))),collapse=",")), by = .(treatment, spid, dataset)]
rm(alldat)


alldat[treatment == "Tris(2-chloroethyl) phosphate", .(unique(rowi)), by = .(apid)]
