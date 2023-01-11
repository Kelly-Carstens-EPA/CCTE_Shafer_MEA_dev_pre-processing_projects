# ------------------------------------------------------------------------ #
# Prepare MEA NFA data for NTP
# August 1, 2022
# Amy Carpenter
# ------------------------------------------------------------------------ #

rm(list = ls())
library(data.table)
library(stringi)

# Load data
load('DNT_NTP2021/output/DNT_NTP2021_longfile.RData')
cat(description)
# DNTP MEA NFA prepared data
# Date Ran: 2022-08-01


# Apply checks that I usually apply before send to ToxCast Pipeline -------

# any duplicated data?
if (nrow(dat[, .N, by = .(apid, rowi, coli, acsn)][N != 1]) > 0) {
  cat("There appears to be duplicated data for the following apid/row/coli/acsn:\n")
  print(dat[, .N, by = .(apid, rowi, coli, acsn)][N != 1])
}

# any NA spid?
if(any(is.na(dat$spid))) {
  cat("Some spid are NA in the following datasets:\n")
  cat(dat[is.na(spid), unique(dataset)],sep = "\n")
}
dat[is.na(DNTP_blind_code)] # empty, good

# any NA conc
if(any(is.na(dat$conc))) {
  cat("Some conc's are NA for the following spids:\n")
  cat(dat[is.na(conc), unique(spid)],sep = "\n")
}


# Below adapated from "save_lvl0_snapshot" in get_latest_dat.R,
# which I run to do some final checks beforing saving the "level 0" data for TCPL
cat("Removing apid where median of controls on DIV 12 is < 10 spikes per min or < 2 active electrodes...\n")
remove_apid <- dat[wllq == 1 & wllt == 'n' & grepl("firing_rate_mean_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 10/60, unique(apid)]
remove_apid <- union(remove_apid,
                     dat[wllq == 1 & wllt == 'n' & grepl("active_electrodes_number_DIV12",acsn), .(bval = median(rval, na.rm = T)), by = .(apid)][bval < 2, unique(apid)])
cat("Setting wllq:=0 for these apid:",remove_apid,"\n")
# Setting wllq:=0 for these apid:  
print(dat[apid %in% remove_apid & (grepl("active_electrodes_number_DIV12",acsn) | grepl("firing_rate_mean_DIV12",acsn)) & wllt == "n", .(apid, sub("CCTE_Shafer_MEA_dev_","",acsn), rval)])
# Empty data.table (0 rows and 3 cols): apid,V2,rval
dat[apid %in% remove_apid, `:=`(wllq = 0,
                                wllq_notes = paste0(wllq_notes,"; Median MFR < 10 spikes per min or nAE < 2 on DIV12"))]

cat("Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints:",dat[!grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
# Number of points where wllq==1 and rval is NA for AUC, LDH, or AB endpoints: 0 
cat("Number of points where wllq==1 and rval is NA for DIV endpoints:",dat[grepl("_DIV",acsn) & is.na(rval) & wllq == 1, .N],"\n")
# Number of points where wllq==1 and rval is NA for DIV endpoints: 40505  
# (this is typical for data points from individual DIV)

cat("Renaming 'acsn' to 'acnm'...\n")
setnames(dat, old = "acsn", new = "acnm", skip_absent = TRUE)



# Clean data for NTP ------------------------------------------------------


# Get the time_point
dat[, time_point := stri_extract(acnm, regex = 'DIV[0-9]*$')]
dat[grepl('(AB)|(LDH)',acnm), time_point := 'DIV12'] # cytotox measurements come from DIV12
dat[is.na(time_point), time_point := 'AUC']
dat[, .N, by = .(time_point, acnm)][order(time_point, acnm)]
# looks good

# Confirm no NA rvals in non-DIV endpoints where wllq == 1
dat[is.na(rval) & wllq == 1, .N, by = .(time_point, wllq, grepl('(LDH)|(AB)',acnm))]
# cool, no NA rvals for the AUC or cyto endpoints

# Get the culture_date
dat[, culture_date := sub('_.*$','',apid)]
dat[, summary(as.numeric(culture_date))]

# add anm column
dat[, anm := 'CCTE_Shafer_MEA']

# Add date_read
dat[time_point == 'AUC', DIV_numeric := 12]
dat[time_point != 'AUC', DIV_numeric := as.numeric(sub('DIV','',time_point))]
dat[is.na(DIV_numeric)] #e mpty, good
dat[, culture_date := as.Date(culture_date, format = '%Y%m%d')]
dat[, date_read := format(culture_date + DIV_numeric, format = '%Y%m%d')]
dat[, .N, by = .(apid, time_point, date_read)]


# Get desired column names for plate position, blind code
dat[, .N, by = .(DNTP_blind_code)] # this is all good!
setnames(dat, old = 'treatment', new = 'plate_position')
dat[, .N, by = .(plate_position)]


# Add the replicate ID
dat[, rep_id := frank(paste0(apid,rowi,coli), ties.method = 'dense'), by = .(plate_position, DNTP_blind_code, conc, anm)]
dat[, summary(rep_id)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   27.41    3.00  414.00 
dat[rep_id > 6, .N, by = .(plate_position)]
# plate_position     N
# 1:           DMSO 35496
# 2:          Water  1044
# this makes sense

# Visualize an example
ggplot(dat[DNTP_blind_code == '1852' & grepl('LDH',acnm)], aes(x = conc, y = rval)) +
  geom_point(aes(color = as.factor(rep_id), pch=as.factor(wllq)))+
  scale_x_log10()

# Okay, this could be confusing... because there is nothing that is actually the same as rep #1 from the first culture
# and rep #2 from the second culture...
# e.g. there are multiple culture dates associated with rep 1, depending on the conc
dat[DNTP_blind_code == '1852' & rep_id == 1, .N, by = .(culture_date, conc)]
# culture_date  conc  N
# 1:   2021-09-15 1e-01 87
# 2:   2021-09-15 3e-01 87
# 3:   2021-09-15 1e+00 87
# 4:   2021-09-15 3e+00 87
# 5:   2021-09-15 1e+01 87
# 6:   2021-09-15 3e+01 87
# 7:   2021-09-15 1e+02 87
# 8:   2022-06-01 1e-04 87
# 9:   2022-06-01 3e-04 87
# 10:   2022-06-01 1e-03 87
# 11:   2022-06-01 3e-03 87
# 12:   2022-06-01 1e-02 87
# 13:   2022-06-01 3e-02 87

# I would really encourage them to rely more on the apid to define the replicates
# Note that a given chemical only appears on each plate once (for every conc tested) -> so each apid is a replicate
dat[, length(unique(paste0(rowi,coli))), by = .(DNTP_blind_code, conc, apid)][V1 > 1,.N, by = .(DNTP_blind_code, conc)]
#    DNTP_blind_code conc  N
# 1:            DMSO  0.1 69
# 2:           Water  0.1  3
# (other than DMSO and water)

# Visualization using the apid to define the replicates
ggplot(dat[DNTP_blind_code == '1852' & grepl('LDH',acnm)], aes(x = conc, y = rval)) +
  geom_point(aes(color = as.factor(apid), pch=as.factor(wllq)))+
  scale_x_log10()

# Note: I am including replicates where the wllq is 0. So when you remove the wllq == 0 points, some replicates will appear to be missing
# e.g:
dat[DNTP_blind_code == '1852' & grepl('LDH',acnm) & wllq == 1, .(rep_ids = paste0(sort(unique(rep_id)),collapse = ",")), by = .(conc)]
#     conc rep_ids
# 1: 1e-04   1,2,3
# 2: 3e-04   1,2,3
# 3: 1e-03   1,2,3
# 4: 3e-03   1,2,3
# 5: 1e-02   1,2,3
# 6: 3e-02   1,2,3
# 7: 1e-01   4,5,6

# Further illustration of why the rep_id may be misleading if I'm interpretting its purpose correctly:
example <- dcast(dat[DNTP_blind_code == '1852'], apid ~ conc, value.var = 'rep_id', fun.aggregate = function(x) paste0(sort(unique(x)),collapse = ","))
example
#                  apid 1e-04 3e-04 0.001 0.003 0.01 0.03 0.1 0.3 1 3 10 30 100
# 1: 20210915_MW75-9220                                     1   1 1 1  1  1   1
# 2: 20210915_MW75-9301                                     2   2 2 2  2  2   2
# 3: 20210915_MW75-9302                                     3   3 3 3  3  3   3
# 4: 20220601_MW78-7214     1     1     1     1    1    1   4
# 5: 20220601_MW78-7215     2     2     2     2    2    2   5
# 6: 20220601_MW78-7216     3     3     3     3    3    3   6


# add the other columns we need
setnames(dat, old = 'units', new = 'tested_conc_unit')
dat[, .N, by = .(tested_conc_unit)]
# tested_conc_unit      N
# 1:               uM 221067
# 2:            mg/ml  36540
# 3:                %  37584
dat[, dilution_solvent_percent := '0.1'] # I confirmed this with Seline 07/28/2022


# Any cleaning to the wllq_notes column?
View(dat[, .N, by = .(wllq, wllq_notes)])
# looks okay

# Any specific notes to highlight?
# e.g. where excat conc coudl not be determined? -> it's there, they should know this. Conc's should be fine
dat[grepl('Plate Map stock conc <= 10. Exact concentration could not be determined',wllq_notes), .N, by = .(DNTP_blind_code, conc)]
# looks like all of these have max conc as 10

# Any additional columns i think shoudl be included?
names(dat)
# I could add in the solvent (whether DMSO or Water)
# But they did not ask for this, so I will exclude for not unless but keep in my copy of the data

# Make sure columns are ordered as they appear in table
dat <- dat[, .(apid, rowi, coli, plate_position, DNTP_blind_code, wllt, conc, tested_conc_unit, dilution_solvent_percent, wllq, wllq_notes, rval, anm, acnm, srcf, time_point, date_read, rep_id)]

# Save it
dat <- dat[order(-anm, acnm, DNTP_blind_code, conc, apid, rep_id)]
write.csv(dat, row.names = F, file = paste0('DNT_NTP2021/output/DNT_NTP2021_EPA_MEA_NFA_2022-08-01.csv'))
