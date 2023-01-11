# original "Confirm Conc's" section, where I determined that there was a mistake in the conc's in the Calculations file
# from 10/06/2020
# Do not run ------------------------

# Confirm Conc's ----------------------------------------------------------------
# confirm that the conc's collected from master chem lists and Calc files match
# and that the correct concentration-corrections has been done for each compound

# check if there are multiple conc's assigned to the same well (usually occurs if there are differences between master chem file and calc file)
# Note: in TCPL mc1, the conc's are set to dat[ , conc := signif(conc, 3)]. So it's okay for us to round here.
dat[, .(num_unique_concs_in_well = length(unique(signif(conc,3)))), by = .(treatment, apid, rowi, coli)][num_unique_concs_in_well > 1]
#          treatment               apid rowi coli num_unique_concs_in_well
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
#                  apid treatment rowi coli AUC Calc DIV
# 1: 20170201_MW1145-25    Captan    3    8  10  0.1  10
dat[treatment == "Captan", unique(conc)]

summary_wide[Calc != AUC]
#                  apid       treatment rowi coli AUC Calc DIV
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

# I am correcting this in the Calculations file as well -> so this correction does not need to be repeated