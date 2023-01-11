# ------------------------------------------------------------------------ #
# Correction to MEA NFA data for NTP
# Specifically correcting tested_conc_unit from mg/mL to ug/mL
# August 16, 2022
# Amy Carpenter
# ------------------------------------------------------------------------ #

rm(list = ls())
library(data.table)
library(stringi)


# Load existing data ------------------------------------------------------

dat <- as.data.table(read.csv(paste0('DNT_NTP2021/output/DNT_NTP2021_EPA_MEA_NFA_2022-08-01.csv')))


# Make correction ---------------------------------------------------------

dat[, .N, by = .(tested_conc_unit)]
# tested_conc_unit      N
# 1:            mg/ml  36540
# 2:               uM 221067
# 3:                %  37584

# Checking this out
dat[tested_conc_unit == 'mg/ml', .(max_conc = max(conc)), by = .(DNTP_blind_code)][order(max_conc)]
# compare these to HCI conc's tested
# most have "10" as max conc

# Make the correction (using capital L, since that's what was used in the coded plate map for the stock conc)
dat[tested_conc_unit == 'mg/ml', tested_conc_unit := 'ug/mL']

dat[, .N, by = .(tested_conc_unit)]
# tested_conc_unit      N
# 1:            ug/mL  36540
# 2:               uM 221067
# 3:                %  37584

# Save corrected data -----------------------------------------------------

write.csv(dat, row.names = F, file = paste0('DNT_NTP2021/output/DNT_NTP2021_EPA_MEA_NFA_2022-08-16.csv'))

# Checking - why is this new file 2,000 fewer kilobytes?
rm(dat)
dat1 <- as.data.table(read.csv(paste0('DNT_NTP2021/output/DNT_NTP2021_EPA_MEA_NFA_2022-08-01.csv')))
dat2 <- as.data.table(read.csv(paste0('DNT_NTP2021/output/DNT_NTP2021_EPA_MEA_NFA_2022-08-16.csv')))
nrow(dat1)
nrow(dat2)
# same

all.equal(dat1, dat2)
# [1] "Column 'tested_conc_unit': Levels not identical even after refactoring since trim.levels is TRUE"
# okay, that's the only difference noted!