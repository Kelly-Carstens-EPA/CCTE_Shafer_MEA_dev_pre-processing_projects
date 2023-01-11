# ------------------------------------------------------------------------ #
# Create tables of chem that likley need to be retested at lower concentrations
# (bc efficacious at all tested concs)
# Based on insight gained in investigate chemicals need to be repeated rmd
#
# May 2 2022
# ------------------------------------------------------------------------ #

library(data.table)
library(openxlsx)

# load data ---------------------------------------------------------------

load(file.path('DNT_NTP2021','output','DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-02.RData'))
cat(description)



# Calculate key values ----------------------------------------------------

# Find the lowest conc at which it and all following conc's are above the coff
dat2[, max_cndx_below_coff := max(cndx[med_resp_vs_coff < 1], na.rm  = T), by = .(treatment, aenm)]
dat2[any_wllq1 == 1 & (is.na(max_cndx_below_coff) | is.infinite(max_cndx_below_coff)), max_cndx_below_coff := 0] # if no med resp's are below the coff
dat2[, max_cndx := max(cndx, na.rm = T), by = .(treatment, aenm)]
dat2[, min_consecutive_cndx_above_coff := pmin(max_cndx_below_coff+1, max_cndx), by = .(treatment, aenm)]
dat2[, summary(min_consecutive_cndx_above_coff)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   5.000   7.000   5.527   7.000   7.000    1302 
dat2[is.na(min_consecutive_cndx_above_coff), .N, by = .(any_wllq1)]
# any_wllq1    N
# 1:         0 1302
# cool, all the treatment-aenm's with NA values just don't have any useable points

# UPdate - oh that's right, we removed all wllq 0 points in tcpl -like prcoessing...


# Create table to save ----------------------------------------------------
dat2[is.na(wllq_notes), wllq_notes := '']
dat2[, unique(culture_date)]
tb1 <- dat2[, .(`endpoints_no_cndx_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% 1])),
                `endpoints_cndx<=1_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% c(1,2)])),
                `endpoints_cndx<=2_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% c(1,2,3)])),
                total_endpoints = length(unique(aenm)),
                wllq_notes = paste0(unique(wllq_notes), collapse = "; "),
                cultures_tested = paste0(sort(unique(culture_date)),collapse = ";")), by = .(treatment)]
View(tb1)

tb2 <- dat2[!grepl('DIV12',aenm) & !grepl('_up',aenm), .(`endpoints_no_cndx_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% 1])),
                                                         `endpoints_cndx<=1_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% c(1,2)])),
                                                         `endpoints_cndx<=2_below_coff` = length(unique(aenm[min_consecutive_cndx_above_coff %in% c(1,2,3)])),
                                                         total_endpoints = length(unique(aenm)),
                                                         wllq_notes = paste0(unique(wllq_notes), collapse = "; "),
                                                         cultures_tested = paste0(sort(unique(culture_date)),collapse = ";")), by = .(treatment)]

tb3 <- dat2[!treatment %in% c('DMSO','H2O'), .(`chem_no_cndx_below_coff` = length(unique(treatment[min_consecutive_cndx_above_coff %in% 1])),
                                               `chem_cndx<=1_below_coff` = length(unique(treatment[min_consecutive_cndx_above_coff %in% c(1,2)])),
                                               `chem_cndx<=2_below_coff` = length(unique(treatment[min_consecutive_cndx_above_coff %in% c(1,2,3)])),
                                               total_chem_tested = length(unique(treatment))), by = .(aenm)]
View(tb2)

wb <- createWorkbook()
addWorksheet(wb, sheetName = 'stats by chem, all aenm')
addWorksheet(wb, sheetName = 'stats by chem, AUC dn only')
addWorksheet(wb, sheetName = 'stats by aenm')
writeData(wb, sheet = 'stats by chem, all aenm', tb1)
writeData(wb, sheet = 'stats by chem, AUC dn only', tb2)
writeData(wb, sheet = 'stats by aenm', tb3)

saveWorkbook(wb, file = 'DNT_NTP2021/check_for_chem_to_rescreen/DNT_NTP2021_MEA_NFA_summary_activity_above_coff_2022-05-02.xlsx')
