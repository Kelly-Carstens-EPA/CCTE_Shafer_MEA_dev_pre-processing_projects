# Get methods for future reference
# May 2 2022

library(tcpl)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')

aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c('aid','anm','acid','acnm','export_ready','internal_ready'))
aeid.tb <- aeid.tb[grepl('_dev_',aenm)]

sc1.mthds <- tcplMthdLoad(lvl = 1L, id = unique(aeid.tb$aeid), type = 'sc')
sc2.mthds <- tcplMthdLoad(lvl = 2L, id = unique(aeid.tb$aeid), type = 'sc')

mc2.mthds <- tcplMthdLoad(lvl = 2L, id = unique(aeid.tb$acid), type = 'mc')
mc3.mthds <- tcplMthdLoad(lvl = 3L, id = unique(aeid.tb$aeid), type = 'mc')
mc4.mthds <- tcplMthdLoad(lvl = 4L, id = unique(aeid.tb$aeid), type = 'mc')
mc5.mthds <- tcplMthdLoad(lvl = 5L, id = unique(aeid.tb$aeid), type = 'mc')
mc6.mthds <- tcplMthdLoad(lvl = 6L, id = unique(aeid.tb$aeid), type = 'mc')

description <- 'All MEA NFA sc and mc TCPL methods as of May 2, 2022.
These methods were used for the latest udpates to the MEA NFA data that took place in
June 2021 for the sc and May 2021 for the mc.'
cat(ls(), sep = ",\n")
save(aeid.tb,
     description,
     mc2.mthds,
     mc3.mthds,
     mc4.mthds,
     mc5.mthds,
     mc6.mthds,
     sc1.mthds,
     sc2.mthds,
     file = 'L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/mea_nfa_tcpl_methods_2022-05-02.RData')


# Update July 27, 2022 -> also saving a copy in nfa project folder
load('L:/Lab/NHEERL_MEA/tcpl_nheerl_mea_dev/mea_nfa_tcpl_methods_2022-05-02.RData')
description <- paste0(description, '\nCreated with the script supplemental_scripts/get_mea_nfa_tcpl_methods_2022-05-02.R')

save(aeid.tb,
     description,
     mc2.mthds,
     mc3.mthds,
     mc4.mthds,
     mc5.mthds,
     mc6.mthds,
     sc1.mthds,
     sc2.mthds,
     file = 'tcpl_results/mea_nfa_tcpl_methods_2022-05-02.RData')
