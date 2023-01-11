library(data.table)
library(RMySQL)
nfa_spid <- as.data.table(read.xlsx(spidmap_file1, sheet = 1))
acute_spid <- as.data.table(read.xlsx("L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx", sheet = 1))
head(nfa_spid)
head(acute_spid)

setdiff(nfa_spid$EPA_SAMPLE_ID, acute_spid$EPA_SAMPLE_ID) # several

common_spids <- intersect(nfa_spid$EPA_SAMPLE_ID, acute_spid$EPA_SAMPLE_ID)
nfa_spid[EPA_SAMPLE_ID %in% common_spids]

# So it appears that these 2 files have totally different spids,
# but some of the same compounds
# But in the database, the stock conc's for the compounds in NFA list match the stock conc's for the compounds in the Acute list
# where teh compounds overlap...

setdiff(nfa_spid$preferred_name, acute_spid$dsstox_preferred_name) # 36 are in nfa but not in acute
setdiff(acute_spid$dsstox_preferred_name, nfa_spid$preferred_name) # sevearl dont' overlap here...
intersect(acute_spid$dsstox_preferred_name, nfa_spid$preferred_name) # 60 do overlap

# so let's review/verify:
# There are some compounds where the 

con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='invitrodb',host = Sys.getenv('INVITRODB_HOST_RO'))
query_term <- paste0("SELECT * FROM sample WHERE spid IN('",paste(nfa_spid[!is.na(EPA_SAMPLE_ID),unique(EPA_SAMPLE_ID)],collapse="','",sep=""),"');")
sample_info <- dbGetQuery(con, query_term)
dbDisconnect(con)
nrow(sample_info) == nrow(nfa_spid) # TRUE, so nothing got dropped
sample_info <- merge(sample_info, nfa_spid, by.x = c("spid"), by.y = "EPA_SAMPLE_ID", suffixes = c(".db",".file"))
setDT(sample_info)
sample_info[signif(stkc,3) != signif(ALIQUOT_CONC,3)] # 10 same compounds
question_trts <- sample_info[signif(stkc,3) != signif(ALIQUOT_CONC,3), unique(preferred_name)]
write.csv(sample_info[signif(stkc,3) != signif(ALIQUOT_CONC,3), .(preferred_name, spid, ALIQUOT_CONC_in_Shafer_96misc = ALIQUOT_CONC, STOCK_CONC_in_invitrodb = stkc)],
          file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/ToxCast2016/spids_unexpected_stock_conc.csv", row.names = F)


# So for these 10 compounds, do the stkc's all match the aliquot conc's listed in acute_spid?
compare_concs <- merge(sample_info[, .(spid, stkc, preferred_name)], acute_spid[, .(EPA_SAMPLE_ID, ALIQUOT_CONC, dsstox_preferred_name)],
                       by.x = c("preferred_name"), by.y = "dsstox_preferred_name", suffixes = c(".db",".file"))
compare_concs # okay, again, there are 60 that overlap
compare_concs[signif(stkc,3) != signif(ALIQUOT_CONC,3)] # all of htese agree
compare_concs[preferred_name %in% question_trts]
# preferred_name         spid stkc EPA_SAMPLE_ID ALIQUOT_CONC
# 1:            Erythromycin TP0001649D07  5.0  TP0001412H04          5.0
# 2: Methadone hydrochloride TP0001649D10 14.5  TP0001412E10         14.5
# 3:            Methoxychlor TP0001649E06 19.9  TP0001413G10         19.9
# 4:             Prallethrin TP0001649F10 19.9  TP0001414A12         19.9
# 5:      Pravastatin sodium TP0001649E12 10.0  TP0001412B05         10.0
# 6:               Tamoxifen TP0001649B03 10.0  TP0001412F09         10.0
# okay, so, the stock conc's listed in the acute 384 file match what is listed for the same comopunds (but diff spids) in teh 96 nfa list



# let's see - for teh acute spids, do the stock conc's agree, or are these just flipped?
con <- dbConnect(drv = RMySQL::MySQL(), user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), dbname='invitrodb',host = Sys.getenv('INVITRODB_HOST_RO'))
query_term <- paste0("SELECT * FROM sample WHERE spid IN('",paste(acute_spid[!is.na(EPA_SAMPLE_ID),unique(EPA_SAMPLE_ID)],collapse="','",sep=""),"');")
sample_info.acute <- dbGetQuery(con, query_term)
dbDisconnect(con)
nrow(sample_info.acute) == nrow(acute_spid) # TRUE, so nothing got dropped
sample_info.acute <- merge(sample_info.acute, acute_spid, by.x = c("spid"), by.y = "EPA_SAMPLE_ID", suffixes = c(".db",".file"))
setDT(sample_info.acute)
sample_info.acute[signif(stkc,3) != signif(ALIQUOT_CONC,3)] # empty - so all conc's agree
sample_info.acute[dsstox_preferred_name %in% question_trts, .(spid, chid, stkc, ALIQUOT_CONC, dsstox_preferred_name)] # yep, all of these are the same
# spid  chid stkc ALIQUOT_CONC   dsstox_preferred_name
# 1: TP0001412B05 47525 10.0         10.0      Pravastatin sodium
# 2: TP0001412E10 20501 14.5         14.5 Methadone hydrochloride
# 3: TP0001412F09 34187 10.0         10.0               Tamoxifen
# 4: TP0001412H04 22991  5.0          5.0            Erythromycin
# 5: TP0001413G10 20827 19.9         19.9            Methoxychlor
# 6: TP0001414A12 32572 19.9         19.9             Prallethrin
# well, for the 6 in this list
length(intersect(question_trts, unique(acute_spid$dsstox_preferred_name))) # 6

# Compound name	spid source	aliquot conc in file	aliquot conc in invitrodb
nfa_spid_info <- sample_info[signif(stkc,3) != signif(ALIQUOT_CONC,3), .(preferred_name, spid, ALIQUOT_CONC_in_file = ALIQUOT_CONC, STOCK_CONC_in_invitrodb = stkc)]
nfa_spid_info[, spid_source_file := "EPA_12088_EPA-Shafer_96misc_75ul_20160826_key"]
acute_spid_info <- sample_info.acute[dsstox_preferred_name %in% question_trts, .(spid, STOCK_CONC_in_invitrodb=stkc, ALIQUOT_CONC_in_file =ALIQUOT_CONC, preferred_name=dsstox_preferred_name)]
acute_spid_info[, spid_source_file := "EPA_11024_TShafer_384ph2_75ul_13May2015"]
spid_info <- rbind(nfa_spid_info, acute_spid_info)
write.csv(spid_info[preferred_name %in% intersect(question_trts, unique(acute_spid$dsstox_preferred_name))][order(preferred_name), .(preferred_name, spid, spid_source_file, ALIQUOT_CONC_in_file, STOCK_CONC_in_invitrodb)],
          file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/ToxCast2016/six_spids_in_acute_unexpected_stock_conc.csv", row.names = F)
