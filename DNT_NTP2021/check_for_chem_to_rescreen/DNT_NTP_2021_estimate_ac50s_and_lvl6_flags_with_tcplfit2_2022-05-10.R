# ------------------------------------------------------------------------ #
# Would tcplfit2 serve as a decent proxy for the AC50s adn the flags
# that will be assigned when this data is run in tcpl?
# Or is that even the right question?
# Will this data even be run with tpcl, or tcplfit2?
# Lookign at tcpl_v3 -> similar flags will be assigned at level 6 (unless something changes)
# 
# Regardless, I think similarity to the current setup is less important
# than will the data look good in tcpl_v3?
# (i.e., will there be flags?)
# 
# May 10, 2022
# ------------------------------------------------------------------------ # 

# To do:
# See if there is additional data to add, since Seline's update
# Estimate the AC50s with tcplfit2
# estimate the flags
# see what would be nice to re-run! (or necessary)
# make this streamlined?


library(data.table)
library(tcplfit2)

# Load tcpl_v3, stored in a different library
.libPaths(new = 'C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R_extras/library_alternates')
.libPaths() # okay, now this is the new top .libPaths()
library(tcpl)
# tcpl:::tcplFit2
# cool, it's there!

# Since I"m open in another project..
setwd('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl')


# Load data ---------------------------------------------------------------

load(file.path('DNT_NTP2021','check_for_chem_to_rescreen','DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-02.RData'))
cat(description)
# An mc3-like table containing resp values along with current invitrodb cutoffs.
# Created from DNT_NTP2021_preliminary_longfile.RData.
# Created with apply_in_house_tcpl-like_processing_2022-05-02.R


# option:
# use the tcplFit2 function in tcpl v3 (this calls tcplfit2_core)
# make my own fun to call tcplfit2

# The key question:
# is this format helpful for me to run tcplhit, or to plot, or get AC50s, etc?


# What columns are expected in dat for tcplFit2?
# https://ccte-bitbucket.epa.gov/projects/TOX/repos/tcpl/browse/R/tcplFit2.R?at=tcpl_v3
cols.string <- 'aeid
spid
logc
resp
bmad
osd
bmed
m3id'
cat(gsub('\n',"',\n'",cols.string))
needed.cols <- c('aeid',
                 'spid',
                 'logc',
                 'resp',
                 'bmad',
                 'osd',
                 'bmed',
                 'm3id')
setdiff(needed.cols, names(dat2))
# [1] "aeid"   "spid"   "logc"   "bmad"   "osd"    "mbmed"  "rmds"   "m3id"   "nconcs"

# Add an aeid
aenm.map <- dat2[, unique(.SD), .SDcols = 'aenm']
aenm.map[, aeid := .I]
dat2 <- merge(dat2, aenm.map, by = 'aenm', all.x = T)

# Add spid
dat2[, spid := treatment]

# Add logc
dat2[, logc := log10(conc)]

# Add bmad
grep('bmad',names(dat2), val = T) # "bmad_subset"    "bmad_invitrodb"
# we want the bmad from invitrodb
dat2[, bmad := bmad_invitrodb]

# add osd... what's that?
# probably onesd...
# I spy this method in tcpl_v3 mc4_mthds:
# onesd.aeid.lowconc.twells = function() {
#   
#   e1 <- bquote(dat[ , osd := sd(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
#   list(e1)
#   
# },
# Cool, looks like on sd of cndx1&2
# I don't think it will affect the model, wouldn't hurt to have this
dat2[, osd := sd(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE), by = .(aeid, aenm)]

# bmed
# also see this method in tcpl_v3 mc4_mthds:
# bmed.aeid.lowconc.twells = function() {
#   
#   e1 <- bquote(dat[ , bmed := median(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
#   list(e1)
#   
# },
dat2[, bmed := median(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE), by = .(aeid, aenm)]
# bmed will affect tcplhit_core, but not tcplfit2_core

# add arbitrary m3id
dat2[, m3id := .I]


# Trying tcpl:::tcplfit2_core... ------------------------------------------

fit.res <- tcpl:::tcplFit2(dat2)
# Error in which(abs(rmds) >= cutoff) : 
#   dims [product 7] do not match the length of object [68]
# In addition: Warning message:
#   In abs(rmds) >= cutoff :
#   longer object length is not a multiple of shorter object length


# Breaking down what tcplfit2 is doing...
# Adding some columsn to dat
dat2[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds"), {
  rmns <- mean(resp)
  rmds <- median(resp)
  nconcs <- .N
  med_rmds <- rmds >= (3 * bmad)
  .(rmns, rmds, nconcs, med_rmds)
}), keyby = .(aeid, spid, logc)]

# Creating another object with 1 row per aeid, spid
res <- dat2[, .(
  bmad = min(bmad), resp_max = max(resp), osd = min(osd), bmed = ifelse(is.null(bmed), 0, max(bmed)),
  resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
  max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
  logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
  npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
  concentration_unlogged = list(10^(logc)), response = list(resp), m3ids = list(m3id)
),
keyby = .(aeid, spid)
]

# Adding columns to res (particularly fitting parameters!)
res[, `:=`(tmpi = seq_len(.N)), keyby = .(aeid)]
fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5")
res[,`:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
                                                   unlist(response),
                                                   cutoff = bmad,
                                                   bidirectional = TRUE,
                                                   verbose = FALSE, force.fit = TRUE,
                                                   fitmodels = fitmodels
))),
keyby = .(spid)]
# Error in which(abs(rmds) >= cutoff) : 
#   dims [product 7] do not match the length of object [68]
# In addition: Warning message:
#   In abs(rmds) >= cutoff :
#   longer object length is not a multiple of shorter object length

# I wonder if this is maybe supposed to only run for 1 aeid at a type?
# What if i made it go by spid AND Aeid?

res[,`:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
                                                   unlist(response),
                                                   cutoff = bmad,
                                                   bidirectional = TRUE,
                                                   verbose = FALSE, force.fit = TRUE,
                                                   fitmodels = fitmodels
))),
keyby = .(spid, aeid)]
# Error in if ((rmad <- mad(resp)) > 0) log(rmad) else log(1e-32) : 
# missing value where TRUE/FALSE needed

# I wonder if this is happening for my spids that are not really treatment substances?
unique(res$spid)
res[, .N, by = .(nconc)]
res <- res[nconc > 1]

res[,`:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
                                                   unlist(response),
                                                   cutoff = bmad,
                                                   bidirectional = TRUE,
                                                   verbose = FALSE, force.fit = TRUE,
                                                   fitmodels = fitmodels
))),
keyby = .(spid, aeid)]
# Error in if ((rmad <- mad(resp)) > 0) log(rmad) else log(1e-32) : 
# missing value where TRUE/FALSE needed

# same error...

# I'll just adapt and make my own function

my_tcplfit2 <- function(dat, fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                        bmed = NULL) {
  
  res <- dat2[, .(
    bmad = min(bmad), resp_max = max(resp), osd = min(osd), bmed = ifelse(is.null(bmed), 0, max(bmed)),
    resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
    max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
    logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
    npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
    concentration_unlogged = list(10^(logc)), response = list(resp)
  ),
  keyby = .(aeid, spid)
  ]
  
  # Adding columns to res (particularly fitting parameters!)
  # res[, `:=`(tmpi = seq_len(.N)), keyby = .(aeid)]
  
  # Keep getting this error with the below, even after removing rows with nconcs == 1
  # res[,`:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
  #                                                    unlist(response),
  #                                                    cutoff = bmad,
  #                                                    bidirectional = TRUE,
  #                                                    verbose = FALSE, force.fit = TRUE,
  #                                                    fitmodels = fitmodels
  # ))),
  # keyby = .(spid, aeid)]
  # Error in if ((rmad <- mad(resp)) > 0) log(rmad) else log(1e-32) : 
  # missing value where TRUE/FALSE needed
  
  #So, we'll try for-looping it
  aeid.spid.tb <- res[, unique(.SD), .SDcols = c('aeid','spid')]
  setkey(aeid.spid.tb, aeid, spid)
  setkey(res, aeid, spid)
  
  for(i in 1:nrow(aeid.spid.tb)) {
    test <- res[J(aeid.spid.tb[i])]
    test[, `:=`(fitparams = list(tcplfit2::tcplfit2_core(unlist(concentration_unlogged),
                                                         unlist(response),
                                                         cutoff = bmad,
                                                         bidirectional = TRUE,
                                                         verbose = FALSE, force.fit = TRUE,
                                                         fitmodels = fitmodels
    )))]
    test2 <-  tcplfit2::tcplfit2_core(unlist(test$concentration_unlogged),
                                      unlist(test$response),
                                      cutoff = test$bmad,
                                      bidirectional = TRUE,
                                      verbose = FALSE, force.fit = TRUE,
                                      fitmodels = fitmodels
    )
    break
  }
  test
  
}

?tcplhit2_core
hit.res <- tcplhit2_core(test2,
              conc = unlist(test$concentration_unlogged),
              resp = unlist(test$response),
              cutoff = 3*test$bmad,
              onesd = test$osd,
              bmed = test$bmed)
str(hit.res)

# Vision:
# - pivot output for tcplfit2 to a long table, save as your "level 4" output for reference (or keep as list for now, see what's most helpful for plotting)
# - output of tcplhit2 is tabular - rbidn these rows together in "level 5" table!
# - then can assign flags!!


my_tcplfit2 <- function(dat, fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                        bmed = NULL) {
  
  res <- dat2[, .(
    bmad = min(bmad), resp_max = max(resp), osd = min(osd), bmed = ifelse(is.null(bmed), 0, max(bmed)),
    resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
    max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
    logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
    npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
    concentration_unlogged = list(10^(logc)), response = list(resp)
  ), keyby = .(aeid, spid)]
  
  #So, we'll try for-looping it
  aeid.spid.tb <- res[, unique(.SD), .SDcols = c('aeid','spid')]
  setkey(aeid.spid.tb, aeid, spid)
  setkey(res, aeid, spid)
  
  mc4.list <- list()
  for(i in 1:nrow(aeid.spid.tb)) {
    dati <- res[J(aeid.spid.tb[i])]
    resi <-  tcplfit2::tcplfit2_core(unlist(test$concentration_unlogged),
                                      unlist(test$response),
                                      cutoff = test$bmad,
                                      bidirectional = TRUE,
                                      verbose = FALSE, force.fit = TRUE,
                                      fitmodels = fitmodels
    )
    resi$aeid <- aeid.spid.tb[i,aeid]
    resi$spid <- aeid.spid.tb[i,spid]
    mc4.list <- c(mc4.list, resi)
  }
  return(mc4.list)
  
}

mc4.list <- my_tcplfit2(dat2)


# RESUME HERE
# get the above to work
# chunk as need to run in tcplhit2_core, create level 5 data frame
# DIY assignment of level 6 flags!! Figure out if you need any addiontional columns made earlier on
# See how it looks, clean this up into your own functions, and you're good to go!
