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

# Load data ---------------------------------------------------------------

load(file.path('DNT_NTP2021','check_for_chem_to_rescreen','DNT_NTP2021_preliminary_in_house_normalized_values_2022-05-11.RData'))
cat(description)
# An mc3-like table containing resp values along with current invitrodb cutoffs.
# Created from DNT_NTP2021_preliminary_longfile.RData.
# Created with apply_in_house_tcpl-like_processing_2022-05-11.R


# option:
# use the tcplFit2 function in tcpl v3 (this calls tcplfit2_core)
# make my own fun to call tcplfit2

# The key question:
# is this format helpful for me to run tcplhit, or to plot, or get AC50s, etc?

# What I learned yesterday (May 10, 2022)
# - In tcpl_v3, there are 2 key functions in tcplFit2.R: tcplfit2 and tcplhit2. These call the "core" function in tcplfit2::
# - the "tcplFit2" function in tcpl_v3 might provide a good starting point, 
#   but I would need to do a lot of tinkering to configure my data to get to work
#   (and it kept givign me an error that I coudl not explain!)
#   Since the point of runnign tcplfit2 for me is to not have to deal with a database back-end,
#   I'm just going to make my own "lite" versions of these functions, inspired by what's in tcpl_v3


my_tcplfit2 <- function(dat.by.spid.aenm, fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                        bmed = NULL) {
  
  #So, we'll try for-looping it
  aenm.spid.tb <- dat.by.spid.aenm[, unique(.SD), .SDcols = c('aenm','spid')]
  setkey(aenm.spid.tb, aenm, spid)
  setkey(res, aenm, spid)
  
  mc4.list <- list()
  for(i in 1:nrow(dat.by.spid.aenm)) {
    dati <- dat.by.spid.aenm[i]
    resi <-  tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),
                                     unlist(dati$response),
                                     cutoff = dati$bmad,
                                     bidirectional = TRUE,
                                     verbose = FALSE, 
                                     force.fit = TRUE,
                                     fitmodels = fitmodels
    )
    resi$aenm <- aenm.spid.tb[i,aenm]
    resi$spid <- aenm.spid.tb[i,spid]
    mc4.list[[i]] <- resi
  }
  return(mc4.list)
  
}


my_tcplhit2 <- function(dat, mc4.list) {
  
  mc5 <- data.table()
  for (i in 1:length(mc4.list)) {
    spidi <- mc4.list[[i]]$spid
    aenmi <- mc4.list[[i]]$aenm
    dati <- dat[spid == spidi & aenm == aenmi]
    hit.res <- tcplhit2_core(mc4.list[[i]],
                             conc = unlist(dati$conc),
                             resp = unlist(dati$response),
                             cutoff = unique(dati$coff),
                             onesd = unique(dati$osd),
                             bmed = unique(dati$bmed),
                             identifiers = data.frame('spid' = spidi, 'aenm' = aenmi))
    mc5 <- rbind(mc5, hit.res)
  }
  
  return(mc5)
}

# Vision:
# - pivot output for tcplfit2 to a long table, save as your "level 4" output for reference (or keep as list for now, see what's most helpful for plotting)
# - output of tcplhit2 is tabular - rbidn these rows together in "level 5" table!
# - then can assign flags!!



# Add needed columns ------------------------------------------------------

# Add spid
dat2[, spid := treatment]

# Add logc
dat2[, logc := log10(conc)]

# Define bmad column to use
grep('bmad',names(dat2), val = T) # ""bmad_subset"        "bmad_invitrodb"     "bmad_pooled"        "bmad_pooled_vs_idb"
# we want the bmad pooled from invitrodb and this new data set
dat2[, bmad := bmad_pooled]

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
dat2[, osd := sd(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE), by = .(aenm)]

# bmed
# also see this method in tcpl_v3 mc4_mthds:
# bmed.aeid.lowconc.twells = function() {
#   
#   e1 <- bquote(dat[ , bmed := median(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE)])
#   list(e1)
#   
# },
dat2[, bmed := median(resp[cndx %in% 1:2 & wllt == "t"], na.rm = TRUE), by = .(aenm)]
# bmed will affect tcplhit_core, but not tcplfit2_core

# Define coff column
dat2[, coff := coff_pooled]

# Add some add'l columns, taken from tcplfit2 tcpl_v3
dat2[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds"), {
  rmns <- mean(resp)
  rmds <- median(resp)
  nconcs <- .N
  med_rmds <- rmds >= (3 * bmad)
  .(rmns, rmds, nconcs, med_rmds)
}), keyby = .(aenm, spid, logc)]


# Try my_tcplfit2 ---------------------------------------------------------

# Create table to input
dat.by.spid.aenm <- dat2[, .(
  bmad = min(bmad), resp_max = max(resp), osd = min(osd), bmed = ifelse(is.null(bmed), 0, max(bmed)),
  resp_min = min(resp), max_mean = max(rmns), max_mean_conc = logc[which.max(rmns)],
  max_med = max(rmds), max_med_conc = logc[which.max(rmds)],
  logc_max = max(logc), logc_min = min(logc), nconc = length(unique(logc)),
  npts = .N, nrep = median(as.numeric(nconcs)), nmed_gtbl = sum(med_rmds) / first(nconcs),
  concentration_unlogged = list(10^(logc)), response = list(resp),
  # columns I'm adding
  coff = unique(coff)
), keyby = .(aenm, spid)]
# note: the resp and conc logged "columns" are actually lists
# but still 1 row in data table per anem, spid

# Run the function!!
dat2[1, .(spid, aenm)]
test.dat <- dat.by.spid.aenm[spid == '7126 A1' & aenm == 'CCTE_Shafer_MEA_dev_AB_dn']
test.mc4.list <- my_tcplfit2(test.dat)
str(test.mc4.list)
# All works, cool!

# How handle cases where nconcs is quite low?
test.dat <- dat.by.spid.aenm[spid == 'DMSO' & aenm == 'CCTE_Shafer_MEA_dev_AB_dn']
test.mc4.list <- my_tcplfit2(test.dat)
# Error in log10(conc) : non-numeric argument to mathematical function
test.dat$concentration_unlogged # empty list
# so we'll have to avoid these cases


# Try tcplhit2_core -------------------------------------------------------

?tcplhit2_core
test.dat <- dat.by.spid.aenm[spid == '7126 A1' & aenm == 'CCTE_Shafer_MEA_dev_AB_dn']
test.mc4.list <- my_tcplfit2(test.dat)
hit.res <- tcplhit2_core(test.mc4.list[[1]],
                         conc = unlist(test.dat$conc),
                         resp = unlist(test.dat$response),
                         cutoff = unique(test.dat$coff),
                         onesd = unique(test.dat$osd),
                         bmed = unique(test$bmed),
                         identifiers = test.dat[, unique(.SD), .SDcols = c('spid','aenm')])
str(hit.res)
# Let's put this into a function!!

debugonce(my_tcplhit2)
test.mc5 <- my_tcplhit2(test.dat, test.mc4.list)
str(test.mc5)

# Ithink this worked!!
# Looking at the tcplhit2 funciton in tcpl_v3, looks liek the rest of the code
# is just designed to manage the database

# Try it with multiple points
test.dat <- dat2[spid == '7126 A1' & aenm %in% c('CCTE_Shafer_MEA_dev_AB_dn','CCTE_Shafer_MEA_dev_LDH_dn')]
test.mc4.list <- my_tcplfit2(test.dat)
hit.res <- tcplhit2_core(test.mc4.list[[1]],
                         conc = unlist(test.dat$conc),
                         resp = unlist(test.dat$resp),
                         cutoff = unique(test.dat$coff),
                         onesd = unique(test.dat$osd),
                         bmed = unique(test$bmed),
                         identifiers = test.dat[, unique(.SD), .SDcols = c('spid','aenm')])
str(hit.res)
# Let's put this into a function!!

debugonce(my_tcplhit2)
test.mc5 <- my_tcplhit2(test.dat, test.mc4.list)

# Merge into 2 table
setdiff(names(test.mc5), names(test.dat))
intersect(names(test.mc5), names(test.dat))
usedat <- merge(test.dat, test.mc5, by = c('spid','aenm'))


# Assign level 6 flags ----------------------------------------------------

list(
  
  singlept.hit.high = function(mthd) {
    
    flag <- "Only highest conc above baseline, active"
    out  <- c("m5id", "m4id", "aeid", "mc6_mthd_id", 
              "flag", "fval", "fval_unit")
    init <- bquote(list(.(mthd), .(flag), NA_real_, NA_character_, FALSE))
    e1 <- bquote(ft[ , .(c(out[4:7], "test")) := .(init)])
    e2 <- bquote(ft[ , lstc := max_med_conc == logc_max])
    e3 <- bquote(ft[ , test := nmed_gtbl == 1 & hitc == 1 & lstc])
    e4 <- bquote(f[[.(mthd)]] <- ft[which(test), .SD, .SDcols = .(out)])
    cr <- c("mc6_mthd_id", "flag", "fval", "fval_unit", "test", "lstc")
    e5 <- bquote(ft[ , .(cr) := NULL])
    list(e1, e2, e3, e4, e5)
    
  }
)

needed.cols <- c('max_med_conc','logc_max','nmed_gtbl','hitc')
setdiff(needed.cols, names(usedat))
# [1] "hitc" 
usedat[, hitc := hitcall]


# Assign dummy id's
usedat[, aeid := which(sort(unique(aenm)) == aenm)]
usedat[, m5id := .I]
usedat[, m4id := .I]

# Add column for modl_ga
usedat[, modl_ga := pmin(ac50, ac50_loss)]
names(usedat)


# **Idea: What if I just calculate the lvl 4, 5, adn 6 all in 1 go of a for loop?
# then, you would already have things like max_med_conc, etc on tap?
# BUT, I do want to save several of these columns fo rmy easy access regardless
# so might be worth goign the database route

# adapted from how methods are applied in tcpl
ft <- usedat

## Initialize f, the list of data.tables containing the flag information
# (ms is tcplMthdLoad(lvl = 6L, for given aeid))
ms <- data.table(mthd_id = c(1), mthd = 'singlept.hit.high')
setkey(ms, mthd_id) # must do for "exprs" line below
f <- vector(mode = "list", length = max(ms$mthd_id))

## Generate and evaluate flag expressions
mthd_funcs <- tcpl:::mc6_mthds()
exprs <- lapply(ms$mthd_id, function(x) mthd_funcs[[ms[J(x), mthd]]](x)) # XXX this breaks if functions appear more than once in mc6_mthds() 
exprs
# [[1]]
# [[1]][[1]]
# ft[, `:=`(c("mc6_mthd_id", "flag", "fval", 
#             "fval_unit", "test"), list(1, "Only highest conc above baseline, active", 
#                                        NA_real_, NA_character_, FALSE))]
# 
# [[1]][[2]]
# ft[, `:=`(lstc, max_med_conc == logc_max)]
# 
# [[1]][[3]]
# ft[, `:=`(test, nmed_gtbl == 1 & hitc == 1 & lstc)]
# 
# [[1]][[4]]
# f[[1]] <- ft[which(test), .SD, .SDcols = c("m5id", "m4id", 
#                                            "aeid", "mc6_mthd_id", "flag", "fval", 
#                                            "fval_unit")]
# 
# [[1]][[5]]
# ft[, `:=`(c("mc6_mthd_id", "flag", "fval", 
#             "fval_unit", "test", "lstc"), NULL)]
# woah!! this just transformed the function into data.table arguments!!
mthd_funcs[[1]](1) # similar
fenv <- environment()
invisible(rapply(exprs, eval, envir = fenv))

f <- rbindlist(f)

f
# empty.. but I think that's because it didn't have this flag


# Attempt run all mc6 methods ---------------------------------------------

# Let's try the rest of the methods, see if I'm missing any other columns
# (ms is tcplMthdLoad(lvl = 6L, for given aeid))
ms <- data.table(mthd_id = seq(1, length(mthd_funcs)), mthd = names(mthd_funcs))
setkey(ms, mthd_id) # must do for "exprs" line below
f <- vector(mode = "list", length = max(ms$mthd_id))

## Generate and evaluate flag expressions
mthd_funcs <- tcpl:::mc6_mthds()
exprs <- lapply(ms$mthd_id, function(x) mthd_funcs[[ms[J(x), mthd]]](x)) # XXX this breaks if functions appear more than once in mc6_mthds() 
exprs
fenv <- environment()
invisible(rapply(exprs, eval, envir = fenv))
# Error in eval(jsub, SDenv, parent.frame()) : object 'modl_la' not found
# I'm realizing this could take a very long time
# Maybe I'll be a bit more selective for which flags I want to cater to?

names(mthd_funcs)
use_mthds <- c("singlept.hit.high", "singlept.hit.mid","multipoint.neg",
               "modlga.lowconc") # was goign to include "noise", but need modl_rmse
ms <- data.table(mthd_id = which(names(mthd_funcs) %in% use_mthds), mthd = use_mthds)
setkey(ms, mthd_id) # must do for "exprs" line below
f <- vector(mode = "list", length = max(ms$mthd_id))

## Generate and evaluate flag expressions
mthd_funcs <- tcpl:::mc6_mthds()
exprs <- lapply(ms$mthd_id, function(x) mthd_funcs[[ms[J(x), mthd]]](x)) # XXX this breaks if functions appear more than once in mc6_mthds() 
exprs
fenv <- environment()
invisible(rapply(exprs, eval, envir = fenv))

f <- rbindlist(f)
f
# cool, no errors!

# I'm thinking I'll just consider/peruse for the other flags on my own


# Next steps
# See how it looks, clean this up into your own functions, and you're good to go!
# Does conc units matter? (some are in mg/mL)
