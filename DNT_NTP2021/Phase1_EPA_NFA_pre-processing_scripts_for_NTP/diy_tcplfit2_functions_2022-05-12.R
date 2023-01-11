# ------------------------------------------------------------------------ #
# DIY tcplfit2 scripts
# 
# May 11, 2022
# ------------------------------------------------------------------------ # 

# library(tcpl)
# library(tcplfit2)
# library(data.table)

# Expected data input:
# somethign that has been in house "pre-processed", similar to an mc3
# as made from apply_in_house_tcpl-like_processing_2022-05-11.R

# Prepare input for tcplfit2

create_dat.by.spid.aenm <- function(dat, bmad_col = 'bmad_pooled', coff_col = 'coff_pooled') {
  
  # Add needed columns
  dat2[, spid := treatment]
  dat2[, logc := log10(conc)]
  dat2[, bmad := get(bmad_col)]
  
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
  dat2[, coff := get(coff_col)]
  
  # Add some add'l columns, taken from tcplfit2 tcpl_v3
  dat2[, `:=`(c("rmns", "rmds", "nconcs", "med_rmds"), {
    rmns <- mean(resp)
    rmds <- median(resp)
    nconcs <- .N
    med_rmds <- rmds >= (3 * bmad)
    .(rmns, rmds, nconcs, med_rmds)
  }), keyby = .(aenm, spid, logc)]
  
  # Create table for input to tcplfit2_core
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
  
  return(dat.by.spid.aenm)
}


my_tcplfit2 <- function(dat.by.spid.aenm, 
                        fitmodels = c("cnst", "hill", "gnls", "poly1", "poly2", "pow", "exp2", "exp3", "exp4", "exp5"),
                        bmed = NULL) {
  
  mc4.list <- list()
  i <- 0
  setkey(dat.by.spid.aenm, spid, aenm)
  cat('Processing',length(unique(dat.by.spid.aenm$aenm)),'assay endpoints...\n')
  
  for(aenmi in unique(dat.by.spid.aenm$aenm)) {
    for(spidi in dat.by.spid.aenm[aenm == aenmi, unique(spid)]) {
      dati <- dat.by.spid.aenm[aenm == aenmi & spid == spidi]
      resi <-  tcplfit2::tcplfit2_core(unlist(dati$concentration_unlogged),
                                       unlist(dati$response),
                                       cutoff = dati$bmad,
                                       bidirectional = TRUE,
                                       verbose = FALSE, 
                                       force.fit = TRUE,
                                       fitmodels = fitmodels
      )
      i <- i+1
      resi$aenm <- aenmi
      resi$spid <- spidi
      mc4.list[[i]] <- resi
    }
    cat(as.character(Sys.time()), aenmi,'complete\n')
  }
  return(mc4.list)
}


my_tcplhit2 <- function(dat, mc4.list) {
  
  mc5 <- data.table()
  for (i in 1:length(mc4.list)) {
    spidi <- mc4.list[[i]]$spid
    aenmi <- mc4.list[[i]]$aenm
    dati <- dat[spid == spidi & aenm == aenmi]
    hit.res <- tcplfit2::tcplhit2_core(mc4.list[[i]],
                             conc = unlist(dati$conc),
                             resp = unlist(dati$response),
                             cutoff = unique(dati$coff),
                             onesd = unique(dati$osd),
                             bmed = unique(dati$bmed),
                             identifiers = data.frame('spid' = spidi, 'aenm' = aenmi))
    mc5 <- rbind(mc5, hit.res)
  }
  
  # Merge into 1 table
  usedat <- merge(dat, mc5, by = intersect(names(dat), names(mc5)))
  
  return(usedat)
}

# Potential vision, if want to be mroe in line with tcpl
# - pivot output for tcplfit2 to a long table, save as your "level 4" output for reference (or keep as list for now, see what's most helpful for plotting)
# - output of tcplhit2 is tabular - rbidn these rows together in "level 5" table!
# - then can assign flags!!


my_mc6 <- function(mc5, use_mthds = c("singlept.hit.high", "singlept.hit.mid","multipoint.neg",
                                      "modlga.lowconc")) {
  if (!all(use_mthds %in% c("singlept.hit.high", "singlept.hit.mid","multipoint.neg",
                            "modlga.lowconc"))) 
    warning(paste0('my_mc6 may not prepare all of the columns needed for ', 
                   setdiff(use_mthds, c("singlept.hit.high", "singlept.hit.mid","multipoint.neg","modlga.lowconc"))))
  
  # Prepare some add'l columns in mc5
  mc5[, hitc := hitcall]
  
  # If missing, assign dummy id's
  if(!'aeid' %in% names(mc5)) mc5[, aeid := which(sort(unique(aenm)) == aenm)]
  if(!'m5id' %in% names(mc5)) mc5[, m5id := .I]
  if(!'m4id' %in% names(mc5)) mc5[, m4id := .I]
  
  # Add column for modl_ga
  if(!'modl_ga' %in% names(mc5)) mc5[, modl_ga := pmin(ac50, ac50_loss)]
  
  ft <- mc5 # that's what it's called in the mc6_mthds, just have to use that
  
  ## Initialize f, the list of data.tables containing the flag information
  # (ms is tcplMthdLoad(lvl = 6L, for given aeid))
  mthd_funcs <- tcpl:::mc6_mthds()
  cat('Other flags to consider:', setdiff(names(mthd_funcs),use_mthds), sep = '\n')
  ms <- data.table(mthd_id = which(names(mthd_funcs) %in% use_mthds), mthd = use_mthds)
  setkey(ms, mthd_id) # must do for "exprs" line below
  f <- vector(mode = "list", length = max(ms$mthd_id))
  
  ## Generate and evaluate flag expressions
  exprs <- lapply(ms$mthd_id, function(x) mthd_funcs[[ms[J(x), mthd]]](x)) # XXX this breaks if functions appear more than once in mc6_mthds() 
  fenv <- environment()
  invisible(rapply(exprs, eval, envir = fenv))
  
  # Transform the flags object into a data table?
  f <- rbindlist(f)
  
  # Add the flag name
  f[, mc6_mthd := names(mthd_funcs)[mc6_mthd_id]] # note that these id's will be different than how they appear in invitrodb
  
  f.condensed <- f[, .(flag_length = .N,
                       mc6_mthd_ids = paste0(sort(unique(mc6_mthd_id)),collapse = ","),
                       mc6_mthds = paste0(sort(unique(mc6_mthd)),collapse = ";")),
                   by = .(m5id, m4id, aeid)]
  mc5_mc6 <- merge(mc5, f.condensed, by = intersect(names(mc5), names(f.condensed)), all.x = T)
  mc5_mc6[is.na(flag_length), flag_length := 0]
  
  return(mc5_mc6)
}



# Examples ----------------------------------------------------------------


# ## Prepare
# dat.by.spid.aenm <- create_dat.by.spid.aenm(dat2)
# 
# ## tcplfit2
# test.dat <- dat.by.spid.aenm[spid == '7126 A1' & aenm == 'CCTE_Shafer_MEA_dev_AB_dn']
# test.mc4.list <- my_tcplfit2(test.dat)
# str(test.mc4.list)
# ## All works, cool!
# 
# ## tcplhit2
# test.mc5 <- my_tcplhit2(test.dat, test.mc4.list)
# str(test.mc5)
# 
# ## Apply mc6 flags
# mc5_mc6 <- my_mc6(test.mc5)

# I'm thinking I'll just consider/peruse for the other flags on my own
