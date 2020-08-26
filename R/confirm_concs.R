confirm_concs <- function(dat, spidmap, expected_target_concs = c(0.03,0.1,0.3,1,3,10,30), expected_stock_conc = 20) {
  
  cat("Checking conc's:\n")
  
  # check if any conc's are NA
  if (dat[, any(is.na(conc))]) {
    stop(paste("\nThe following treatments have conc NA:",paste0(dat[is.na(conc), unique(treatment)],collapse=",")))
  }
  
  # compare the concentrations
  cat("\nAll compounds are assumed to have conc's",expected_target_concs,"\n(You can change this by updating the argument 'expected_target_concs' of the function confirm_concs()).\n")
  compare_concs <- merge(spidmap[, .(stock_conc, spidmap_guess_concs = paste0(signif(stock_conc/expected_stock_conc*expected_target_concs,3),collapse=",")), by = "spid"],
                         dat[wllt == "t", .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=",")), by = c("spid","treatment")], 
                         by = "spid", all.y = TRUE)
  if(nrow(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)]) > 0) {
    cat("The concentrations for the following compounds might need to be corrected:\n")
    compare_concs$probably_partially_conc_corrected <- sapply(strsplit(compare_concs$source_concs,split=","), function(x) length(x) > length(expected_target_concs))
    print(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)])
    
    response <- readline(prompt = "Update conc's for these compounds? (y/n): ")
    if (response %in% c("y","Y","yes","Yes")) {
      
      compare_concs[source_concs != spidmap_guess_concs, need_to_update_concs := TRUE]
      
      # for spid's with 'probably_partially_conc_corrected', standardize the conc's first:
      cat("Standardizing concs where 'probably_partially_conc_corrected'==TRUE...\n")
      dat[, conc_org := conc]
      dat <- merge(dat, compare_concs[, .(spid, probably_partially_conc_corrected, need_to_update_concs, stock_conc)], by = "spid", all.x = TRUE)
      dat[probably_partially_conc_corrected == TRUE, conc := signif(conc, digits = 1)]
      dat[, conc_standardized := conc]
  
      # now correct the conc's
      cat("Correcting conc's...\n")
      dat[need_to_update_concs == TRUE, conc := signif(stock_conc/expected_stock_conc*conc, 3)]
      print(dat[need_to_update_concs == TRUE, .(stock_conc, concs_in_source_dat = paste0(unique(conc_org),collapse=", "),
                                                concs_standardized = paste0(unique(conc_standardized))), by = c("spid","conc","stock_conc")][order(spid,conc), .(spid, stock_conc, concs_in_source_dat, concs_standardized, conc_updated = conc)])
      response <- readline(prompt = "Does conc correction look correct for each compound and dose? (y/n): ")
      if (!(response %in% c("y","Y","yes","Yes"))) browser()
      
      dat[, c("conc_org","probably_partially_conc_corrected","stock_conc","need_to_update_concs","conc_standardized") := list(NULL)]
      
      # final check:
      compare_concs <- merge(spidmap[, .(stock_conc, spidmap_guess_concs = paste0(signif(stock_conc/expected_stock_conc*expected_target_concs,3),collapse=",")), by = "spid"],
                             dat[wllt == "t", .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=",")), by = c("spid","treatment")], 
                             by = "spid", all.y = TRUE)
      if (nrow(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)]) > 0) {
        cat("Something is still not right:\n")
        print(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)])
        stop("Update conc's")
      }
    }
    else {
      stop("Update conc's, then re-run\n")
    }
  }
  else {
    cat("All compounds have the expected concetration-corrected values\n")
  }
  return(dat)
}
