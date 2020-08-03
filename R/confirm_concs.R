confirm_concs <- function(dat, spidmap, check_conc_correction = TRUE, expected_target_concs = c(0,0.03,0.1,0.3,1,3,10,30)) {
  
  # check if any conc's are NA
  if (dat[, any(is.na(conc))]) {
    stop(paste("\nThe following treatments have conc NA:",paste0(dat[is.na(conc), unique(treatment)],collapse=",")))
  }
  
  # check that all conc's are the same for each endpoint
  print("The following treatment appear to have multiple conc's for same conc:")
  print(dat[, .(conc = paste0(unique(conc),collapse=",")), by = c("apid","coli","treatment")][grepl(",",conc)])
  # So I guess it actually does matter a ton... I am going to correct this anyhow
  
  # check if concentration corrections have been done
  if (check_conc_correction) {
    
    # compare the concentrations
    cat("\nAll compounds are assumed to have conc's",expected_target_concs,"\n(You can change this by setting the 'expected_target_concs' argument of the fun confirm_concs()).\n")
    compare_concs <- merge(spidmap[, .(stock_conc, spidmap_guess_concs = paste0(signif(stock_conc/20*expected_target_concs,3),collapse=",")), by = "spid"],
                           dat[, .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=",")), by = c("spid","treatment")], 
                           by = "spid", all.y = TRUE)
    if(nrow(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)]) > 0) {
      cat("The concentrations for the following compounds might need to be corrected:\n")
      compare_concs$probably_partially_conc_corrected <- sapply(strsplit(compare_concs$source_concs,split=","), function(x) length(x) > length(expected_target_concs))
      print(compare_concs[source_concs != spidmap_guess_concs])
      
      # # implement a potential fix
      # 
      # # update all conc's fro these treatments that are expected target conc
      # fix_spids <- compare_concs[probably_partially_conc_corrected == TRUE, unique(spid)]
      # dat <- merge(dat, spidmap[, .(spid, stock_conc)], by = "spid")
      # dat[spid %in% fix_spids & conc %in% expected_target_concs, conc := signif(stock_conc/20 * conc, 3)]
      # compare_concs <- merge(spidmap[, .(stock_conc, spidmap_guess_concs = paste0(signif(stock_conc/20*expected_target_concs,3),collapse=",")), by = "spid"],
      #                        dat[, .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=",")), by = c("spid","treatment")], 
      #                        by = "spid", all.y = TRUE)
      # 
      # # update all conc's that do not match spidmap_guess_conc's using the expected_target_concs
      # fix_spids <- compare_concs[source_concs != spidmap_guess_concs, unique(spid)]
      # dat4[spid %in% fix_spids, conc2 := expected_target_concs[which(conc == sort(unique(conc)))], by = "spid"] # eh... hmm
      
      response <- readline(prompt = "Continue anyways? (y/n): ")
      if (!(response %in% c("y","Y","yes","Yes"))) {
        stop("Update conc's, then re-run\n")
      }
    }
    else {
      cat("All compounds have the expected concetration-corrected values")
    }
  }
}
