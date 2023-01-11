# Goal:
# Find a combination of endpoints and cutoffs that detects as many mc_positives as possible
# based on median resp as max conc only
# While minimizing the number of false positives detected

# Very brief outline:
# - Assign the hit calls based on the aeid's in initial_endset and coff's initialized in sdat
# - Find the initial true positives and false positives based on hits in initial_endset
# - While some of the true positives are still undetected and there are any aeid left to add:
#   * Set the cutoff for each remaining aeid as follows:
#     > cutoff lower bound = max of med_resp_max_conc of all remaining negatives
#     > cutoff upper bound = min of med_resp_max_conc of all remaining positives
#     > cutoff is the average of the lower bound and upper bound (ensures that no new false positives will be added)
#   * Determine which endpoint will add the most additional true positives. Add that aeid to endset
#     > If there is a tie, repeat for each possible choice of aeid
#   * If no more true positives can be detected, break.

# Output:
# estl = a list which contains 1 data.table for each combination of endpoints and cutoffs found
# estl will likely not be unique

greedy_algo_methodC_bmad_multipliers_3_list <- function(sdat, bmad_multiplier = 'methodC', initial_endset = c(), outfile = NULL,
                                                   hit_cnt_threshold = 1, any_hit_initial_endset_positve = TRUE,
                                                   finish_with_method_B = TRUE, initial_i = 1) {
  
  # Update:
  # ce always based on number of TP/FP hits, not number additional detected, regardless of hit_cnt_threshold
  
  if (hit_cnt_threshold != 1) {
    stop('Function has not been validated with hit_cnt_threshold > 1')
  }
  
  cat('Starting Method C (min bmad_multiplier with no added FP) ...\n')
  if (length(initial_endset) > 0) {
    update_cols <- c('med_resp_max_conc','coff')
    sdat[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
    sdat[, sc_hit := as.integer(med_resp_max_conc >= coff)]
    
    if (any_hit_initial_endset_positve) {
      sdat[, sc_pos := as.numeric(any(sc_hit[aeid %in% initial_endset])), by = .(spid)]
    }
  }
  
  # initialize endset, true positives, and false positives
  mc_positives <- sdat[mc_pos == 1, unique(spid)]
  endset <- initial_endset
  true_positives <- sdat[aeid %in% endset & mc_pos == 1 & sc_pos == 1, unique(spid)]
  false_positives <- sdat[aeid %in% endset & mc_pos == 0 & sc_pos == 1, unique(spid)]
  endset_tb <- sdat[aeid %in% endset, unique(.SD), .SDcols = intersect(c('aeid','aenm','bmad_multiplier','coff','bmad'),names(sdat))]
  endset_tb[, `:=`(ttl_TP = length(true_positives), ttl_FP = length(false_positives))]
  rdat <- sdat[sc_pos == 0] # "remaining" dat, i.e. all undetected compounds
  i <- initial_i
  remaining_aeid <- setdiff(unique(sdat$aeid), endset)
  
  if(!is.null(outfile)) sink(outfile)
  cat('    AEID   bmad_multi\tce\tadd.TP.hits\tadd.FP.hits\tttl_TP\tttl_FP\taenm\n')
  cat("0", '. ', ifelse(i<10,' ',''), sep='')
  cat(rep("\t",8))
  cat(length(true_positives),'\t')
  cat(length(false_positives),'\t')
  cat(sdat[aeid %in% endset, unique(aenm)],sep=',')
  cat('\n')
  
  estl <- list()
  
  # Repeat while the endset sc_hit's do not cover all mc_positives
  while (any(!(mc_positives %in% true_positives)) & length(remaining_aeid) > 0) {
    
    # set coff just above the highest negative which would become a positive with 1 more hit
    # rdat[, hit_cnt := sum(sc_hit), by = .(spid)]
    # rdat[, coff := max(med_resp_max_conc[mc_pos == 0 & hit_cnt > hit_cnt_threshold - 1], na.rm=T), by = .(aeid, aenm)] # so many na's though...
    
    # rdat[aeid %in% remaining_aeid, coff := max(med_resp_max_conc[mc_pos == 0], na.rm=T), by = .(aeid, aenm)]
    # rdat[aeid %in% remaining_aeid, coff := (coff + ifelse(any(med_resp_max_conc > coff),min(med_resp_max_conc[med_resp_max_conc > coff]),
    #                               coff+1))/2, by = .(aeid, aenm)] # average this max value with the next highest resp_vs_bmad
    
    # Same result, but making my intention more explicit
    rdat[aeid %in% remaining_aeid, coff_lb := max(med_resp_max_conc[mc_pos == 0], na.rm=T), by = .(aeid, aenm)] # lower bound for coff is the highest med_resp_max_conc for negatives
    rdat[aeid %in% remaining_aeid, coff_ub := ifelse(any(med_resp_max_conc[mc_pos == 1] > unique(coff_lb)),
                                                     min(med_resp_max_conc[mc_pos == 1 & med_resp_max_conc > coff_lb], na.rm=T),
                                                     unique(coff_lb)*1.1), by = .(aeid, aenm)] # upper bound for coff is the lowest med_resp_max_conc for positives, if there are any above coff_lb
    rdat[aeid %in% remaining_aeid, coff := (coff_lb + coff_ub)*0.5]
    # note that an aeid will not be added to the endset if no med_resp_max_conc of positives are above coff_lb, so the choice of unique(coff_lb)*1.1 is ineffectual, as long as it is above coff_lb
    
    # assign hit calls
    rdat[, bmad_multiplier := coff/bmad]
    update_cols <- c('med_resp_max_conc','coff')
    rdat[, c(update_cols) := lapply(.SD, signif, digits = 6), .SDcols = update_cols]
    rdat[, sc_hit := as.numeric(med_resp_max_conc >= coff)]
    
    # Determine cost-efficiency of addding each aeid with given coffs and hit calls
    rdat[, sc_pos_test := 0]
    ce_vector <- c()
    add_TP_hits_cnts <- c()
    add_FP_hits_cnts <- c()
    for (aeidi in remaining_aeid) {
      rdat[, sc_pos_test := as.numeric(length(unique(aenm[sc_hit == 1 & aeid %in% c(endset, aeidi)])) >= 1), by = .(spid)]
      add_TP_hits <- rdat[mc_pos == 1 & sc_pos_test == 1, length(unique(spid))] # true positive have already been removed from rdat
      add_FP_hits <- rdat[mc_pos == 0 & sc_pos_test == 1, length(unique(spid))]
      ce <- add_FP_hits/add_TP_hits
      ce_vector <- c(ce_vector, ce)
      add_TP_hits_cnts <- c(add_TP_hits_cnts, add_TP_hits)
      add_FP_hits_cnts <- c(add_FP_hits_cnts, add_FP_hits)
    }
    
    # If can't add any more sc_hit's for remaining positives with the remaining aeid's, then stop
    if (sum(add_TP_hits_cnts) == 0) {
      break
    }
  
    # Determine which aeid to add
    # Choose the aeid with the best cost efficiency
    ce_vector <- signif(ce_vector, digits = 6)
    add_aeid <- remaining_aeid[ce_vector == min(ce_vector, na.rm = T)]
    # If there is still a tie, Choose the aeid that adds the most TP hits
    if (length(add_aeid) > 1) {
      max_add_TP_hits <- max(add_TP_hits_cnts[remaining_aeid %in% add_aeid], na.rm = T)
      add_aeid <- remaining_aeid[remaining_aeid %in% add_aeid & add_TP_hits_cnts == max_add_TP_hits]
      # If there is still a tie, keep the first one and repeat with each remaining possibility
      if(length(add_aeid) > 1) {
        use_sdat <- sdat[, unique(.SD), .SDcols = setdiff(names(sdat), 'coff')]
        use_sdat <- merge(use_sdat, rdat[, .(coff = unique(coff)), by = .(aeid)], by = 'aeid', all.x = T)
        for (add_aeidi in add_aeid[-1]) {
          # desired coff's have already been defined in use_sdat for desired aeid
          # only the coffs for initial_endset will be considered
          res <- greedy_algo_methodC_bmad_multipliers_3_list(sdat = use_sdat, bmad_multiplier = bbmad_multiplier,
                                                                           initial_endset = c(endset, add_aeidi), outfile,
                                                                           hit_cnt_threshold, any_hit_initial_endset_positve = TRUE,
                                                                           finish_with_method_B = FALSE, initial_i = i)
          estl[(length(estl)+1):(length(estl)+length(res))] <- res
          rm(res)
        }
        add_aeid <- add_aeid[1]
      }
    }
    add_TP_hits <- add_TP_hits_cnts[remaining_aeid == add_aeid]
    add_FP_hits <- add_FP_hits_cnts[remaining_aeid == add_aeid]
    ce <- ce_vector[remaining_aeid == add_aeid]
    
    # Add the chosen aeid to the endset and update sc_pos
    endset <- c(endset, add_aeid)
    rdat[, sc_pos := as.numeric(length(unique(aenm[sc_hit == 1 & aeid %in% endset])) >= hit_cnt_threshold), by = .(spid)]
    true_positives <- c(true_positives, rdat[sc_pos == 1 & mc_pos == 1, unique(spid)])
    false_positives <- c(false_positives, rdat[sc_pos == 1 & mc_pos == 0, unique(spid)])
    endset_tb <- rbind(endset_tb, cbind(rdat[aeid == add_aeid, unique(.SD), .SDcols = c('aeid','aenm','coff','bmad','bmad_multiplier')], 
                                        data.table('ce' = ce,
                                                   'add_TP_hits' = add_TP_hits,
                                                   'add_FP_hits' = add_FP_hits, 
                                                   'ttl_TP' = length(true_positives), 'ttl_FP' = length(false_positives))), fill = TRUE)    
    rdat <- rdat[sc_pos == 0] # remove the compounds that have been detected
    
    # Output to console
    cat(i, '. ', ifelse(i<10,' ',''), sep='')
    cat(add_aeid, '\t')
    cat('-','\t')
    cat(signif(ce,3),'\t')
    cat(add_TP_hits,'\t\t')
    cat(add_FP_hits,'\t\t')
    cat(length(true_positives),'\t')
    cat(length(false_positives),'\t')
    cat(rdat[aeid == add_aeid, unique(aenm)],'\n')
    
    i <- i+1
    remaining_aeid <- setdiff(unique(sdat$aeid), endset)
    
  }
  
  rm(rdat)
  
  # # now, finish out with method B (optional)
  # if (finish_with_method_B && any(!(mc_positives %in% true_positives))) {
  #   # Noting which compounds have already been hit
  #   # but not initializing endset - allowing repeats at lower coff where that is optimal
  #   cat('\n')
  #   endset_tb_methodB <- greedy_algo_variable_bmad_multipliers_2(sdat, initial_true_positives = true_positives, initial_false_positives = false_positives,
  #                                                             hit_cnt_threshold = hit_cnt_threshold)
  #   endset_tb <- rbind(endset_tb, endset_tb_methodB)
  # }
  
  if(!is.null(outfile)) sink()
  
  endset_tb[, rank := .I]
  estl[[length(estl)+1]] <- endset_tb
  
  return(estl)
}
