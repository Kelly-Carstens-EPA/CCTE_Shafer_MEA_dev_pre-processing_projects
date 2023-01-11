# fun call in meadq. e is s
e <- s[[cur.file]]
#get well information
plateinfo <- plateinfo(e$layout$array)
wells <- plateinfo$wells
names(wells) <- wells #keep the names valid.
wells.layout <- plateinfo$layout
# ns.all<-lapply(wells, function(well) {
#   compute.ns(e, ns.T = ns.T, ns.N = ns.N, sur=sur, whichcells = well)})
whichcells <- wells[1]
names.to.indexes(names(e$spikes), whichcells, allow.na=TRUE)

compute.ns <- function(e, ns.T, ns.N, sur=100, whichcells=NULL,
                       plot=FALSE) {
  ## Main entrance function to compute network spikes.
  ## Typical values:
  ## ns.T: 0.003 (time in seconds)
  ## ns.N: 10
  ## sur: 100
  
  indexes = names.to.indexes(names(e$spikes), whichcells, allow.na=TRUE)
  if (length(indexes)==0) {
    ## No electrodes were found matching "whichcells"
    ## so just return brief information summarising no network activity.
    ns <- list()
    ns$brief <- c(n=NA, peak.m=NA, peak.sd=NA, durn.m=NA, durn.sd=NA)
  } else {
    counts <- spikes.to.count2(e$spikes[indexes], time.interval=ns.T)
    p <- find.peaks(counts, ns.N)
    ns <- list(counts=counts, ns.N=ns.N, ns.T=ns.T)
    class(ns) <- "ns"
    m <- mean.ns(ns, p, plot=plot, nrow=4, ncol=4, ask=FALSE, sur=sur)
    if (is.null(m)) {
      ## No network spikes found.
      ns$brief <- c(n=0, peak.m=NA, peak.sd=NA, durn.m=NA, durn.sd=NA)
    } else {
      ns$mean <- m$ns.mean; ns$measures <- m$measures
      peak.val <- ns$measures[,"peak.val"]
      durn <- ns$measures[,"durn"]
      ns$brief <- c(n=nrow(ns$measures),
                    peak.m=mean(peak.val), peak.sd=sd(peak.val),
                    durn.m=mean(durn, na.rm=TRUE), durn.sd=sd(durn, na.rm=TRUE))
      
    }
  }
  
  ns
}

spikes <- e$spikes[indexes] # list of 16 for each electrode. EAch list contains times of spikes from that elec

# spikes.to.count2 calls this function;
z <- .C(C_ns_count_activity,
        as.double(unlist(spikes)),
        as.integer(nspikes),
        as.integer(length(nspikes)),
        as.double(beg), as.double(end), as.double(time.interval),
        as.integer(nbins),
        counts = integer(nbins))
# void ns_count_activity(Sfloat *allspikes, int *nspikes, int *pncells,
#                        Sfloat *pbeg, Sfloat *pend, Sfloat *pwid,
#                        int *pnbins,
#                        int *count)
# allspikes = the time of every spike in the well, from any electrodes
allspikes <- as.double(unlist(spikes))
head(as.double(unlist(spikes))) # [1] 2.02000 2.02224 2.02696 2.02984 2.03752 6.25568
nspikes <- sapply(spikes, length) # number of spikes from each electrodes
nspikes
# F1_11 F1_12 F1_13 F1_14 F1_21 F1_22 F1_23 F1_24 F1_31 F1_32 F1_33 F1_34 F1_41 F1_42 F1_43 F1_44 
# 1086  1744   107   483   297  3927  3016  1340  1891   584   267   597   391  2786  1050   618 
# pncells - the number of electrodes in the well that spike at least once
ncells <- as.integer(length(nspikes)) # 16
# pbeg
beg=floor(min(unlist(spikes)))
beg # 0
# pend
end=ceiling(max(unlist(spikes)))
end # 900
# pwid
wid <- as.double(ns.T) # 0.05, based on compute.ns
# pnbins - The number of time bins. Calculated in spikes.to.count2
nbins <- ceiling( (end-beg) / ns.T) # 18000
# counts
count = integer(nbins) # creates a vector of 0's that is the length of nbins

# from ns_count_activity
# Sfloat *p, beg, end, wid;
# int ncells, last, b, n, unit, nbins;
# 
# ncells = *pncells; beg = *pbeg; end = *pend; wid = *pwid;
# nbins = *pnbins;

# p = allspikes;
p <- 1 # hmm, this is going a bit differently... noting that R indexes at 1, while C indexes at 0
#  for (unit=0; unit<ncells; unit++)
for(unit in seq(1,ncells)) {
  # /* Count the spikes on electrode UNIT. */
  n = nspikes[unit];
  last = -1;		#	/* check to only increment bin once per unit. */
  
  # while(n-- >0) - post-increment: 1) evaluate if n>0, 2) decrement n
  while(n >0) {
    # b = (int) ( (*p++ - beg)/wid); # /* calc bin number; increment spike ptr */
    # int would trunc in C
    # But, since R starts at 1, we want the first bin to be at index 1, not 0
    b = ceiling(( (allspikes[p] - beg)/wid))
    p <- p+1 # icnrement p
    
    #/* Check bin number is valid: shouldn't happen. */
    # if ( (b <0 ) || (b >= nbins))
    if ( (b <0 ) || (b >= length(count))) {
      sprintf("bin number wrong %f %d\n", allspikes[p-1], b)
      stop("found it!")
    }
    else {
      #/* Update count in relevant bin. */
      if (last != b) {
        count[b] <- count[b] + 1 # count[b]++;
        last = b		# /* stop this bin being updated again for
         # * current unit. */
      }
    }
    n <- n - 1 # added for r, decrement n
  }
}
