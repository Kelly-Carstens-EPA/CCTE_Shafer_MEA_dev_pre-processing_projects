# Documentation of calculation for each parameter

The script provides details on how each parameter is calculated, and which scripts do the calculations

# Still need to verify code:

## Burst Rate 
## Mean Burst Duration
## Mean Interburst Interval
## Interspike Interval in a Burs
## Percent of Spikes in Burst

# Have verified in code:

General info: some parameter values defined in `chgv_parameters.R` (in `meadq`):
elec.min.rate <- 0
elec.max.rate <- 1000
well.min.rate <- 0

mi.par <- list(beg.isi =    0.1,
               end.isi =    0.25,
               min.ibi =    0.8,
               min.durn =   0.05,
               min.spikes = 5)

ns.T <- 0.05 - time length of bins (s)
ns.N <- 4 - the number of spiking electrodes must be greater than 4 for a network spike to count
sur<-100 - number of time bins before and after a network spike peak in which `find.halfmax` will look for the beginning and end of a network spike


## Mean Firing Rate
Scripts: (fun) `construct.s` (`meadq`), called in (fun) `h5.read.spikes`, called in `create_ont_burst_Data.R`
How it's calculated:
For each electrode, total # of spikes / total time (s). Then averaged over AE in `create_ont_burst_Data.R`

## Number of Active Electrodes (AE)
Scripts: `create_burst_ont_Data.R`

How it's calculated:
Sum of the number of electrodes where the s[[1]]$meanfiringrate*60 >= 5

## Number of Actively Bursting Electrodes (ABE)
Scripts: `create_burst_ont_Data.R`

How it's calculated:
Sum of the number of elctrodes where the bursts.per.min >= 0.5 (s[[cur.file]]\$bs\$bursts.per.min >= 0.5 )
    
## Number of Network Spikes * minor bug
Scripts: `create_burst_ont_Data.R` -> ... -> `sjemea.c` (fun: `ns_count_activity`) and `find.peaks()`

How it's calculated:
Divide entire recording into time bins of length ns.T = 0.05. (`networkspikes.R` (sjemea))
Count the number of spiking electrodes in each time bin (`sjemea.c` (fun: `ns_count_activity`))
Cycle though each time bin. When you find a time bin with count > 0, set peak = count at that time bin. If the count is larger at the next time bin, replace peak. Continue until you get to a time bin with count of 0. Thus 'peak.val' is the largest value of 'count' in a set of time bins enclosed by 2 time bins of count=0.\* If 'peak.val' is greater than ns.N = 4, then set of time bins is determined to be a network spike. Otherwise, 'peak.val' is not recorded. (`networkspikes.R`, (fun) `find.peaks`)

Create table like the one below, where each row corresponds to a network spike. "time" is the time at the peak of the network spike. The index is the time-bin index of the network spike (`networkspikes.R`, (fun) `mean.ns`).

time index peak.val      durn
10.10   203       14 0.2233333
38.25   766       13 0.2354167
57.25  1146       13 0.2729167
101.45  2030       14 0.2466667
113.10  2263       14 0.2033333
144.85  2898       14 0.2416667
162.30  3247       12 0.3000000
179.30  3587       13 0.2475000
190.60  3813       11 0.3166667
217.75  4356       14 0.2250000
227.60  4553       14 0.2066667
256.75  5136       13 0.2437500

The number of network spikes is the number of rows of this table.

\* Does the number of spikes really have to get back to 0 for it to be a completed network spike? Like, what if there is just some background electrode... So, we could be undercounting the number of network spikes.

Minor bug: In `networkspikes.R` (sjemea) in (fun) `mean.ns`, any network spike occuring within 100*ns.T of the beginning or end of the recording are removed (ns.T = 0.05 seconds). I doubt many network spikes occur within 5 seconds of the beginning or end though, and the beginning and end of the recording are arbitrary endpoints anyhow.

## Network Spike Peak
Scripts: same as previous

How it's calculated:
As described above, create the network spike summary table for each well. Then find the mean of the 'peak.val'. The mean value is stored in `ns$brief`. However, the final value used is taken from `summary.network.spikes.dh.R`. Which is taken from another iteration of `mean.ns`

## Mean Network Spike Duration
Scripts: `networkspikes.R` (fun) `mean.ns`, (fun) `find.halfmax`, (fun) `compute.ns` (sjemea)

In (fun) `find.halfmax`
For a given network spike, let 'peak.t' be the time bin of 'peak.val'. Then, find the time bins before and after 'peak.t' where the 'count' is equal to half of 'peak.val'.

In (fun) `mean.ns`
The duration of each netwrok spikes is the difference between the indicies of the start and end time bins, then multiply by the length of a time bin (ns.T = 0.05 s).

In `compute.ns`
Find the mean of the the durations of all network spikes in each well.

The mean value of durations is stored in `ns$brief`. However, the final value used is taken from `summary.network.spikes.dh.R`. Which is taken from another iteration of `mean.ns`

## Standard Deviation of Network Spike Duration
Scripts: `networkspikes.R` (fun) `mean.ns`, (fun) `find.halfmax`

## Percent of spikes in Network Spikes * skeptical of this one
Scripts: `summary.network.spikes.dh.R` (see definition of `ns$brief`)
How it's calculated: 

For a given well,
'#' of spikes in network spikes / total # of spikes from any electrode in the well

**however, I diasgree with how the number of spikes in network spikes is calculated. For each network spike, counts all of the spikes in well that occured from the time of the network spike (peak.t) to the next time bin (ns.T).
Shouldn't we use the duration of the network spike, not just 1 time bin?
The network spike duration, using halfmax approach, is usualy ~1-2.5 seconds, >> 1 ns.T = 0.05 s

## Mean Number of Spikes in Network Spikes * skeptical of this one
Scripts: `summary.network.spikes.dh.R`

How it's calculated: 
'#' of spikes in network spikes (same as in percent of spikes in ns) / total number of network spikes
Again, I am skeptical of how the # of spikes in network spikes is calculated. 

Also, is this really how we want to calculate the mean? Alternatively, we could 
use en.map , which is a matrix with rows of the electrodes in well, and columns the peak.t of ea network spike in `summary.network.spikes.dh.R`
x = colSums(en.map) - to get the total number of spikes in each network spike
mean(x) - that would be the mean number of spikes in network spikes

## Interspike Interval in Network Spikes * skeptical of this one
Scripts: `create_burst_on_Data.R` - taken from s[[cur.file]]\$ns.all\$brief['mean.insis']
`summary.network.spikes.dh.R` - where ns.all\$brief is calculated
How it's calculated:
Start with the network spike matrix, as shown in the introduction. Find the difference in time between each consecutive network spike. Average the differences. (Note that the time is the time at the peak of the network spike)

Alternatively, we could look for the time between the approximate end of a network spike and the approximate beginning of the next network spike (using the duration, derived from the half-max approach). I guess this is lower priority, since it's all normalized to the controls anyhow

## Mean correlation
Scripts: `create_burst_ont_Data.R` -> `local.corr.all.ont.ae.filter.R`
Calculate Pearson correlation calculated between pairs of active electrodes. Then, for each electrode, average the pairwise correlations. Then, average the mean correlations from each electrode.
Note: the (spike) tiling coefficient is calculated in `tiling.c` (sjemea). Might revisit how he calculated this previously.