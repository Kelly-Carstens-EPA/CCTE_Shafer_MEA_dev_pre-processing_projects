# to do:
high priority: 
* comb through this doc from after burst parameters again, see what needs to be done
* check how isi in a burst is calculated
* fix anythign other scripts that need to be fixed
* MI - can I validate this? or is it above me?
* review your changes in the code, make sure it's right

ideas:
* fix minor bug in network spike table list
* experiment with going from time bin of 1 to 1, instead of 0 to 0 for network spikes
* fix network spike things
* compare with how calculated in maetro book

# Documentation of calculation for each parameter

The script provides details on how each parameter is calculated, and which scripts do the calculations

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


## Bursting parameters
Scripts: (fun) `mi.find.bursts` in `maxinterval.R` (sjemea), called by `create_burst_ont_Data.R`

Uses a version of the max interval method
Phase 1: find any set of spikes where first pair of spikes is less than 0.1 s apart, and last 2 spikes are less than or equal to 0.25 s apart
Phase 2: any bursts with IBI < 0.8 are merged together
Phase 3: any bursts < 5 spikes or < min.durn are removed

Returns a table of the form:
| beg | end | IBI | len | durn | mean.isis | SI |
| ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- |
| 19 | 25 | NA | 7 | 0.10640 | 0.01773333 | 1 |
| 28  33  5.42920 | 6 | 0.09480 | 0.01896000 | 1 |
| 38 | 46 | 6.89864 | 9 | 0.13504 | 0.01688000 | 1 |
| 72 | 78 | 27.27240 | 7 | 0.12040 | 0.02006667 | 1 |
| 92 | 98 | 25.09456 | 7 | 0.06904 | 0.01150667 | 1 |

IBI column is the time from the last spike of previous burst to the first spike of the current burst
mean.isis for an burst is durn of given burst / (nspikes in burst - 1)

What needs to change:
The above method leads to some unintended outcomes, namely:
- Burst creation - If two sets of 4 or fewer spikes labelled as a 'bursts' in phase 1 are within 0.8 s of each other, they will be merged into 1 burst in phase 2. Then this set of spikes can pass through phase 3 as a valid burst
- Burst extension - If a set of 4 or fewer spikes labelled as a 'burst' found in phase 1 is within 0.8 s of a valid burst, the bursts will be merged together. Thus the duration, # of spikes, and interspike interval in the valid burst are extended.
- Burst merging - If two valid bursts are less than 0.8 seconds apart, these bursts will be merged into 1 burst in phase 3

New approach in `mi.find.bursts_edits.R`:

The order of phase 2 and 3 is switched. That is,

Phase 1: find any set of spikes where first pair of spikes is less than 0.1 s apart, and last 2 spikes are less than or equal to 0.25 s apart
Phase 3: any bursts < 5 spikes or < min.durn are removed
Phase 2: any bursts with IBI < 0.8 are merged together

These changes should eliminate "burst creation" and "burst extension". They do not address "burst merging" - we might want to remove Phase 2 altogether to prevent that.

Parameters affected by changes:
burst rate - would decrease - because the burst duration would be artificially raised by merging mini "bursts" into something that would be long enough / enough spikes to count as a valid burst in phase 3
mean burst duration - would definitely decrease
mean interburst interval - would increase
percent of spikes in burst - have not looked into yet

If we don't implement these changes, we could just re-define a burst as a set of spikes for which:
* The first set of spikes is within 0.1 (that actually does not change)
* the ISI must stay less than 0.25, unless,
* there can be an ISI greater than 0.25 only if it is followed by an ISI less than 0.1
* All spikes are within 0.8 of each other

## Burst Rate * Recently revised
(location in `s` object: ``s[[cur.file]]$bs$bursts.per.min`)

in `calc.burst.summary` in `create_ont_burst_Data.R`:
The number of burst on an electrode the # rows of the burst table, as shown above
The bursts per min ( # bursts / duration of the recording)*60
For each well, the average bursts per minute of active elctrodes is found

## Mean Burst Duration * Recently revised
(location in `s` object: `s[[cur.file]]$bs$mean.dur`), averaged on AE

in `calc.burst.summary` in `create_ont_burst_Data.R`:
`mean.dur` is the average "durn" from the burst table for each electrode
It is the found by spikes[data[,"beg"]] - spikes[data[,"end"]] - i.e., the time at the index of the first spike, to the time at the index of the last spike in teh burst.
For each well, the average mean duration of bursts on actively bursting elctrodes is found

## Mean Interburst Interval * Recently revised
(location in `s` object: `s[[cur.file]]$bs$mean.IBIs`)
Scripts: `calc.ibi` -> `calc.all.ibi`

In the burst summary table above, the IBI column is the time from the last spike of the previous burst to the first spike of the current burst.
For each electrode, find the average inter burst interval.
For each well, find the average of the mean inter burst intervals on each actively bursting electrode

## Interspike Interval in a Burst * need to figure this one out
`create_burst_ont_Data.R`
calls `calc.burst.summary`
calls `calc.all.isi` (sjemea)


`create_burst_ont_Data.R`
calls `calc.burst.summary`

calls `calc.all.isi` (sjemea) - for each electrode, returns a vector of the time between all spikes that were in bursts
calls `calc.burst.summary` - averages the interspike intervals within bursts for each electrode. Returned in res$mean.isis

in `create_burst_ont_Data.R`, stored in `s[[cur.file]]$bs$mean.isi`. averaged over actively bursting electrodes

(note: the "SI" (spike interval?) is calculated in `calc.burst.summary` as well. However, this value is not used in create_burst_ont_Data.R)


## Percent of Spikes in Burst * Recently revised (only change a little bc of burst creation and extension)
(location in `s` object: `s[[cur.file]]$bs$per.spikes.in.burst`)

in `calc.burst.summary` called in `create_ont_burst_Data.R`:
For each electrode, sum up total number of spikes in burst from burst table (the "len" column).
Then divide by total number of spikes on that electrode.
For each well, find the average percent of spikes in burst on ABE



## Mean Firing Rate
Scripts: (fun) `construct.s` (`meadq`), called in (fun) `h5.read.spikes`, called in `create_ont_burst_Data.R`
How it's calculated:
For each electrode, total # of spikes / total time (s). Then averaged over AE in `create_ont_burst_Data.R`

## Number of Active Electrodes (AE)
Scripts: `create_burst_ont_Data.R`

How it's calculated:
Sum of the number of electrodes where the s[[1]]$meanfiringrate*60 >= 5

## Number of Actively Bursting Electrodes (ABE) * Recently revised, bc of chnages in burst table
Scripts: `create_burst_ont_Data.R`

How it's calculated:
Sum of the number of elctrodes where the bursts.per.min >= 0.5 (s[[cur.file]]\$bs\$bursts.per.min >= 0.5 )
    
## Number of Network Spikes * minor bug
Scripts: `create_burst_ont_Data.R` -> ... -> `sjemea.c` (fun: `ns_count_activity`) and `find.peaks()`

Note: 
Here, the network *spikes* are calculated. Network bursts are a different phenomena, which require that electrodes are bursting invidually, before it can be considered part of a network burst.

How it's calculated:
Divide entire recording into time bins of length ns.T = 0.05. (`networkspikes.R` (sjemea))
Count the number of spiking electrodes in each time bin (`sjemea.c` (fun: `ns_count_activity`))
Cycle though each time bin. When you find a time bin with count > 0, set peak = count at that time bin. If the count is larger at the next time bin, replace peak. Continue until you get to a time bin with count of 0. Thus 'peak.val' is the largest value of 'count' in a set of time bins enclosed by 2 time bins of count=0.\* If 'peak.val' is greater than ns.N = 4, then the set of time bins is determined to be a network spike. Otherwise, 'peak.val' is not recorded. (`networkspikes.R`, (fun) `find.peaks`)

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

\* Does the number of spikes really have to get back to 0 for it to be a completed network spike? Like, what if there is just some background electrode... So, we could be undercounting the number of network spikes. - actually,  I think this is okay. Would we really want to split a network spike into 2? depends...

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

## Percent of spikes in Network Spikes * Recently revised
Scripts: `summary.network.spikes.dh.R` (location in `s` object: `s[[cur.file]]$ns.all$brief['percent.of.spikes.in.ns']`)
`mean.ns`, `find.halfmax` (`sjemea`)
How it's calculated: 

We want to find:
total # of spikes in network spikes / total # of spikes from any electrode in the well.

Previous method:
To find the total # of spikes in network spikes in a given well, the script finds the number of spikes on each electrode that occured at the time bin (0.05 s) of the peak of the network spike. If an electrode has less than ns.E = 2 spikes in that time frame (i.e., only 1 spike during the network spike), then it is not counted as a spike in the network spike. The total number of these spikes is summed, then divided by the total number of spikes in the given well.

Why want to change:
I think we should use the duration of the network spike bin (as determined with the halfmax approach), not just 1 time bin
The network spike duration, using halfmax approach, is usualy ~0.15 - 0.35 seconds, so a typical network spike would cover 3-7 time bins of ns.T = 0.05 s.

New approach:
In `summary.network.spikes.dh_edits.R`:

To find the total # of spikes in network spikes in a given well, the script finds the total number of spikes on each electrode that occured at a time greater than or equal to the left half max time point or less than the right half max time point. If an electrode has less than ns.E = 2 spikes in that time frame (i.e., only 1 spike during the network spike), then it is not counted as a spike in the network spike. The total number of these spikes is summed, then divided by the total number of spikes in the given well.

## Mean Number of Spikes in Network Spikes * Recently revised
Scripts: `summary.network.spikes.dh.R` (location in `s` object: `s[[cur.file]]$ns.all$brief['mean.spikes.in.ns']`)
`mean.ns`, `find.halfmax` (`sjemea`)

How it's calculated: 

We want to find:
total # of spikes in network spikes / total number of network spikes

Previous method:
The total number of spikes in network spikes was calcualted as in 'Percent of Spikes in Network Spikes'. The total number of network spikes is the number of rows in the network spike summary table. 

New approach:
In `summary.network.spikes.dh_edits.R`:

Again, the total number of spikes in network spikes is now calculated using the 'duration' of the network spikes. The total number of network spikes is the same.

## Inter-Network Spike Interval * skeptical of this one, but I don't think we'll change it
Scripts: `create_burst_on_Data.R` - taken from s[[cur.file]]\$ns.all\$brief['mean.insis']
`summary.network.spikes.dh.R` - where ns.all\$brief is calculated
How it's calculated:
Start with the network spike matrix, as shown in the introduction. Find the difference in time between each consecutive network spike. Average the differences. (Note that the time is the time at the peak of the network spike)

Alternatively, we could look for the time between the approximate end of a network spike and the approximate beginning of the next network spike (using the duration, derived from the half-max approach). I guess this is lower priority, since it's all normalized to the controls anyhow

*** The name in tcpl and Brown 2016 implies that this endpont calculates the interspike interval in network spikes. But it actually calculates the inter-network spike interval

## Mean correlation
Scripts: `create_burst_ont_Data.R` -> `local.corr.all.ont.ae.filter.R`
Calculate Pearson correlation calculated between pairs of active electrodes. Then, for each electrode, average the pairwise correlations. Then, average the mean correlations from each electrode.
Note: the (spike) tiling coefficient is calculated in `tiling.c` (sjemea). Might revisit how he calculated this previously.

## Notes
The function `construct.R` is in both the meadq and sjema package. The only difference appears to be a patch fix in the meadq version - if there were no spikes on an electrode, the meanfiringrate = nspikes = list().
`map2list` is also in both packages