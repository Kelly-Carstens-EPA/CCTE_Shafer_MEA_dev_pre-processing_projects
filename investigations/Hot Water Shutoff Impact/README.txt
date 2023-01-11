_v2 files have 9 apid removed where the number of usable treatments (wllt == 't') with wllq == 1 on the plate is less than or equal to 1.
This was used to eliminate looks at control wells from plates that were probably bad, and I now see should be removed entirely (particularly from the DNT NTP 2021 data set)

Backstory: I set wllq to 0 for individual samples that I determiend to be "noisy" and need to be rescreened. But I did not change the wllq for the DMSO control wells. Now I'm thinking I should, so that those control wells are not included in the claculation of the bmad.

August 2, 2022:

After going over the results with Seline, we have a hypothesis about the observed trends in network spike activity over time (regardless of the hot water status):
**Possible explanation for decrease in # network spikes and increase in network spike duration seen over time: the newer maestro is more sensitive!! So me thinks: because network spikes require a window of exactly 0 spikes to define the beginning and end of a spike, if the newer machine is picking up more noise, then there are probably more instances when what might have previously been considered separate network spikes are not getting merged into 1!
This hypotheses agrees with the trends in the mean network spike duration, which is tending to increase in value and in variance.