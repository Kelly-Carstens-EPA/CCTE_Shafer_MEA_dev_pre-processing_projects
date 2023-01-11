Notes on plots
Dec 6, 2021

Master data set for these plots:
Includeas the "longfile" from the following projects:
Brown2014
DNTGF2019
Frank2017
NTP91
OPP2015
PFAS2018
ToxCast2016


* mea_nfa_solvent_controls_comparison.pdf
	Data used: All controls wells with wllq == 1
	Takeaways: 
		- Most of the DMSO 0.15% wells are outside the 3rd quartile of the DMSO 0.1% wells, but within 1.5 IQR. 
		- Water and DMSO 0.1% generally seem quite comparable, with water perhaps on the high end.
		- (Of course, this pattern does not hold true for every endpoint. But I'm not seeing any cases where the median of DMSO 0.15% or Water is outside the typical threshold for outliers of the DMSO 0.1% wells.)
		- I wonder if most of the differences in DMSO 0.15% and DMSO 0.1% are due to differences in the specific culture that used the non-standard vehicle controls, than than the vehicle controls themselves?

So, let's compare the non-standard vehicle controls with the DMSO 0.1% wells from the same culture and plate: 


* apid_with_multiple_solvent_controls.pdf
	Data used: Control wells from any apid with multiple solvent treatments OR concentrations is shown.
	Takeaway: Visually, I don't see a noticeable or consistent difference between the solvent controls within a given apid/culture.


Another way of looking at it: Normalize to the median of the (pooled) vehicle controls from the same apid.

* mea_nfa_solvent_controls_comparison_normalized.pdf
	Data used: All controls with wllq == 1. Normalized resp calculated as resp := (rval - median(rval))/median(rval) (of controls only)
	Takeaways: 
		- I'm seeing even fewer differences by apid than when I looked at the rval's. This confirms to me that the differences we do see by solvent are mostly due to differences in the culture - not the solvents themselves. 
		- Yes, there are some apid for which all of the solvent controls are water. So normalizing to the median of the water wells would other course center the values around 0. But in these cases, I think it's safe to assume that everything on these plates was dissolved in water, so of course we are going to normalize to the water wells exclusively!