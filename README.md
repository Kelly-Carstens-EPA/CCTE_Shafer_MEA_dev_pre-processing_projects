# Microelectrode Array Network Formation Assay Spike List Pe-Processing Scripts

## Summary/background of this assay
What this is:
  Network Formation Assay Concentration-Response

See these papers for more in depth. As a quick summary, cortical cells are grown on 48-well microelectrode-containing plates. The cells are grown for 12 days. 
The goal is to determine if some compounds significantly alter the development of a neuronal network. So, the acitivity of the neurons in each well is recorded as the network develops, on days 5, 7, 9, and 12.
Each 6x8 plate contains 6 compounds with 7 concentrations of each compound. Each set of compounds and concentrations is replicated on 3 plates.
Because we want to measure any changes to the development of the neuronal network, several parameters are calculated from the raw spike data.
In order to condense this data overtime into one value, the area under the curve is calculated for each well...

## Purpose of these scripts

We collect the raw spike list data
We wanna get the dose response curve, and determine which are positive hits.

These scripts process the raw spike list data (of the format jfkdja ) in order to transform the data into a long file format. 
Then, the data can be processed with the ToxCast Pipeline to make the dose response curves and hit calls.

prepare them to be processed in the toxcast pipeline. (the endpoints are calculated, AUC calculated, data put in long file format, and cytotox data added)
The raw data used is a long file recording of the 
		

| Time (s) | Electrode | Amplitude(mV) |
| ----------- | ----------- | ----------- |
  
Endpoints calculated, well-level values taken...
Put in a long file format so that can be processed with tcpl in order to create dose-response curves and determine if a compound is a positive hit call.

## How to use these scripts

For a step-by-step guide, see the document *Step-by-Step_Guide.docx*. Below is a diagram showing the general flow of the process and the scripts used at each step. Raw data files are shown in blue. Intermediate output files are shown in purple.
![spikelist_to_mc0_overview](/images/SpikeList_to_mc0_overview.png)

## Narrative of the process


