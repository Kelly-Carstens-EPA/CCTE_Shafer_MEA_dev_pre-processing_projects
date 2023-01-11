# 10/19/2020
library(data.table)
library(openxlsx)

root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl"
source(file.path(root_output_dir,"supplemental_scripts","get_latest_dat.R"))

alldat <- get_latest_dat()
# Loading longfile.Rdata for...
# Brown2014 
# DNTGF2019 
# Frank2017 
# NTP91 
# OPP2015 
# PFAS2018 
# ToxCast2016

# Gathering all SPIDs -----------------------------------------------------------------------------------
all_spid_files <- list.files("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Sample IDs", pattern = "\\.xlsx", full.names = T)

# this file has signficantly diff file names
dnt_spidmap <- as.data.table(read.xlsx(grep("All Assays_list_toxcast_OECD 20190524",all_spid_files,val=T),sheet="NFA Groups"))
setnames(dnt_spidmap, old = c(2,3), new = c("PREFERRED_NAME","EPA_SAMPLE_ID"))
# for example, setnames(spidmap, old = c("Aliquot_Vial_Barcode", "Concentration", "EPA_Sample_ID"), new = c("treatment","stock_conc","spid"))
dnt_spidmap[, EPA_SAMPLE_ID := paste0("EPAPLT0",EPA_SAMPLE_ID)]
dnt_spidmap <- dnt_spidmap[, .(PREFERRED_NAME,EPA_SAMPLE_ID, Conc)]
dnt_spidmap[, FILENAME := "All Assays_list_toxcast_OECD 20190524.xlsx"]

all_spid_files <- Filter(function(filei) !grepl("All Assays_list_toxcast_OECD 20190524",filei), all_spid_files)
spidmap <- rbindlist(lapply(all_spid_files, function(filei) {
  tb <- as.data.table(read.xlsx(filei,sheet=1))
  tb$filename <- basename(filei)
  setnames(tb, old = names(tb), new = toupper(names(tb)))
  setnames(tb, old = "DSSTOX_PREFERRED_NAME", new = "PREFERRED_NAME", skip_absent = T)
  return(tb) }
), use.names = T, fill = TRUE)
spidmap <- rbindlist(l = list(spidmap, dnt_spidmap), use.names = T, fill = TRUE)
head(spidmap)
unique(spidmap$EPA_SAMPLE_ID)
spidmap[!is.na(PD_SAMPLE) & PD_SAMPLE != EPA_SAMPLE_ID] # cool, can ignore that column

# remove all spids from this file except the 3 PFAS ones
spidmap <- spidmap[FILENAME != "EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx" | PREFERRED_NAME %in% c("1,1,2,2-Tetrahydroperfluoro-1-decanol","1H,1H,2H,2H-Perfluorooctyl iodide","Perfluoroundecanoic acid")]

# first, confirm I have a spidmap for all spid's preset in alldat
alldat[spid %in% setdiff(alldat[wllt == "t", unique(spid)], unique(spidmap$EPA_SAMPLE_ID)), .(unique(treatment)), by = c("spid","dataset")]
# empty


# Tabulate tested compounds with multiple SPIDs available -------------------------------------------------------------------------
# find the spids that correspond to the same compounds
spidmap[is.na(PREFERRED_NAME)] # empty
check_trts <- spidmap[, .(length(unique(EPA_SAMPLE_ID))), by = .(PREFERRED_NAME)][V1 != 1, unique(PREFERRED_NAME)] # there are 82 compounds with multiple spids
spidmap[, .(length(unique(PREFERRED_NAME))), by = .(EPA_SAMPLE_ID)][V1 != 1] # empty - so preferred name is consistent

# if neither spid has been used - then that compound hasn't been tested, so can eliminate
check_trts_spids <- spidmap[PREFERRED_NAME %in% check_trts, unique(EPA_SAMPLE_ID)] # get the corresponding spids
check_trts_spids <- alldat[spid %in% check_trts_spids, unique(spid)] # get the spids of these treatments that are used - to filter out the treatments not tested anywhere with any spid
check_trts <- spidmap[EPA_SAMPLE_ID %in% check_trts_spids, .(length(unique(EPA_SAMPLE_ID))), by = .(PREFERRED_NAME)][V1 != 1, unique(PREFERRED_NAME)] # filtered down to 18
# PREFERRED_NAME V1
# 1:                       Chlorpyrifos  2
# 2:                  Chlorpyrifos oxon  2
# 3:                       Bisphenol AF  2
# 4:              6-Propyl-2-thiouracil  2
# 5:                          Parathion  2
# 6:                       Phenanthrene  2
# 7: 3-Iodo-2-propynyl-N-butylcarbamate  2
# 8:                             Captan  2
# 9:                            Aspirin  2
# 10:                   17beta-Estradiol  2
# 11:                           Rotenone  2
# 12: Potassium perfluorohexanesulfonate  2
# 13:                         D-Glucitol  2
# 14:         Di(2-ethylhexyl) phthalate  2
# 15:                    Hexachlorophene  2
# 16:                         Permethrin  2
# 17: 2,2',4,4'-Tetrabromodiphenyl ether  2
# 18:                      Glufosinate-P  2
check_trts_spids <- spidmap[PREFERRED_NAME %in% check_trts, unique(EPA_SAMPLE_ID)]

# for each of these, did I assign the correct spid for the correct cultures?
multi_spid_summary <- merge(alldat[spid %in% check_trts_spids, .N, by = .(treatment, spid, dataset)], 
      spidmap[PREFERRED_NAME %in% check_trts, .(EPA_SAMPLE_ID, PREFERRED_NAME, FILENAME, ALIQUOT_PLATE_BARCODE)], by.x = c("spid"), by.y = c("EPA_SAMPLE_ID"), all = T)
write.csv(multi_spid_summary[order(PREFERRED_NAME)], file = file.path(root_output_dir, "true_spid_replicates.csv"), row.names=F)
# again, these are all of the compounds where there are multiple spid's available,
# and I have used 1 of the spids at least one of them.
# This table shows all available spids for those compounds, and where I have or have not used those spids.

# Summarize spidmap files sources used for each dataset --------------------------------------
spidmap[FILENAME == "EPA_9238_EPA-Shafer_75_20180511_key_MW Waste Calculations.xlsx", ALIQUOT_PLATE_BARCODE := "EPA_9238_EPa-Shafer_75 (PFAS)"]
spidmap[FILENAME == "All Assays_list_toxcast_OECD 20190524.xlsx", ALIQUOT_PLATE_BARCODE := "OECD 20190524 (DNT)"]
alldat[, spid_source := spidmap[match(alldat$spid, spidmap$EPA_SAMPLE_ID), ALIQUOT_PLATE_BARCODE]]
alldat[, spid_file := spidmap[match(alldat$spid, spidmap$EPA_SAMPLE_ID), FILENAME]]
alldat[, .(paste0(sort(unique(spid_source)),collapse=",")), by = .(dataset)]
write.csv(alldat[wllt == "t", .(number_of_spids = length(unique(spid)), ifelse(length(unique(spid))<7, paste0(sort(unique(treatment)[1:3]),collapse=","), "")), by = .(dataset, spid_source, spid_file)][order(dataset, -number_of_spids)],
          file = file.path(root_output_dir,"summary_of_spids_by_dataset.csv"), row.names = F)


# Compounds with variable solvent controls ---------------------------------------------------

# group by corresponding solvent control (i.e., was a compound dissolved in different things in diff cultures?) Should this data be combined, or kept separate?
trt_apid_rowi_summayr <- alldat[wllt == "t", .N, by = .(treatment, spid, apid, rowi, dataset)]
control_summary <- alldat[wllt == "n", .N, by = .(treatment, spid, apid, rowi, dataset)]
solvent_summary <- merge(trt_apid_rowi_summayr, control_summary, by = c("apid","rowi","dataset"), suffixes = c(".trt",".cntrl"), all = T)
solvent_summary[, .(length(unique(spid.cntrl))), by = .(spid.trt)][V1 != 1]
# spid.trt V1
# 1: EX000404  2
# 2: EX000411  2
solvent_summary[spid.trt %in% c("EX000404","EX000411")][order(spid.trt, dataset, apid)]
alldat[treatment == "Loperamide", spid := "EX000411"] # changing this in Brown2014, because casrn actually corresponds to Lop hydrochloride

col_map <- data.table(dataset = unique(alldat$dataset), 
                      col = unlist(lapply(c("black","blue","green","red","orange","purple","cyan"), function(col) rgb(t(col2rgb(col))/255, alpha = 0.5))))
view_replicates_by_dose <- function(alldat, use_treatment = NULL, use_spid = NULL, acsni = "CCTE_Shafer_MEA_dev_firing_rate_mean") {
  plotdat <- alldat[spid %in% use_spid | treatment %in% use_treatment]
  datasets <- unique(plotdat$dataset)
  plotdat[, conc_index := signif(log10(conc),3)]
  plotdat$conc_index <- factor(plotdat$conc_index, levels = sort(unique(plotdat$conc_index)), ordered = T)
  stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", 
             col = col_map[dataset == dataseti, col])
  for (dataseti in datasets) {
    stripchart(rval ~ conc_index, plotdat[acsn == acsni & wllq == 1 & dataset == dataseti], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", 
               col = col_map[dataset == dataseti, col], add = T)
  }
  abline(h = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)])
  text(x = length(unique(signif(plotdat$conc))), y = alldat[wllt == "n" & acsn == acsni & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(1,-0.2))
  title(main = paste0(unique(plotdat$treatment)[1]," MFR AUC by dose\nfrom ",paste0(unique(plotdat$dataset),collapse=", ")))
  axis(side = 1, at = seq(1, length(unique(signif(plotdat$conc)))), labels = sort(unique(signif(plotdat$conc,3))))
  legend(x = "topright", legend = c(datasets), col = col_map[match(datasets, dataset), col], pch = 19, bg = "transparent", cex = 0.8)
  rm(list= c("plotdat"))
}

# Acetaminophen
alldat[treatment == "Acetaminophen", .N, by = "dataset"]
adat <- alldat[treatment == "Acetaminophen"]
stripchart(rval ~ signif(log10(conc),3), adat[grepl("firing_rate_mean$",acsn) & wllq == 1], vertical = T, pch = 1, xaxt = "n", xlab = "conc (uM)")
abline(h = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)])
text(x = 1, y = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(0,-0.2))
title(main = paste0("Acetaminophen MFR AUC by dose from ",paste0(unique(adat$dataset),collapse=", ")))
axis(side = 1, at = seq(1, length(unique(signif(adat$conc)))), labels = sort(unique(signif(adat$conc,3))))
# as far as the plot/hit call, I think it would be fine to include or not include all of the replicates

# Loperamide
ldat <- alldat[spid %in% c("EX000411", "EX000492")]
stripchart(rval ~ signif(log10(conc),3), ldat[grepl("firing_rate_mean$",acsn) & wllq == 1], vertical = T, pch = 19, xaxt = "n", xlab = "conc (uM)", col = rgb(0.5,0.5,0.5,0.5))
abline(h = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)])
text(x = length(unique(signif(ldat$conc))), y = alldat[wllt == "n" & grepl("firing_rate_mean$",acsn) & wllq == 1, median(rval)], labels = "median of all controls", cex = 0.7, adj = c(1,-0.2))
title(main = paste0("Loperamide MFR AUC by dose\nfrom ",paste0(unique(ldat$dataset),collapse=", ")))
axis(side = 1, at = seq(1, length(unique(signif(ldat$conc)))), labels = sort(unique(signif(ldat$conc,3))))
# as far as the plot/hit call, I think it would be fine to include or not include all of the replicates
rm(list= c("ldat","adat"))


# Confirm SPIDs that are used in multiple datasets --------------------------------------------
# (confirm lot number, etc., matches for each case)
alldat[wllt == "t", .(num_datasets = length(unique(dataset)), trt = paste0(unique(treatment)[1],collapse=","), datasets = paste0(sort(unique(dataset)),collapse=",")), by = .(spid)][num_datasets != 1]
#        spid num_datasets                                   trt                               datasets
# 1: EX000404            3                         Acetaminophen           Brown2014,Frank2017,PFAS2018
# 2: EX000475            2                 Bisindolylmaleimide I                    Brown2014,Frank2017
# 3: EX000487            2                         L-Domoic acid                    Brown2014,Frank2017
# 4: EX000411            4                            Loperamide Brown2014,DNTGF2019,Frank2017,PFAS2018
# 5: EX000498            2                            Mevastatin                    Brown2014,Frank2017
# 6: EX000499            2                  Sodium orthovanadate                    Brown2014,Frank2017
# 7: EX000408            2                            Glyphosate                    DNTGF2019,Frank2017
# 8: EX000361            2 Tris (2-chloroethyl) phosphate (TCEP)                        Frank2017,NTP91
# 9: EX000420            2                           Bisphenol A                     Frank2017,PFAS2018
# 10: EX000362            2                           Valinomycin                      NTP91,ToxCast2016
# Glyphosate and BPA are my only concerns... will run this by Tim


# any SPID repeated many times (likely assay controls) - decide if keep all data -------------------------
alldat[wllq == 1 & wllt == "t", .(num_apid = length(unique(apid)), trt = unique(treatment)[1]), by = .(spid)][num_apid > 6][order(-num_apid)]
# spid num_apid                    trt
# 1: EX000411       22             Loperamide
# 2: EX000404       21          Acetaminophen
# 3: EX000374        9 L-Glufosinate Ammonium
# 4: EX000408        9             Glyphosate
# 5: EX000413        9   Cytosine Arabinoside
# 6: EX000420        9            Bisphenol A
# 7: EX000475        7  Bisindolylmaleimide I
# 8: EX000487        7          L-Domoic acid
# 9: EX000498        7             Mevastatin
# 10: EX000499        7   Sodium orthovanadate

# L-Glufosinate
view_replicates_by_dose(alldat, use_spid = "EX000374")
# ya, not worth stressing over. We'll keep all of these

# Glyphosate
view_replicates_by_dose(alldat, use_spid = "EX000408")

alldat[spid == "EX000413", unique(dataset)] # "Frank2017". This is fine to include all I think

# bPA - looks okay acutally. Def not the same, but okay
view_replicates_by_dose(alldat, use_spid = "EX000420")
view_replicates_by_dose(alldat, use_spid = "EX000475")
view_replicates_by_dose(alldat, use_spid = "EX000487")
view_replicates_by_dose(alldat, use_spid = "EX000411")
view_replicates_by_dose(alldat, use_spid = "EX000404")
# I think that all of these are fine, just run it by Tim
graphics.off()
pdf(file = file.path(root_output_dir, paste0("assay_controls_mfr_auc_replicates_oct19_2020.pdf")))
for (spidi in alldat[wllq == 1 & wllt == "t", .(num_apid = length(unique(apid)), trt = unique(treatment)[1]), by = .(spid)][num_apid > 6][order(-num_apid), spid]) {
  view_replicates_by_dose(alldat, use_spid = spidi)
}
graphics.off()


# PLOTS ---------------------------------------------------------
par(mfrow = c(2,1))
# defaults:
# $mar
# [1] 5.1 4.1 4.1 2.1
plot_by_apid <- function(plotdat, threshold = NULL) {
  acsni <- unique(plotdat$acsn)
  plot_desc <- acsni
  if(!grepl("DIV",acsni)) plot_desc <- paste0(plot_desc," AUC")
  
  par(oma = c(2,0,1,0), mar = c(4,4.1,1,2.1))
  plotdat$apid <- factor(plotdat$apid, levels = sort(unique(plotdat$apid)), ordered = T)
  stripchart(rval ~ apid, plotdat, vertical = T, pch = "", las = 2, cex = 1.5, cex.axis = 0.85, xaxt = "n", ylab = sub("CCTE_Shafer_MEA_dev_","",plot_desc), xlab = "apid by culture")
  stripchart(rval ~ apid, plotdat[wllq == 0], vertical = T, pch = 19, las = 2, col = rgb(1,0,0,alpha=0.5), 
             cex = 1.5, cex.axis = 0.85, add = T)
  stripchart(rval ~ apid, plotdat[wllq == 1], vertical = T, pch = 19, las = 2, col = rgb(0,0,1,alpha=0.5), 
             cex = 1.5, cex.axis = 0.85, add = T)
  
  # add the median from each plate
  stripchart(bval*60 ~ apid, plotdat[wllq == 1, .(bval = median(rval)), by = .(apid)], vertical = T, pch = 19, las = 2, col = rgb(0.1,0.1,0.1,alpha=0.75), 
             cex = 1.5, cex.axis = 0.85, add = T)
  
  # add horizontal threshold line, if present
  abline(h = threshold, lty = "dashed", lwd = 2)
  
  # show the separations in culture date
  apid_char <- sort(unique(as.character(plotdat$apid)))
  cultures <- unique(sub("_.*$","",apid_char))
  cultures <- sapply(cultures, function(x) sum(grepl(x,apid_char)))
  abline(v = cumsum(cultures) + 0.5, col = "gray80")
  # text(x = cumsum(cultures) - 0.5*cultures, y = (max(60*plotdat$rval) - min(60*plotdat$rval))*.9 + min(60*plotdat$rval), 
  #      labels = names(cultures), srt = 90)
  axis(side = 1, at = cumsum(cultures) - 0.5*cultures, labels = names(cultures), las = 2, tick = FALSE, cex = 0.75)
  
  # titles and legend
  if(length(setdiff(c("n"),unique(plotdat$wllt)))==0) plot_desc <- paste0(plot_desc, " Control Wells")
  title(main = plot_desc)
  legend(x = "topright", legend = c("control wllq==1","control wllq=0","apid control median"), col = c(rgb(0,0,1,alpha=0.5),rgb(1,0,0,alpha=0.5),rgb(0.1,0.1,0.1,alpha=0.75)),
         cex = 0.65, pch = 19, bg = "transparent")
}
plotdat <- alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean_DIV12" & wllt == "n"]
plotdat[, rval := rval*60]
plot_by_apid(plotdat, threshold = 50)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_firing_rate_mean" & wllt == "n"])

plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number_DIV12" & wllt == "n"], threshold = 10)
plot_by_apid(alldat[acsn == "CCTE_Shafer_MEA_dev_active_electrodes_number" & wllt == "n"])

