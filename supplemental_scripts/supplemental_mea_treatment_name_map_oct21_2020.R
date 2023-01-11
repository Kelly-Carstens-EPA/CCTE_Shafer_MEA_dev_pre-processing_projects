# taken from all of the original renaming in the run_me files
# using this text to create a csv file with name maps
text1 <- "'1Ethyl3methylimidazolium diethylphosphate', treatment := '1-Ethyl-3-methylimidazolium diethylphosphate']
'2Ethylhexyl 2345 tetrabromobenzoate', treatment := '2-Ethylhexyl-2,3,4,5-tetrabromobenzoate']
'2Ethylhexyl diphenyl phosphate', treatment := '2-Ethylhexyl diphenyl phosphate']
'4HCyclopenta def phenanthrene', treatment := '4-H-Cyclopenta(d,e,f)phenanthrene']
'6 Hydroxydopamine hydrochloride', treatment := '6-Hydroxydopamine hydrochloride']
'Auramine', treatment := 'Auramine O']
'Benzo a pyrene', treatment := 'Benzo(a)pyrene']
'Benzo e pyrene', treatment := 'Benzo(e)pyrene']
'Benzo ghi perylene', treatment := 'Benzo[g,h,i]perylene']
'Benzo k fluoranthene', treatment := 'Benzo(k)fluoranthene']
'Bis 2 ethylhexyl tetrabromophthalate', treatment := 'Bis(2-ethylhexyl) tetrabromophthalate']
'Carbamic acid', treatment := 'Dibenz[a,c]anthracene']
'Carbamic acid butyl 3iodo2propynyl ester', treatment := 'Carbamic acid, butyl-, 3-iodo-2-propynyl ester']
'D Glucitol', treatment := 'D-Glucitol']
'Di 2ethylhexyl phthalate', treatment := 'Di(2-ethylhexyl) phthalate']
'Dibenz ac anthracene', treatment := 'Dibenz[a,c]anthracene']
'Dibenz ah anthracene', treatment := 'Dibenz(a,h)anthracene']
'Firemaster', treatment := 'Firemaster 550']
'L Ascorbic acid', treatment := 'L-Ascorbic acid']
'Manganese tricarbonyl 12345 eta 1 methyl 24 cyclopentadien 1 yl', treatment := 'Manganese, tricarbonyl[(1,2,3,4,5-.eta.)-1-methyl-2,4-cyclopentadien-1-yl]-']
'Tris 2chloroisopropyl phosphate', treatment := 'Tris(2-chloroethyl) phosphate']
'tert Butylphenyl diphenyl phosphate', treatment := 'tert-Butylphenyl diphenyl phosphate']
'Phenol isopropylated phosphate', treatment := 'Phenol, isopropylated, phosphate (3:1)'] # this is the only phosphate in the spidmap that would make sense
'1-2-PropyleneGlycol', treatment := '1,2-Propylene glycol']
'Amoxacillin', treatment := 'Amoxicillin']
'DEHP', treatment := 'Bis (2-ethylhexyl) phthalate (DEHP)'] # DEHP in 20140827, longer name in 20141112
'Bis (2-ethylhexyl) phthalate-DEHP', treatment := 'Bis (2-ethylhexyl) phthalate (DEHP)'] # DEHP in 20140827, longer name in 20141112
'Bis1', treatment := 'Bisindolylmaleimide I (Bis 1)']
'Cyclophosphamid', treatment := 'Cyclophosphamide']
'Cytosine arabinoside', treatment := 'Cytosine Arabinoside']
'Lead acetate', treatment := 'Lead Acetate']
'PBDE-47', treatment := '2,2'4,4'-Tetrabromodiphenyl ether (BDE-47)'] # pubchem lists PBDE 47 as a synonym
'Sodium Arsenate', treatment := 'Sodium Arsenite'] # see notes
'Tetrabromobisphenol A', treatment := 'Tetrabromobisphenol A (TBBPA)']
'TBBPA, treatment := 'Tetrabromobisphenol A (TBBPA)']
'Tris(2-chloroethyl) phosphate (TCEP)', treatment := 'Tris (2-chloroethyl) phosphate (TCEP)']
'TCEP, treatment := 'Tris (2-chloroethyl) phosphate (TCEP)']
'Tris(1,3-dichloro-2-propyl) phosphate (TDCPP)', treatment := 'Tris (1,3-dichloro-2-propyl) phosphate (TDCIPP)']
'TDCPP, treatment := 'Tris (1,3-dichloro-2-propyl) phosphate (TDCIPP)']
'Triphenyl phosphate (TPP)', treatment := 'Triphenyl phosphate (TPHP)']
'TPP, treatment := 'Triphenyl phosphate (TPHP)']
'Valproate', treatment := 'Valproic acid'] # after chat with Tim - the compounds named 'Valproate' and 'Valproic acid' are both refering to 'Sodium valproate' with cas number 1069-66-5
'TBHQ', treatment := 'tert-Butylhydroquinone'] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
'6-propyl-2-thiouracil', treatment := '6-Propyl-2-thiouracil']
'6 Propyl 2 thiouracil', treatment := '6-Propyl-2-thiouracil']
'Methadone', treatment := 'Methadone hydrochloride'] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
'IPBC', treatment := '3-Iodo-2-propynyl-N-butylcarbamate'] # these names are listed in the same chemical row in the Shafer 2019 paper Table 1 as the PREFERRED_NAME and Common name
'3 Iodo 2 propynyl N butylcarbamate', treatment := '3-Iodo-2-propynyl-N-butylcarbamate'] # these names are listed in the same chemical row in the Shafer 2019 paper Table 1 as the PREFERRED_NAME and Common name
'Pravastatin', treatment := 'Pravastatin sodium'] # Pravastatin was not used in this dataset, only Pravastain sodium
'Pravastin sodium', treatment := 'Pravastatin sodium'] # Pravastatin was not used in this dataset, only Pravastain sodium
'HPTE', treatment := '2,2-Bis(4-hydroxyphenyl)-1,1,1-trichloroethane'] # these names are listed in the same chemical row in the Shafer 2019 paper as teh PREFERRED_NAME and Common name
'Clothiandin', treatment := 'Clothianidin']
'Diphenhydramine', treatment := 'Diphenhydramine hydrochloride']# these names are listed in the same chemical row in the Shafer 2019 paper as the PREFERRED_NAME and Common name
'17beta Estradiol', treatment := '17beta-Estradiol']
'MGK 264', treatment := 'MGK-264']
'Tributyltin methylacrylate', treatment := 'Tributyltin methacrylate'] # 'Tributyltin methylacrylate' does not seem to be a chemical at all
'Rotenone - ToxCast G-8', treatment := 'Rotenone'] # this compound was tested in both the NTP and Toxcast groups.
'Disulfiram - ToxCast G-8', treatment := 'Disulfiram'] # this compound was tested in both the NTP and Toxcast groups. 
'Valinomycin - NTP', treatment := 'Valinomycin']
'1,1,2,2-Tetrahydroperfluoro-1-decanol - TP0001411', treatment := 'Clotrimazole'] # See lab noteook 20171011. TP0001411 + ToxCast Plate Location E03 corresponds to Clotrimazole
'1H,1H,2H,2H-Perfluorooctyl iodide - TP0001413', treatment := '1H,1H,2H,2H-Perfluorooctyl iodide']
'Perfluoroundecanoic acid - TP0001411', treatment := 'Perfluoroundecanoic acid']
'Bisindolymaleimide 1', treatment := 'Bisindolylmaleimide I'] # Shafer_42 list CASRN is 133052-90-1, which matches the casn shown in paper
'Domoic Acid', treatment := 'L-Domoic acid'] # Shafer_42 CASRN is 14277-97-5, which matches the casn shown in the paper
'Sodium Orthovanadate', treatment := 'Sodium orthovanadate'] # just change in captilization
'Diazonon', treatment := 'Diazinon'] # when I google search for 'Diazonon', it redirects to 'Diazinon'
'Malaxon', treatment := 'Malaoxon'] # 'Malaxon' does not have any resutls in Google, I think this just a typo
'Z-tetrachlorvinphos', treatment := 'Z-Tetrachlorvinphos'] # capitalization
"
# these have ' in treatment name, will have to add separately
# '2244 Tetrabromodiphenyl ether', treatment := '2,2',4,4'-Tetrabromodiphenyl ether']
# '22445 Pentabromodiphenyl ether', treatment := '2,2',4,4',5-Pentabromodiphenyl ether']
# '224455 Hexabromodiphenyl ether', treatment := '2,2',4,4',5,5'-Hexabromodiphenyl ether']
# 'op-DDT', treatment := 'o,p'-DDT']
# 'pp-DDD', treatment := 'p,p'-DDD']
# 'pp-DDE', treatment := 'p,p'-DDE']
# 'pp-DDT', treatment := 'p,p'-DDT']

# extract the mea treatment name, corresponding name in spidmap, and notes
text2 <- strsplit(text1, split = "/n")
notes <- unlist(lapply(text2, function(x) ifelse(grepl("#",x), sub(".*# ","",x), "")))
mea_treatment_name <- unlist(lapply(text2, function(x) sub(", treatment :=.*$","",x)))
mea_treatment_name <- sub("'","",mea_treatment_name)
mea_treatment_name <- sub("'","",mea_treatment_name)
spid_treatment_name <- unlist(lapply(text2, function(x) sub("^.*, treatment := ","",x)))
spid_treatment_name <- sub("'].*$","",spid_treatment_name)
spid_treatment_name <- sub("'","",spid_treatment_name)

# add the treatment singled out above
mea_treatment_name <- c(mea_treatment_name, c("2244 Tetrabromodiphenyl ether","22445 Pentabromodiphenyl ether","224455 Hexabromodiphenyl ether",
                                              "op-DDT","pp-DDD","pp-DDE","pp-DDT"))
spid_treatment_name <- c(spid_treatment_name, c("2,2',4,4'-Tetrabromodiphenyl ether","2,2',4,4',5-Pentabromodiphenyl ether","2,2',4,4',5,5'-Hexabromodiphenyl ether",
                                                "o,p'-DDT","p,p'-DDD","p,p'-DDE","p,p'-DDT"))
notes <- c(notes, rep("", 7))
treatment_name_table <- data.table(mea_treatment_name, spid_treatment_name, notes)

# save the results
# don't overwrite the dataset assignments I made in the csv file!!
# write.csv(treatment_name_table, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/supplemental_mea_treatment_name_map.csv", row.names = F)



