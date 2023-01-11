# 09/04/2020
# openxlsx function verification
file_path <- "L:/Lab/NHEERL_MEA/PIP3 - Project/Data/Organophosphates/20160217 Culture/MW1105-20/ON_20160217_MW1105-20_Summary.xlsx"

xlsx::getSheets(file_path)

openxlsx::getSheetNames(file_path)

dat1 <- openxlsx::read.xlsx(file_path, sheet = "Alamar Blue", skipEmptyRows = FALSE, colNames = F)
dat1
dat2 <- xlsx::read.xlsx(file_path, sheetName = "Alamar Blue")
dat2
base::all.equal(dat1, dat2, check.attributes = F)
dat1$X13
dat2$NA..13
names(dat1)
names(dat2)

head(dat1)
head(dat2)
nrow(dat1)
nrow(dat2)
# Okay, so even with skipEmptyRows = FALSE, that only applies to empty rows after the first row containing data in teh file
# I don't hink this will matter though

# comparing final outputs:
# (from calc files)
xlsx_dat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/output/Example2020_cytotox_xlsx_package.csv"))
open_dat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/output/Example2020_cytotox.csv"))
all.equal(xlsx_dat, open_dat)
nrow(xlsx_dat) # 576
nrow(open_dat) # 864

xlsx_dat[, .N, by = "srcf"]
# srcf   N
# 1:   20180801_ON PFAS G1_1 Calculations.xlsx 288
# 2: 20180815_ON PFAS G2-1_1 Calculations.xlsx 288
open_dat[, .N, by = "srcf"]
# srcf   N
# 1:   20180801_ON PFAS G1_1 Calculations.xlsx 288
# 2: 20180815_ON PFAS G2-1_1 Calculations.xlsx 288
# 3: 20180815_ON PFAS G2-1_2 Calculations.xlsx 288

all.equal(xlsx_dat, open_dat[srcf != "20180815_ON PFAS G2-1_2 Calculations.xlsx"])
# TRUE!!!!
all.equal(open_dat[srcf != "20180815_ON PFAS G2-1_2 Calculations.xlsx"], xlsx_dat)
# also TRUE

# let's try it one mroe time with a Summary file in the mix
xlsx_dat2 <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/output/Example2020_cytotox_xlsx_package2.csv"))
open_dat <- as.data.table(read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_nfa_for_tcpl/Example2020/output/Example2020_cytotox_openxlsx_package.csv"))
all.equal(xlsx_dat2, open_dat)
# TRUE
all.equal(open_dat, xlsx_dat2)
# TRUEUUUUUUUUUUUUUUUUUUUUUUUU!