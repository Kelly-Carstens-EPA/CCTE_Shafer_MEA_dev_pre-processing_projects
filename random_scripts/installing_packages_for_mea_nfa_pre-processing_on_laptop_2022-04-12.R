# ------------------------------------------------------------------------ #
# Installing packages needed to run MEA NFA Scripts
# With R 4.0, needed to install an additional R package to download packages from github
# needed special permission to do that.
# But that does not seem to be the case anymore!!
# Apr 12, 2022
# ------------------------------------------------------------------------ #

# Can I replace reshape package?

library(gtools)
library(devtools)
install.packages("pracma")
library(compiler)
if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("rhdf5")

devtools::install_github("sje30/sjemea")
# Downloading GitHub repo sje30/sjemea@HEAD
# √  checking for file 'C:\Users\ACARPE01\AppData\Local\Temp\RtmpQn8Zcq\remotes36481fd226a0\sje30-sjemea-4857188/DESCRIPTION' (459ms)
# -  preparing 'sjemea': (1.2s)
# √  checking DESCRIPTION meta-information ...
# -  cleaning src
# -  checking for LF line-endings in source and make files and shell scripts
# -  checking for empty or unneeded directories
# -  looking to see if a 'data/datalist' file should be added
# -  building 'sjemea_0.43.tar.gz'
# 
# * installing *source* package 'sjemea' ...
# ** using staged installation
# ** libs
# 
# *** arch - i386
# "C:/ProgramData/rtools40/mingw32/bin/"gcc  -I"C:/Users/ACARPE01/ONEDRI~1/Profile/DOCUME~1/R/R-41~1.1/include" -DNDEBUG          -O2 -Wall  -std=gnu99 -mfpmath=sse -msse2 -mstackrealign  -c sjemea.c -o sjemea.o
# sjemea.c: In function 'bin2_overlap':
#   sjemea.c:300:23: warning: variable 'bin_numi' set but not used [-Wunused-but-set-variable]
# int bin_num, nbins, bin_numi;
# ^~~~~~~~
#   sjemea.c: In function 'ns_count_activity':
#   sjemea.c:383:19: warning: variable 'end' set but not used [-Wunused-but-set-variable]
# Sfloat *p, beg, end, wid;
# ^~~
#   sjemea.c: In function 'frate':
#   sjemea.c:437:19: warning: variable 'end' set but not used [-Wunused-but-set-variable]
# double *p, beg, end, wid, *count;
# ^~~
#   "C:/ProgramData/rtools40/mingw32/bin/"gcc  -I"C:/Users/ACARPE01/ONEDRI~1/Profile/DOCUME~1/R/R-41~1.1/include" -DNDEBUG          -O2 -Wall  -std=gnu99 -mfpmath=sse -msse2 -mstackrealign  -c tiling.c -o tiling.o
# # ...
# # ...
# tiling.c:148:16: warning: unused variable 'count' [-Wunused-variable]
# int a, b, n, count;
# ^~~~~
#   C:/ProgramData/rtools40/mingw64/bin/gcc -shared -s -static-libgcc -o sjemea.dll tmp.def sjemea.o tiling.o -LC:/Users/ACARPE01/ONEDRI~1/Profile/DOCUME~1/R/R-41~1.1/bin/x64 -lR
# installing to C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/R/R-4.1.1/library/00LOCK-sjemea/00new/sjemea/libs/x64
# ** R
# ** data
# ** inst
# ** byte-compile and prepare package for lazy loading
# ** help
# *** installing help indices
# converting help for package 'sjemea'
# finding HTML links ... done
# compute.ns                              html  
# feller.read.spikes                      html  
# h5.read.spikes                          html  
# hist.ab                                 html  
# isi.gamma                               html  
# isi.local.variation                     html  
# isi.spearman.rank.corr                  html  
# jay.read.spikes                         html  
# litke.read.spikes                       html  
# make.movieframes                        html  
# mcd.data.to.array                       html  
# mcd.read.spikes                         html  
# ncl.read.spikes                         html  
# nex.read.spikes                         html  
# sanger.read.spikes                      html  
# sjemea-package                          html  
# spikes.to.bursts                        html  
# sun.read.spikes                         html  
# tiling.allpairwise                      html  
# tiling.corr                             html  
# ** building package indices
# ** installing vignettes
# ** testing if installed package can be loaded from temporary location
# *** arch - i386
# *** arch - x64
# ** testing if installed package can be loaded from final location
# *** arch - i386
# *** arch - x64
# ** testing if installed package keeps a record of temporary installation path
# * DONE (sjemea)

devtools::install_github("dianaransomhall/meadq")
# Downloading GitHub repo dianaransomhall/meadq@HEAD
# √  checking for file 'C:\Users\ACARPE01\AppData\Local\Temp\RtmpQn8Zcq\remotes36484db933ba\dianaransomhall-meadq-9acfb2d/DESCRIPTION' (1.2s)
# -  preparing 'meadq': (3.7s)
# √  checking DESCRIPTION meta-information ...
# -  checking for LF line-endings in source and make files and shell scripts (1.2s)
# -  checking for empty or unneeded directories
# -  looking to see if a 'data/datalist' file should be added
# -  building 'meadq_1.0.7.tar.gz' (887ms)
# 
# * installing *source* package 'meadq' ...
# ** using staged installation
# ** R
# ** data
# ** inst
# ** byte-compile and prepare package for lazy loading
# Note: possible error in 'filter.spikes.dh(h5Files[i], ': unused argument (beg = beg) 
# ....
# *** arch - i386
# *** arch - x64
# ** testing if installed package keeps a record of temporary installation path
# * DONE (meadq)
# Warning message:
#   In doTryCatch(return(expr), name, parentenv, handler) :
#   restarting interrupted promise evaluation



# Do we really still need reshape package? --------------------------------

scripts.to.check <- list.files(path = 'nfa-spike-list-to-mc0-r-scripts/R', pattern = '', full.names = T)

check.phrase <- 'reshape'

library(stringi)
for (filei in scripts.to.check) {
  texti <- scan(filei, what = character())
  if(any(stri_detect(texti, fixed = check.phrase), na.rm = T)) {
    cat(check.phrase,'found in',filei)
  }
}
# No instances - I'm going to assume that we don't need it
