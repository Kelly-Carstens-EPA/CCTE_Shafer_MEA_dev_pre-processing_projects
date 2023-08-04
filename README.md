# Repo for CCTE_Shafer_MEA_NFA pre-processing scripts and output

## Notes on the folders

* Project-specific folders (e.g., `Brown2014`) contain all files specific to each project
    * `run_me_[project_name].R` outlines the steps used to pre-process the data. For projects before Summer 2023, run_me scripts referenced functions that are now under the folder `deprecated_nfa-spike-list-to-mc0-r-scripts`. For projects after Summer 2023, run_me scripts will reference functions in the separate MEA_dev scripts repo.
    * `output` subfolders - contain the intermediate and final output from the 
        * files ending in "_longfile.Rdata" or "_NFA_for_tcpl_lvl0.RData" are the final output of the run_me scripts
* `deprecated_nfa-spike-list-to-mc0-r-scripts/R` - deprecated versions of generic functions used to calculate parameters and wrangle the data. Starting Summer 2023, the run_me scripts can use the functions located under `L:\Lab\NHEERL_MEA\CCTE_Shafer pre-process for TCPL\MEA_dev\CCTE_Shafer_MEA_dev_pre-processing_scripts\R` instead
    
## Content that is .gitingored due to file size:

* All h5 files
* */All_MI/*
* */prepared_data/*
* */source_data/*

## How to obtain past versions of the generic pre-processing scripts corresponding to a particular project

Note that the generic functions under `nfa-spike-list-to-mc0-r-scripts/` have been updated over time. In order to obtain the version of these functions used to pre-process the data for a particular past project, you can:

* Determine the date range in which the project was pre-processed
* Find the commit id SHA associated with a commit from the desired date range, e.g., open Git Bash in this repo and run `git log --since='July 1 2023' --until 'Aug 1 2023'
* View the repository as it appeared at that commit, e.g.,
    * Go to https://github.com/Kelly-Carstens-EPA/CCTE_Shafer_MEA_dev_pre-processing_projects/insert-commit-id-SHA, OR
    * Create a new branch that checks out the repository as it appeared with that commit, e.g., `git checkout -b test-branch first-6-digits-of-commit-SHA`

** Note that before the commit on Jan 11, 2023, this repo only contained the items under `nfa-spike-list-to-mc0-r-scripts`. This repo was expanded to include the project folders on Jan 11, 2023.