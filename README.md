# Repo for CCTE_Shafer_MEA_NFA pre-processing scripts and output

## Content that is located on L:\Lab\NHEERL_MEA\Carpenter_Amy\pre-process_mea_nfa_for_tcpl, but is gitignored from the remote repo:

* All h5 files
* */All_MI/*
* */prepared_data/*
* */source_data/*

(these are just numerous and sometimes large data files, so I didn't want to push these to remote to clone. But they will be saved in this folder)

## Current repo organization

* `nfa-spike-list-to-mc0-r-scripts/`
    * `R/`
    	* scripts that begin with `run_me/` - the final version of the `run_me_[project_name].R[md]` scripts used to process the data for each project. 
	* several generic scripts for pre-processing the data. These functions are called by the `run_me_[project_name].R` files
* Project-specific folders (e.g., `Brown2014`) contain all files specific to each project EXCEPT the final `run_me_[project_name].R` script
    * `output` subfolders - contain the intermediate and final output from the `run_me_[project_name].R`
        * the longfile.Rdata files are the final output of the run_me scripts
* `lvl0_snapshots/` - "snapshots" of level 0 data prepared from the _longfile's for projects that were ready to be processed in TCPL
    

Note that the generic scripts under `nfa-spike-list-to-mc0-r-scripts/` may be updated over time. In order to obtain versions of the used to pre-process the data for a particular project, you can checkout how the repository appeared under a past commit with git.

For example, 

* Determine the date range in which the project was pre-processed
* Find the commit id SHA associated with a commit from the desired date range, e.g., `git log --since='July 1 2023' --until 'Aug 1 2023'
* Create a new branch that checks out the repository as it appeared with that commit, e.g., `git checkout -b test-branch first-6-digits-of-commit-SHA`

** Note that before the commit on Jan 11, 2023, this repo only contained the items under `nfa-spike-list-to-mc0-r-scripts`. This repo was expanded to include the project folders on Jan 11, 2023.