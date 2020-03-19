# Batch Correlation Matching
Module for matching clusters across experimental runs using multiple correlations.
To run pipeline:
1) Run ImportDataMaster
2) Run ParallelCoordinatePlots
3) Run sScatterPlot
4) Run MinMaxRescale
5) Run FilterSize
6) Run CorrelationMatching
7) Run FilterCorrResults
8) Run GenerateMatchingPPT
9) Manual matching through powerpoint visualization

Template for pipeline can be found in CorrelationMatchingExample.R
* Currently, importing functions into master script is suboptimal (To do)
* Currently, Hungarian algorithm is implemented for multi-experiment matching but needs work implementing transitivity bias

**ImportDataMaster**:
Function for importing excel file from clustering in Vortex
* excel: path to excel file (Ex: '../myexcel.xlsx')
* scatterplot_import: export table to fill out for group assignment and scatterplot generation? (Ex:True)
* save_new: save a new file with summary? (Ex: False) -- typically do not need
* save_as: if save_new is true, what file name to save as? (Ex: 'new_excel.xlsx')
* remove: remove samples from analysis? (Ex: c('sample1', 'sample2'))
* pheno_cols: columns to import for analysis (Ex: c(1:12,14:20)) -- always start from 1
* experiment_name: name of this experiment (Ex: 'Group1') -- must include
* remove_marker_string: string to remove from fcs column names -- leave default setting for now

**ParallelCoordinatePlots**:
Function for creating parallel coordinate plots
--must have run ImportDataMaster first
* out_dir: creates folder with the specified name and outputs images here (Ex: 'Group1PCP') -- must include

**ScatterPlot**:
Function for creating scatter plots -- must have run ImportDataMaster first with scatterplot_import argument set to true
--must have run ImportDataMaster first
--must have run ImportDataMaster first with scatterplot_import argument set to true
--must have filled out the sheet that is exported after setting scatterplot_import to true
* out_dir: creates folder with the specified name and outputs images here (Ex: 'Group1Scatter') -- must include

**MinMaxRescale**:
Function for rescaling each column of data prior to correlation calculations
--must have run ImportDataMaster first
* Use ImportDataMaster object as input

**FilterSize**:
Function for filtering cell counts using 0.02% cutoff
--must have run ImportDataMaster first
* Use ImportDataMaster object as input

**CorrelationMatching**:
Function for correlation cluster matching across two expirements
--must have run ImportDataMaster first for two experiments
--must have run ParallelCoordinatePlots first first for two experiments
--must have run ImportDataMaster first first for two experiments
--must have run ScatterPlot first first for two experiments
* method: method used for correlation (Ex: "spearman")
* include_count: use count in correlation calculations (Ex: FALSE) -- use false
* csv_name: name of file that will be exported with results (Ex: 'my_correlations.csv')

**FilterCorrResults**:
Function for filtering correlation results with top n matches
--must have run CorrelationMatching
* top_n: integer indicating number of top matches to keep (Ex: 5) -- 5 is typically good
* csv_name: name of file that will be exported with results (Ex: 'my_filtered_correlations.csv')

**GenerateMatchingPPT**:
Function for generating powerpoint that summarizes correlations for manaul assessment
* ppt_name: name of file that will be exported with results (Ex: 'my_correlation_ppt.pptx')
