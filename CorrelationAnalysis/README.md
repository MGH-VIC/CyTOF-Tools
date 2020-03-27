# Correlation Analysis to Clinical Measures
Module for conducting correlation analysis to clinical measures followed by clustering into modules and dimension reduction.
To run pipeline:
1) Run CorrCalculation
2) Optional: Run OptimalClustering (May not be well-suited for your application...also can run clustering in dimension reduction module)
3) Run script to export correlation matrix heatmap
4) In python, run 


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


