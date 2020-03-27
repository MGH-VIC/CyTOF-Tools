# Correlation Analysis to Clinical Measures
Module for conducting correlation analysis to clinical measures followed by clustering into modules and dimension reduction.
To run pipeline:
1) Open MasterCorrelationAnalysis.R
2) Run CorrCalculation
3) Optional: Run OptimalClustering (May not be well-suited for your application...also can run clustering in dimension reduction module)
4) Run script to export correlation matrix heatmap
4) In python, open DimensionReductionMaster.py and follow module...plug in your data (TODO)

* Currently, NbClust (optimal clustering number in step 3 is unstable)
* Currently, python modules are not the easiest they could possibly be for usage, but it should be self-explanatory to plug in your data for usage. Plotting functions are sensitive and it is difficult to generalize a plotting function without spending significant time.
* Note: Code could be much cleaner if this were a solely python implementation or R, but dimension reduction is easier in python (personal opinion) and correlation analyses were already coded in R.

**CorrCalculation**:
Function for importing excel file from clustering in Vortex
* filename: Enter the name of your data sheet (Ex: "path/to/Template.xlsx")
* sheetname: Enter the name of the sheet of the data sheet you are using for calculations (Ex:"Sheet1")
* corr_type: Enter correlation type (Options are "spearman" or "pearson")
* export: Do you want to export the p values and correlation coefficient data? (Ex:TRUE)
* r_export_name: If you are exporting, what do you want to call the correlation coefficient sheet? (Ex: "R value.xlsx")
* p_export_name: If you are exporting, what do you want to call the p-value sheet? (Ex: "P value.xlsx")
* cluster_prefix: Do you want to add a prefix to cluster names? If not, leave as NULL (Ex: if not NULL, c("B","I"))
* cell_cols: Which columns are you using as cells for calculation? (Ex: 20:31)
* not_cell_cols: Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells (Ex: NULL)
* group_col: Which column indicates the groups you are using? (Ex: 3)
* groups: Which groups are you wanting to use? (Ex. groups = c(1,2) if numbered Ex. groups = c("Vaccinated","Naive") if strings)

**DimensionReductionMaster.py**
Code for conducting dimension reduction after running correlations in R with a grid of plots for each clinical parameter. Script will have comments for what you need to insert.
* Note: You will need to specify the clinical parameters to keep (Make it identical in format to the 'Columns to Keep.txt' file). The parameters in this document will be extracted from the correlation matrix R and P values.
* Note: If you choose to cluster using this module instead of the NbClust option in R, you will first need to run UMAP for dimension reduction, after which you will run PCA on the affinity matrix produced by UMAP. This option is already the default in the example script. The resulting PCA plot is used to identify the knee point (This will be your final number of clusters). Then, run spectral clustering on the UMAP affinity matrix using a number of clusters equal to the knee point in the PCA plot.