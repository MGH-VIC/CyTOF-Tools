# Regularized logistic regression workflow
Module for running elastic net and/or lasso regression across multiple pariwise
comparisons using multicore implementation in R. The module is designed to provide
insight to which clusters are predictive of group assignment based on cluster frequencies.
To run pipeline:
1) Data must be clustered
2) Data must be in the format given by Template.xlsx
3) Run MasterEN function

Template for pipeline can be found in ExampleMasterEN.R

**MasterEN**:
Function for running all combinations/pairwise comparisons between experimental groups for
elastic net/lasso logistic regression.
* filename: Enter the name of your data file (If multiple files, use: list("File1name.xlsx","File2name.xlsx")) (Ex: filename = "Template.xlsx")
* sheetname: Enter the name of the sheet of the data sheet you are using for calculations (If multiple sheets, use: list("Sheet1name.xlsx","Sheet2name.xlsx")) (Ex: sheetname = "Sheet1")
* scale_clusters: Scale your data? (z-score) (Ex: scale_clusters = TRUE) --leave TRUE for equal representation of all variables
* cluster_prefix: Do you want to add a prefix to cluster names? If not, leave as NULL (If multiple sheets, use: c("B","I")) (Ex:cluster_prefix = NULL)
* cell_cols: Which columns are you using as cells for calculation? (Variables in your excel sheet) (Ex: cell_cols = 20:31)
*not_cell_cols: Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells. (Variables in your excel sheet) (Ex: not_cell_cols = 2:5)
* group_col: Which column indicates the groups you are using? (Variables in your excel sheet) (Ex: group_col = 3)
* groups: Which groups are you wanting to use? (Ex. groups = c(1,2,3) if numbered Ex. groups = c("Vaccinated","Naive","Vax Sick") if strings) (Ex: groups = c(1,2,3))
* cv_type: Which type of cross-validation (Options are "repeated" or "bootstrap") (Ex: cv_type = "repeated")

* export_coeff: Export an excile file containing elastic net waterfall plot data (the coefficients)? (Ex: export_coeff = TRUE)
* export_coeff_img: Export an image containing elastic net waterfall plot data (the coefficients)? (Ex: export_coeff_img=TRUE)
* export_cv: Export an excel file containing elastic net cross-validation data? (Ex: export_cv = TRUE)
* export_cv_img: Export an image containing elastic net cross-validation data? (Ex: export_cv_img = TRUE)
* num_cores: How many cores to use for the calculations? (Ex: num_cores = 8)
* top_n: Select the top n coefficient models based on magnitude (Ex: top_n = 30) --this option should be used if the elastic net outputs too many clusters to visually inspect on waterfall plot