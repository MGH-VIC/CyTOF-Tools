# Linear Discriminant Analysis workflow

Module for running linear discriminant
To run pipeline:
1) Data must be clustered
2) Data must be in the format given by Template.xlsx
3) Run MasterLDA function

Template for pipeline can be found in ExampleMasterLDA.R

**MasterLDA**:
Function for running through all combinations of clusters (given a max size to use) for predicting
class assignment based on cluster frequencies. This analysis should use two groups to train the LDA model on, and
a 3rd class can be predicted to give a continuous response/assignment. This will show, for example, if some samples have
an intermediate outcome/response compared to two extremes.
* filename: Enter the name of your data file (If multiple files, use: list("File1name.xlsx","File2name.xlsx")) (Ex: filename = "Template.xlsx")
* sheetname: Enter the name of the sheet of the data sheet you are using for calculations (If multiple sheets, use: list("Sheet1name.xlsx","Sheet2name.xlsx")) (Ex: sheetname = "Sheet1")
* scale_clusters: Scale your data? (z-score) (Ex: scale_clusters = TRUE) --leave TRUE for equal representation of all variables
* cluster_prefix: Do you want to add a prefix to cluster names? If not, leave as NULL (If multiple sheets, use: c("B","I")) (Ex:cluster_prefix = NULL)
* cell_cols: Which columns are you using as cells for calculation? (Variables in your excel sheet) (Ex: cell_cols = 20:31)
*not_cell_cols: Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells. (Variables in your excel sheet) (Ex: not_cell_cols = 2:5)
* group_col: Which column indicates the groups you are using? (Variables in your excel sheet) (Ex: group_col = 3)
* groups: Which groups are you wanting to use? (Ex. groups = c(1,2,3) if numbered Ex. groups = c("Vaccinated","Naive","Vax Sick") if strings) (Ex: groups = c(1,2,3))
* cv_type: Which type of cross-validation (Options are "repeated" or "bootstrap") (Ex: cv_type = "repeated")
* export_results_iterative: Export an excile file containing performance metrics per cluster in iterative LDA? (Ex: export_results_iterative = TRUE)
* top_n: Keep this many clusters (based on top classification accuracy) after single cluster LDAs (Ex: top_n = 3)
* export_results_permutative: Export an image containing performance metrics for combinations of clusters? (Ex: export_results_permutative = TRUE)
* max_size: Maxmimum number of clusters per combination to test? (Ex: max_size = 2)
* new_groups: What are the new group assignments for LDA predictions and projections (include those you uused in "groups" argument plus any more for projection of continuous change) (Ex: new_groups = c(1,2,3))
