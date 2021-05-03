#Import modules (Do not change)
source("MasterLDA.R")

#Enter the name of your data file (If multiple files, use: list("File1name.xlsx","File2name.xlsx"))
filename = "ALS-Live-cells-k40-elbow-point_SPADEVizR-relative-abundance-remove.xlsx"
#Enter the name of the sheet of the data sheet you are using for calculations (If multiple sheets, use: list("Sheet1name.xlsx","Sheet2name.xlsx"))
sheetname = "Sheet1"
#Scale your data? (z-score)
scale_clusters = TRUE
#Do you want to add a prefix to cluster names? If not, leave as NULL (If multiple sheets, use: c("B","I"))
cluster_prefix = NULL
#Which columns are you using as cells for calculation? (Variables in your excel sheet)
cell_cols = 3:54
#Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells. (Variables in your excel sheet)
not_cell_cols = NULL
#Which column indicates the groups you are using? (Variables in your excel sheet)
group_col = 2
#Which groups are you wanting to use? (Ex. groups = c(1,2,3) if numbered Ex. groups = c("Vaccinated","Naive","Vax Sick") if strings)
groups = c("FRS-29-39","Healthy")
#Which type of cross-validation (Options are "repeated" or "bootstrap")
cv_type = "repeated"
#Export an excel file containing performance metrics per cluster in iterative LDA?
export_results_iterative = TRUE
#Keep this many clusters (based on top classification accuracy) after single cluster LDAs
top_n = 3
#Export an image containing performance metrics for combinations of clusters?
export_results_permutative = TRUE
#Maxmimum number of clusters per combination to test?
max_size = 2
#What are the new group assignments for LDA predictions and projections (include those you uused in "groups" argument plus any more for projection of continuous change)
new_groups = c("FRS-29-39","FRS-over-40","Healthy")


#Run the LDA pipeline
results = MasterLDA(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters,cluster_prefix,
                     cv_type,export_results_iterative,num_cores,
                     top_n,export_results_permutative,
                     max_size,new_groups)
