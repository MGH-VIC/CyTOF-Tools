#Import modules (Do not change)
source("MasterEN.R")

#Enter the name of your data file (If multiple files, use: list("File1name.xlsx","File2name.xlsx"))
filename = "Template.xlsx"
#Enter the name of the sheet of the data sheet you are using for calculations (If multiple sheets, use: list("Sheet1name.xlsx","Sheet2name.xlsx"))
sheetname = "Sheet1"
#Scale your data? (z-score)
scale_clusters = TRUE
#Do you want to add a prefix to cluster names? If not, leave as NULL (If multiple sheets, use: c("B","I"))
cluster_prefix = NULL
#Which columns are you using as cells for calculation? (Variables in your excel sheet)
cell_cols = 20:31
#Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells. (Variables in your excel sheet)
not_cell_cols = 2:5
#Which column indicates the groups you are using? (Variables in your excel sheet)
group_col = 3
#Which groups are you wanting to use? (Ex. groups = c(1,2,3) if numbered Ex. groups = c("Vaccinated","Naive","Vax Sick") if strings)
groups = c(1,2,3)
#Which type of cross-validation (Options are "repeated" or "bootstrap")
cv_type = "repeated"
#Export an excile file containing elastic net waterfall plot data (the coefficients)?
export_coeff = TRUE
#Export an image containing elastic net waterfall plot data (the coefficients)?
export_coeff_img=TRUE
#Export an excel file containing elastic net cross-validation data?
export_cv = TRUE
#Export an image containing elastic net cross-validation data?
export_cv_img = TRUE
#How many cores to use for the calculations? (Ex: 10)
num_cores = 8
#Set the top n coefficient models
top_n = 30

#Run function for all pairwise elastic nets
results = MasterEN(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters,cluster_prefix,
                  cv_type,export_coeff,export_coeff_img,export_cv,export_cv_img,num_cores,top_n)
