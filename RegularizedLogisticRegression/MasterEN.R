#Functions for Running Elastic Net
#Joshua Hess
require(caret)
require(doParallel)
require(MLmetrics)
require(varhandle)
require(openxlsx)
#Import custom functions
source("ReadData.R")
source("EN.R")

#Create function for running elastic net for each group in a dataset
MasterEN = function(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters=TRUE,cluster_prefix=NULL,
                    cv_type,export_coeff,export_coeff_img,export_cv,export_cv_img,num_cores,iterations,top_n){
  #Function for running elastic net over all combinations of groups in an excel file
  #Inputs are the same as RunEN function and ReadData.R function
  
  #Get all combinations of groups (Pairwise so add sequence length of 2)
  grp_comb = as.data.frame(lapply(seq_len(2),FUN = function(x)combn(groups, x))[2])
  
  #Create a list to store results in
  results_list = list()
  #Read data and run elastic net for each combination
  for (i in 1:ncol(grp_comb)){
    #Read data
    data = ReadData(filename,sheetname,group_col,grp_comb[[i]],cell_cols,not_cell_cols,scale_clusters,cluster_prefix)
    #Print an update
    print(paste('Running Elastic Net for groups:',paste(grp_comb[[i]],collapse="-"),sep = " "))
    #Run elastic net
    tmp_EN = RunEN(data,cv_type,export_coeff,export_coeff_img,export_cv,export_cv_img,num_cores,iterations,top_n)
    #Print an update
    print(paste('Finished Elastic Net for groups:',paste(grp_comb[[i]],collapse="-"),sep = " "))
    #Add this model to the return list
    results_list[[i]] = tmp_EN
  }
  #Return the list of results
  return(results_list)
}