#Functions for Running LDA
#Joshua Hess
require(caret)
require(doParallel)
require(MLmetrics)
require(varhandle)
require(openxlsx)
#Import custom functions
source("ReadData.R")
source("LDA.R")


#Function for complete LDA analysis pipeline
MasterLDA = function(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters=TRUE,cluster_prefix=NULL,
                     cv_type,export_results_iterative,num_cores,
                     top_n,export_results_permutative,
                     max_size,
                     new_groups = NULL,
                     iterations=1000){
  #Function for running the full LDA projections pipeline
  #Inputs are specified for each function line by line
  
  #Read Data
  data = ReadData(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters,cluster_prefix)
  #iterative LDA run
  iterativeLDA = RunIterativeLDA(data,cv_type,export_results_iterative,num_cores,iterations)
  #filtering function
  filt_data = FilterIterativeLDA(data,iterativeLDA,top_n)
  #Permutative LDA function
  permResult = PermutativeLDA(filt_data,cv_type,max_size,export_results_permutative,num_cores,iterations)
  #projection function for Permutative LDA
  proj_stats = ProjectionsPermutativeLDA(permResult, og_dat = data, new_groups = new_groups)
  #Return each object
  return_list = list(data,iterativeLDA,filt_data,permResult,proj_stats)
  #Change the names in the return list
  names(return_list) = c("Data","IterativeLDA","FilteredData","PermutativeLDA","ProjectionStats")
  #Return the list
  return(return_list)
}



#