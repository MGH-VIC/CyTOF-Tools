#Script for reading data for machine learning classification and regression models
#Joshua Hess


#Check for missing packages and install if needed
list.of.packages <- c("dplyr", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(dplyr)
require(openxlsx)


ReadData = function(filename,sheetname,group_col,groups,cell_cols,not_cell_cols,scale_clusters=TRUE,cluster_prefix=NULL){
  #This function will do the calculation for your correlation analysis. This analysis is made for data that
  #has cluster data attached to columns that correspond to the measures that you want to correlate to. The column
  #names in your excel file must be written as "Cluster #" for each cluster. See example excel file if you are having
  #format issues.
  
  #filename: Indicates the path to your excel file (Ex: filename = "user/path/../myexcelfile.xlsx"). You can input a list of paths as well in order 
  #to concatenate a list of files for correlation analysis (Ex: filename = list()). If you choose to do this, you must also include a sheet list 
  
  #export: Logical indicating whether or not you want to export excel files for the correlation data (includes p-value and R value)
  
  #cluster_prefix: List indicating what prefixes you want to add to each of your cluster types in the dataframe. You only need to input this if you are
  #including multiple filenames (Ex: cluster_prefix = list("T","B","I"))
  
  #Get the input arguments to save for later if needed
  args = c(as.list(environment()))
  
  #Read the excel file list if it is a list:
  if (class(filename) == "list"){
    print("Detected a list of data files")
    data = list()
    clusters = list()
    not_clusters = list()
    grp_col = list()
    for (i in 1:length(filename)){
      #Read the data file i
      data[[i]] = na.omit(read.xlsx(filename[[i]],sheet=sheetname[[i]]))
      #Extract only the groups you want to include in the analysis
      data[[i]] = data[[i]][data[[i]][,group_col] %in% groups,]
      #Get the group column
      grp_col[[i]] = data[[i]][,group_col]
      #Separate clusters and other data
      if (scale_clusters){
        #Scale the data if chosen
        clusters[[i]] = scale(data[[i]][,cell_cols],center=TRUE,scale=TRUE)
      }else{
        clusters[[i]] = data[[i]][,cell_cols]
      }
      colnames(clusters[[i]]) = paste(cluster_prefix[[i]],colnames(clusters[[i]]),sep = " ")
      not_clusters[[i]] = data[[i]][,not_cell_cols]
    }
    #Combine all the data sets into a single data frame
    clusters = do.call(cbind,clusters)
    not_clusters = do.call(cbind,not_clusters)
    #Get only the first group column in the list (They should be the same for all excel sheets used!!)
    grp_col = grp_col[[1]]
    #Remove duplicate columns
    not_clusters = not_clusters[,!duplicated(colnames(not_clusters),fromLast = TRUE)]
  } else {
    #Read the single excel file
    print("Detected a single data file")
    #Read the data
    data = na.omit(read.xlsx(filename,sheet=sheetname))
    #Choose only the groups you want to include in the analysis
    data = data[data[,group_col] %in% groups,]
    #Get the group column
    grp_col = data[,group_col]
    #Create separate data frames for clusters and other data
    if (scale_clusters){
      clusters = scale(data[,cell_cols],center=TRUE,scale=TRUE)
    }
    cluster_names = colnames(clusters)
    not_clusters = data[,not_cell_cols]
    
    #If the cluster prefix is not NULL, paste it in here
    if(!is.null(cluster_prefix)){
      colnames(clusters) = paste(cluster_prefix[[i]],colnames(clusters[[i]]),sep = " ")
    }
    
  }
  #Return the data
  return_dat = list(clusters,not_clusters,grp_col,filename,sheetname,args)
  names(return_dat) = c("Clusters","Other","Group","Filename","Sheetname","Args")
  return(return_dat)
}



#

