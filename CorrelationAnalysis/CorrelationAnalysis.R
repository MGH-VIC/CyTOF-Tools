#Script for conducting correlation analysis in R
#Joshua Hess


#Check for missing packages and install if needed
list.of.packages <- c("dplyr", "Hmisc","ppcor","openxlsx","flashClust","dendextend","gtable","gridExtra","grid","gdata",
                      "RColorBrewer","magrittr","factoextra","NbClust")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(dplyr)
require(Hmisc)
require(ppcor)
require(openxlsx)
require(flashClust)
require(dendextend)
require(gtable)
require(gridExtra)
require(grid)
require(gdata)
require(RColorBrewer)
require(magrittr)
require(factoextra)
require(NbClust)



CorrCalculation = function(filename,sheetname,group_col,cell_cols,not_cell_cols,corr_type="spearman",export=FALSE,
                           r_export_name=NULL,p_export_name=NULL,cluster_prefix=NULL){
  #This function will do the calculation for your correlation analysis. This analysis is made for data that
  #has cluster data attached to columns that correspond to the measures that you want to correlate to. The column
  #names in your excel file must be written as "Cluster #" for each cluster. See example excel file if you are having
  #format issues.
  
  
  #filename: Indicates the path to your excel file (Ex: filename = "user/path/../myexcelfile.xlsx"). You can input a list of paths as well in order 
  #to concatenate a list of files for correlation analysis (Ex: filename = list()). If you choose to do this, you must also include a sheet list 
  
  #groups: Indicates the groups you want to use for the calculation (Ex: groups = c("Naive","Challenged"))
  
  #type: Indicates the type of correlation to use (Ex: "spearman", "pearson")
  
  #export: Logical indicating whether or not you want to export excel files for the correlation data (includes p-value and R value)
  
  #cluster_prefix: List indicating what prefixes you want to add to each of your cluster types in the dataframe. You only need to input this if you are
  #including multiple filenames (Ex: cluster_prefix = list("T","B","I"))
  
  #Read the excel file list if it is a list:
  if (class(filename) == "list"){
    print("Detected a list of data files")
    data = list()
    clusters = list()
    not_clusters = list()
    for (i in 1:length(filename)){
      #Read the data file i
      data[[i]] = na.omit(read.xlsx(filename[[i]],sheet=sheetname[[i]]))
      #Extract only the groups you want to include in the analysis
      data[[i]] = data[[i]][data[[i]][,group_col] %in% groups,]
      #Separate clusters and other data
      clusters[[i]] = data[[i]][,cell_cols]
      colnames(clusters[[i]]) = paste(cluster_prefix[[i]],colnames(clusters[[i]]),sep = " ")
      not_clusters[[i]] = data[[i]][,not_cell_cols]
    }
    #Combine all the data sets into a single data frame
    clusters = do.call(cbind,clusters)
    cluster_names = colnames(clusters)
    not_clusters = do.call(cbind,not_clusters)
    #Remove duplicate columns
    not_clusters = not_clusters[,!duplicated(colnames(not_clusters),fromLast = TRUE)]
  } else {
    #Read the single excel file
    print("Detected a single data file")
    #Read the data
    data = na.omit(read.xlsx(filename,sheet=sheetname))
    #Choose only the groups you want to include in the analysis
    data = data[data[,group_col] %in% groups,]
    #Create separate data frames for clusters and other data
    clusters = data[,cell_cols]
    cluster_names = colnames(clusters)
    if (length(not_cell_cols)==1){
      not_clusters = as.data.frame(data[,not_cell_cols])
      colnames(not_clusters) = colnames(data)[not_cell_cols]
    }else{
      not_clusters = data[,not_cell_cols]
    }
    #If the cluster prefix is not NULL, paste it in here
    if(!is.null(cluster_prefix)){
      colnames(clusters) = paste(cluster_prefix[[i]],colnames(clusters[[i]]),sep = " ")
    }
    
  }
  #Perform the correlation calculations
  clusters = as.matrix(clusters)
  not_clusters = as.matrix(not_clusters)
  tot_corr = rcorr(clusters, not_clusters, type=corr_type)
  p_value = as.data.frame(tot_corr$P)
  #******Replace NaN by 0 here....******
  p_value[is.na(p_value)] <- 0
  if (length(not_cell_cols)==1){
    p_value_sub = as.data.frame(p_value[,-which(colnames(p_value)%in%cluster_names)])
    colnames(p_value_sub) = colnames(data)[not_cell_cols]
    not_rows=colnames(p_value_sub)
  }else{
    p_value_sub = p_value[,-which(colnames(p_value)%in%cluster_names)];not_rows=colnames(p_value_sub)
  }
  p_value_sub = p_value_sub[!rownames(p_value_sub) %in% not_rows,]
  r_value = as.data.frame(tot_corr$r)
  #******Replace NaN by 0 here....******
  r_value[is.na(r_value)] <- 0
  if (length(not_cell_cols)==1){
    r_value_sub = as.data.frame(r_value[,-which(colnames(r_value)%in%cluster_names)])
    colnames(r_value_sub) = colnames(data)[not_cell_cols]
    not_rows=colnames(r_value_sub)
  }else{
    r_value_sub = r_value[,-which(colnames(r_value)%in%cluster_names)];not_rows=colnames(r_value_sub)
  }
  r_value_sub = r_value_sub[!rownames(r_value_sub) %in% not_rows,]
  #Export the data if you choose
  if (export){
    write.xlsx(r_value, r_export_name, row.names = TRUE)
    write.xlsx(p_value, p_export_name, row.names = TRUE)
    print('Finished Exporting Results')
  }
  print("Analysis Finished")
  return(list(clusters,not_clusters,r_value_sub,p_value_sub,r_value,p_value,tot_corr,filename,sheetname))
}


OptimalClustering = function(corr_results,export_name,corr_matrix=NULL,distance="euclidean",method="ward.D2",min_clust=5,max_clust=10){
  #This is a function for determining the optimal number of clusters to choose for your correlation matrix
  
  #corr_results: Resulting list from CorrCalculation function
  
  #corr_matrix: Dataframe with correlation coefficient values to use if you want to do it manually
  
  #distance: Distance method for clustering. Default is euclidean
  
  #method: Clustering method. Default is ward.D2
  
  #min_clust: Minimum number of clusters
  
  #max_clust: Max number of clusters
  
  #Set your dataframe if you input that instead of the corr_results output from CorrCalculation function
  if (!is.null(corr_matrix)){
    print("Detected manual matrix input")
    data = as.data.frame(corr_matrix)
  } else {
    data = corr_results[[3]]
  }
  #Run the calculation
  results=NbClust(data,distance=distance,min.nc = min_clust,max.nc = max_clust,
                  method = method)
  #Extract the results
  results$Best.nc
  results$Best.partition
  print(results)
  capture.output(results, file = export_name)
  #Return the output
  return(list(data,results$Best.nc,results$Best.partition))
}






















