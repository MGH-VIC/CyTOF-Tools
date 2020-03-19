#Data import to flowSOM
#Joshua Hess

#Check for missing packages and install if needed
list.of.packages <- c("FlowSOM", "flowCore","dplyr","tibble","stringr","utils","rowr","tools","stats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(FlowSOM)
require(flowCore)
require(dplyr)
require(stringr)
require(tibble)
require(utils)
require(rowr)
require(stats)
require(tools)


ReadFullData = function(files){
  #This function will read the full file list 
  
  if (file_ext(files) == "csv"){
    #Print update
    print('Detected csv extensions...')
    #Iterate through the list of files to extract data
    
    #Read data
    temp_full = read.csv(files)
    
    
    
    
    
    
  } else if (file_ext(files) == "fcs"){
    temp_full = as.data.frame(read.FCS(files)@exprs)
  }
  
  #Change rownames to keep track of cells
  rownames(temp_full) = paste("Cell",rownames(temp_full),files,sep = "_")
  rownames(temp_full) = stringr::str_remove_all(rownames(temp_full),file_ext(files))
  
  #Return the full dataframe
  return(temp_full)
}



GetPercentileParameters = function(full_data, percentile, cols_to_use){
  #This function will normalize all of your data columns to whatever percentile that you input
  
  temp_full = full_data[,cols_to_use]
  
  # Now we will find the 99.5th percentile value of each column:
  col99th <- sapply(as.data.frame(temp_full), quantile, probs = percentile)
  
  return(col99th)
}



#Filtering out data that is above a percentile for each channel
FilterData = function(data_to_filter,percentile,cols_to_use){
  
  #This function will exclude outliers in your dataset by removing those cells that fall
  #above the given percentile for a particular channel.
  #data_to_filter: The dataframe that will be filtered
  #percentile: The percentile number used for filtering (e.g 0.995 = 99.5th percentile)
  #cols_to_use: Columns to use for filtering. Typically set these for all channel markers,
  #but not the spatial information contained in the histoCAT csv output files.
  
  #Form a list to store all dataframes
  exclusion_list = list()
  
  #Loop to extract rows that fall above the percentile for each column
  for (i in cols_to_use){
    tmp_rows = data_to_filter[data_to_filter[,i]>quantile(data_to_filter[,i],percentile),]
    exclusion_list[[i]] = tmp_rows
  }
  
  #Extract unique rows that are above percentile for rows
  exclusion_table = unique(do.call(rbind,exclusion_list))
  
  #Exclude those cells from the data
  return(as.data.frame(subset(data_to_filter,!(rownames(data_to_filter) %in% rownames(exclusion_table)))))
}


NormalizeData = function(mydata, norm_input, cols_to_use){
  
  #This function will normalize the data that you input according to the norm_input parameter
  #Must have the value returned from get.normalize function
  
  # Now we will divide the data by the 99.5th percentile values:
  tmp_data=mydata[,cols_to_use]
  mydata_normalized <- t(t(tmp_data) / norm_input)
  
  return(as.data.frame(mydata_normalized))
}



#data subsampling function...slow for large samples!
SubsampleData = function(mydata, perc_of_total){
  
  #This function will subsample all of the csv files within your directory. This is the first
  #step for importing data from histoCAT to R for clustering
  #files_list: A list of files pattern ending indicated by the list.files function above
  #perc_of_total: fraction indicating total sample size (proportion of the total e.g. 0.7)
  
  #Subsample data
  set.seed = 1231213
  sample_size = perc_of_total*(nrow(mydata))
  temp_subsampled = mydata[sample(x=nrow(mydata),size = sample_size,replace = FALSE),]
  
}


