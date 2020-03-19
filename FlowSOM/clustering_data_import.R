#Data import to flowSOM
#Joshua Hess



#data subsampling function...slow for large samples!
DataSubsample = function(mydata, perc_of_total){
  #This function will subsample a dataframe
  #mydata: A dataframe containing single-cell data
  #perc_of_total: fraction indicating total sample size (proportion of the total e.g. 0.7)
  
  #Subsample data
  set.seed = 1231213
  sample_size = perc_of_total*(nrow(mydata))
  temp_subsampled = mydata[sample(x=nrow(mydata),size = sample_size,replace = FALSE),]
  #Return the subsampled data
  return(temp_subsampled)
}




ReadFiles = function(files_list,subsample_perc=NULL){
  #This function will read all files in the list provided, and will 
  #subsample according to the percentage that is used.
  
  #Set up a list to store each file in
  dat = list()
  if (file_ext(files_list) == "csv"){
    #Read data
    for (file in files_list){
      #Get a temporary name for the file
      fname = gsub(".csv","",file)
      #Read the csv data
      temp_dat = read.csv(file)
      #Change rownames to keep track of cells
      rownames(temp_dat) = paste("Cell",rownames(temp_dat),fname,sep = "_")
      #Check for subsampling
      if (!is.null(subsample_perc)){
        #If subsampling, apply the subsample function
        temp_dat = DataSubsample(temp_dat,perc_of_total = subsample_perc)
      }
      #Add this data table to the list
      dat[[fname]] = temp_dat
    }
  } else if (file_ext(files_list) == "fcs"){
    temp_full = as.data.frame(read.FCS(files_list)@exprs)
  }
  
  #Change rownames to keep track of cells
  rownames(temp_full) = paste("Cell",rownames(temp_full),files_list,sep = "_")
  rownames(temp_full) = stringr::str_remove_all(rownames(temp_full),file_ext(files_list))
  
  #Return the full dataframe
  return(temp_full)
}






GetNormParameters = function(full_data, percentile, cols_to_use){
  #This function will normalize all of your data columns to whatever percentile that you input
  
  temp_full = full_data[,cols_to_use]
  
  # Now we will find the 99.5th percentile value of each column:
  col99th <- sapply(as.data.frame(temp_full), quantile, probs = percentile)
  
  return(col99th)
}








NormalizeData = function(mydata, norm_input, cols_to_use){
  
  #This function will normalize the data that you input according to the norm_input parameter
  #Must have the value returned from get.normalize function
  
  # Now we will divide the data by the 99.5th percentile values:
  tmp_data=mydata[,cols_to_use]
  mydata_normalized <- t(t(tmp_data) / norm_input)
  
  return(as.data.frame(mydata_normalized))
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









#
