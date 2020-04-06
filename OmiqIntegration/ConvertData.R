#Module for converting OmiqAI exported data to VIC CyTOF Tools format
#Joshua Hess
#Check for missing packages and install if needed
list.of.packages <- c("openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Import packages
require('openxlsx')


ConvertOmiqClusterCSVs = function(csv_dir,concat_by,out_name=NULL){
  #Function for importing csv files exported from Omiq AI clustering. Function
  #takes in a folder containing the csv directory and will read all data.
  #Data will be concatenated by the string that you choose (Ex: "fsom_metaclust_elbow")
  #Out name will be exported excel file name and directory. If no directory it will be in current.
  #If out name is left null, then no file will be exported.
  
  #Get the list of csv files in the directory
  csvs = list.files(path = csv_dir, pattern = "*.csv", full.names = TRUE)

  #Read the data into a list
  omiq_dat = list()
  #Keep a list of the min-max values of cluster assignment
  min_clust = 1
  max_clust = 1
  
  #Iterate through the files
  for (f in csvs){
    #Read the data
    omiq_dat[[f]] = read.csv(f,check.names = FALSE)
    
    #Check the min max values of cluster assignment
    if (max(omiq_dat[[f]][concat_by]) > max_clust){
      #Add the new cluster max to the max
      max_clust = max(omiq_dat[[f]][concat_by])
    }
    #Check min values
    if (min(omiq_dat[[f]][concat_by]) < min_clust){
      #Add the new cluster min to the min
      min_clust = min(omiq_dat[[f]][concat_by])
    }
  }
  
  #Create a dataframe with the same columns as the omiq data in the VorteX format
  df <- data.frame()
  #Get names in the omiq data set that are clusters
  omiq_names <- c(colnames(omiq_dat[[1]]))
  #Exclude the omiq cluster columns (contain "clust")
  mod_omiq_names = omiq_names[-c(grep("clust",omiq_names))]
  #Get new names for our data frame
  new_names = c("Cluster","Term","Count")
  
  #Add these new names to the dataframe
  for (k in new_names) df[[k]] <- as.character()
  #Create columns for the new dataframe excluding the omiq cluster columns (contain "clust")
  for (k in mod_omiq_names) df[[k]] <- as.character()
  
  #Create a counter
  idx = 1
  #Iterate through each file in the list
  for (clust in min_clust:max_clust){
    #Iterate through each of the clusters and concatenate the data
    for (f in names(omiq_dat)){
      
      #Get the term or file being used
      term = strsplit(f, "/")[[1]]
      term = term[length(term)]
      #Get the values of the data which match the cluster number
      clust_f = omiq_dat[[f]][which(omiq_dat[[f]][concat_by]==clust),]
      #Get number of cells in this cluster for this file
      n = nrow(clust_f)
      #Get the mean values of the modified omiq columns
      means = colMeans(clust_f[sapply(clust_f,is.numeric)])
      #Get only the modified omiq names (markers of interest)
      means = means[c(mod_omiq_names)]
      
      #Update the dataframe with the mean values
      df[idx,mod_omiq_names] = means
      #Update the dataframe with the new cluster number, term and count
      df[idx,new_names] = c(clust,term,n)
      
      #Update the index counter
      idx = idx + 1
    }
  }
  
  #Convert the character columns to numeric
  df[mod_omiq_names]<- lapply(df[mod_omiq_names], as.numeric)
  #Replace NA values with 0 to match VorteX output
  df[is.na(df)] = 0
  
  #Check to see if xporting an excel file
  if (!is.null(out_name)){
    #Export an excel file
    openxlsx::write.xlsx(df,out_name)
  }
  #Return the new Omiq AI converted dataframe in VorteX format
  return(df)
}












#
        