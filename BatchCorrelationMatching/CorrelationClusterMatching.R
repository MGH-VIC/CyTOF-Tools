#Functions for running correlation between two clusterings
#Joshua Hess

#Check for missing packages and install if needed
list.of.packages <- c("officer","magrittr","magick","DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#load the packages
require('officer')
require(magrittr)
require("magick")
require("DT")




MinMaxRescale <- function(ImportDataMaster_list){
  
  #read the file
  raw_table = ImportDataMaster_list[["Raw"]]
  
  #modify the column name to remove X. or X
  colnames(raw_table) = gsub("X.", "", colnames(raw_table), fixed = TRUE)
  colnames(raw_table) = gsub("X", "", colnames(raw_table), fixed = TRUE)
  
  #modify the cluster name to have dashes inserted
  raw_table$Cluster = gsub("Cluster" , "Cluster_", raw_table$Cluster, fixed = TRUE)
  raw_table$Cluster = gsub(" ", "", raw_table$Cluster, fixed = TRUE)
  raw_table$Cluster = paste("Cluster_", raw_table$Cluster, sep = "")
  
  #sorting the dataset for better view
  raw_table = raw_table[order(raw_table$Cluster, raw_table$Term, decreasing = FALSE),]
  
  #obtain the samples name
  samples = unique(raw_table$Term)
  
  #obtain the amount of samples
  nSample= length(samples)
  
  #obtain the cluster name
  clusters = unique(raw_table$Cluster)
  
  #obtain the amount of clusters
  nCluster = length(clusters)
  
  #create a blank table with labels
  mean_marker_total_cells = data.frame(tmp_name = 0)
  
  for(i in 1:(ncol(raw_table)-1)){
    mean_marker_total_cells = cbind(mean_marker_total_cells, 0)
  }
  
  colnames(mean_marker_total_cells) = colnames(raw_table)
  mean_marker_total_cells = mean_marker_total_cells[, colnames(mean_marker_total_cells)!= "Term"]
  
  # creat a array for storing the number of total numbers of cluster for samples
  cluster_count = rep(0,nCluster)
  
  #calculate and store the total numbers of cluster for samples
  j = 1
  k = 1
  for(i in 1:nrow(raw_table)){
    if(i == nrow(raw_table)){
      cluster_count[j] = cluster_count[j] + 1
      for(n in 1:ncol(mean_marker_total_cells)){
        if(n == 1){
          mean_marker_total_cells[k,n] = raw_table$Cluster[i]
        }
        if(n == 2){
          mean_marker_total_cells[k,n] = sum(raw_table[(i-cluster_count[j]+1):i,n+1])
        }
        if(n > 2){
          mean_marker_total_cells[k,n] = mean(raw_table[(i-cluster_count[j]+1):i,n+1])
        }
      }
      break()
    }
    
    if(raw_table$Cluster[i] == raw_table$Cluster[i+1]){
      cluster_count[j] = cluster_count[j] + 1
    }else{
      cluster_count[j] = cluster_count[j] + 1
      for(n in 1:ncol(mean_marker_total_cells)){
        if(n == 1){
          mean_marker_total_cells[k,n] = raw_table$Cluster[i]
        }
        if(n == 2){
          mean_marker_total_cells[k,n] = sum(raw_table[(i-cluster_count[j]+1):i,n+1])
        }
        if(n > 2){
          mean_marker_total_cells[k,n] = mean(raw_table[(i-cluster_count[j]+1):i,n+1])
        }
      }
      mean_marker_total_cells = rbind(mean_marker_total_cells, 0)
      j = j + 1
      k = k + 1
    }
  }
  
  tmp_rescale <- function(x) (x-min(x))/(max(x) - min(x))
  
  tmp_mean_marker_total_cells = mean_marker_total_cells
  
  tmp_mean_marker_total_cells$Cluster = paste(ImportDataMaster_list[["ExpName"]], 
                                              "_",
                                              tmp_mean_marker_total_cells$Cluster, 
                                              sep = "")
  
  rownames(tmp_mean_marker_total_cells) = tmp_mean_marker_total_cells[,1]
  tmp_mean_marker_total_cells = tmp_mean_marker_total_cells[,-1]
  
  tmp_mean_marker_total_cells$Count1 = tmp_mean_marker_total_cells$Count
  
  for(i in 1:(ncol(tmp_mean_marker_total_cells)-1)){
    tmp_mean_marker_total_cells[,i] = tmp_rescale(tmp_mean_marker_total_cells[,i])
  }
  #Create a frame based on count
  count_frame = as.data.frame(tmp_mean_marker_total_cells$Count1)
  colnames(count_frame) = c("Count")
  rownames(count_frame) = rownames(tmp_mean_marker_total_cells)
  #Remove the count1 column from the data
  tmp_mean_marker_total_cells = tmp_mean_marker_total_cells[,-which(colnames(tmp_mean_marker_total_cells)%in% c("Count1"))]
  #Append the results to the ImportedData list
  ImportDataMaster_list[["RescaledData"]] = tmp_mean_marker_total_cells
  ImportDataMaster_list[["Count"]] = count_frame
  #Return the altered list
  return(ImportDataMaster_list)
} 


MultipleMinMaxRescale = function(Multi_list){
  #Function for rescaling multiple files in the import data master list
  
  #Run the min max rescaling for each data set included
  for (i in 1:length(Multi_list)){
    #Rescale the data
    Multi_list[[Multi_list[[i]][["ExpName"]]]] = MinMaxRescale(Multi_list[[i]])
  }
  #Return the data
  return(Multi_list)
}


FilterSize = function(ImportDataMaster_list, min_size=NULL){
  #Function for filtering clusters based on total count
  
  #Set an object to be the "Count" dataframe for convenience
  count = ImportDataMaster_list[["Count"]]
  #If the minimum size parameter is left blank, filter 0.2%
  if (is.null(min_size)){
    #Calculate the percentage
    min_size = sum(count$Count)*0.002
  }
  #Get the clusters in the count column which are below the specified size
  count = count %>%
    #Create temporary column for cluster ID
    tibble::rownames_to_column('ClusterID') %>%
    #Filter based on size
    filter(Count > min_size) %>%
    #Remove the cluster ID and convert to rownames
    tibble::column_to_rownames('ClusterID')
  #Get the rownames of the filtered count data and filter the corresponding rescaled data
  new_rescaled = ImportDataMaster_list[["RescaledData"]][which(rownames(ImportDataMaster_list[["RescaledData"]]) %in% rownames(count)),]
  #Create the new list again
  return_list = list(new_rescaled,count,ImportDataMaster_list[["ExpName"]])
  #Alter the list to return
  ImportDataMaster_list[["RescaledDataFiltered"]] = new_rescaled
  ImportDataMaster_list[["CountFiltered"]] = count
  #Return the list
  return(ImportDataMaster_list)
}


MultipleFilterSize = function(Multi_list){
  #Function for rescaling multiple files in the import data master list.
  #For now, this function will only calculate the 0.2% minimum size
  
  #Run the min max rescaling for each data set included
  for (i in 1:length(Multi_list)){
    #Rescale the data
    Multi_list[[Multi_list[[i]][["ExpName"]]]] = FilterSize(Multi_list[[i]])
  }
  #Return the data
  return(Multi_list)
}



CorrelationMatching = function(Rescaled1,Rescaled2,method = "spearman",include_count=FALSE,csv_name=NULL){
  #Function for calculating correlation coefficient between groups 
  
  #create a blank table to store the pearson correlation results
  results<-data.frame(experiment1_cluster = 0, experiment2_cluster = 0, experiment1_count = 0, experiment2_count = 0)
  #Change the column names
  exp_names = c(paste(Rescaled1[["ExpName"]],"Cluster"),paste(Rescaled2[["ExpName"]],"Cluster"),
                paste(Rescaled1[["ExpName"]],"Count"),paste(Rescaled2[["ExpName"]],"Count"))
  colnames(results) = exp_names
  
  #Check if including count
  if (!include_count){
    #Remove the count from the two lists of data
    experiment1 = Rescaled1[["RescaledDataFiltered"]][,-which(colnames(Rescaled1[["RescaledDataFiltered"]])%in%c("Count"))]
    experiment2 = Rescaled2[["RescaledDataFiltered"]][,-which(colnames(Rescaled2[["RescaledDataFiltered"]])%in%c("Count"))]
  }else{
    #Assign the full dataset
    experiment1 = Rescaled1[["RescaledDataFiltered"]]
    experiment2 = Rescaled2[["RescaledDataFiltered"]]
  }
  
  #perform pairwise pearson correlation between experiment1 and experiment2
  t=1
  for(i in 1:nrow(experiment1)){
    for(j in 1:nrow(experiment2)){
      #Add results for the cluster names
      results[[1]][t]<-rownames(experiment1)[i]
      results[[2]][t]<-rownames(experiment2)[j]
      #Run the correlation calculation
      corr<-cor.test(as.numeric(experiment1[i,]),as.numeric(experiment2[j,]),method = method)
      #Get the p and r values
      results$Coeff[t]<-corr$estimate
      results$Pvalue[t]<-corr$p.value
      #Add the count
      results[[3]][t]<-Rescaled1[["CountFiltered"]]$Count[i]
      results[[4]][t]<-Rescaled2[["CountFiltered"]]$Count[j]
      
      t<-t+1
      results<-rbind(results, 0)
    }
  }
  #Reoder columns
  results = results[, c(1,2,5,6,3,4)]
  #Sorting the data for better view
  results = results[order(results$Coeff, decreasing = TRUE),]
  #Remove the entries with zero cluster name or negative correlation results
  results = results[-which(results[,1]==0 | results$Coeff < 0),]
  #Create a list for the results
  return_list = list(results,Rescaled1,Rescaled2,csv_name)
  #Change the names
  names(return_list) = c("CorrResults",paste(Rescaled1[["ExpName"]]),paste(Rescaled2[["ExpName"]]),"ExportLocation")
  #Check to see if exporting csv
  if(!is.null(csv_name)){
    #Write out the correlation results
    write.csv(results,csv_name,row.names = FALSE)
  }
  #return the list
  return(return_list)
}




FilterCorrResults = function(corr_results,top_n,csv_name=NULL){
  #Function for filtering the correlation results from cluster 
  #matching to include top_n results per cluster
  
  #Get the results from the correlation matching
  coef = corr_results[["CorrResults"]]
  #Order the data by decreasing correlation value for experiment 1
  coef <- coef[order(coef[,1], coef$Coeff, decreasing = TRUE),]
  #Get the unique cluster names for experiment 1
  cluster.name <- unique(coef[,1])
  #Create new dataframe
  new.data <- coef
  #Only take the first row of the dataframe
  new.data <- new.data[1,]
  #Create NA column
  new.data[1, ] <- NA
  #Iterate through the cluster names for top matching
  for(i in cluster.name){
    #i = "Grp1_Cluster_5169"
    tmp.data <- coef[which(coef[,1] == i), ]
    tmp.data <- tmp.data[1:top_n, ]
    new.data <- rbind(new.data, tmp.data)
  }
  #Remove the NA row
  new.data <- na.omit(new.data)
  #Check if exporting the data
  if (!is.null(csv_name)){
    #Export the data
    write.csv(new.data,csv_name,row.names = FALSE)
  }
  #Create a new list to return (Append these results to the corr_results list). Remove last row...all zero for some reason
  corr_results[["FilteredCorrResults"]] = new.data
  #Return the list
  return(corr_results)
}




GenerateMatchingPPT = function(Corr_list,ppt_name){
  # Function for generating a powerpoint after matching clusters with correlation
  #Must have run scattplot and parallel coordinate plot generation up to this point
  
  #Get the directory so we can export the powerpoint
  dir <- getwd()
  doc <- read_pptx() 
  #Get the names of the first two experiments (Index comes from the list returned by FilterCorrResults function)
  Grp1_name = Corr_list[[2]][["ExpName"]]
  Grp2_name = Corr_list[[3]][["ExpName"]]
  #Get the directory for the cluster PCP
  Grp1_PCP = Corr_list[[2]][["FilesPCP"]]
  Grp2_PCP = Corr_list[[3]][["FilesPCP"]]
  #Get the directory for the cluster scatterplots
  Grp1_Scat = Corr_list[[2]][["ScatterPlotFiles"]]
  Grp2_Scat = Corr_list[[3]][["ScatterPlotFiles"]]
  #Get the number of rows in the datatable
  matching_data = Corr_list[["FilteredCorrResults"]]
  num = nrow(matching_data)
  #Set index
  rownumber = 0
  
  #Iterate through the rows of the filtered corr results
  for(i in 1:num){
    #Get the cluster number from experiment 1
    Grp1_cluster_number = gsub(paste(Grp1_name,"Cluster_",sep="_"),"",matching_data[,1][i])
    #Get the cluster number from experiment 2
    Grp2_cluster_number = gsub(paste(Grp2_name,"Cluster_",sep="_"),"",matching_data[,2][i])
    
    #Get a name for this cluster and its location in the PCP files
    Grp1_cluster_file = paste(dir,"/",Grp1_PCP,"/Cluster ", Grp1_cluster_number, ".jpeg", sep = "")
    Grp2_cluster_file = paste(dir,"/",Grp2_PCP, "/Cluster ", Grp2_cluster_number, ".jpeg", sep = "")
    #Get a name for this cluster and its location in the Scatter plot files
    Grp1_scatter_file = paste(dir,"/",Grp1_Scat, "/Cluster ", Grp1_cluster_number, ".jpeg", sep = "")
    Grp2_scatter_file = paste(dir, "/",Grp2_Scat,"/Cluster ", Grp2_cluster_number, ".jpeg", sep = "")
    
    #Read the group 1 cluster PCP
    Grp1_cluster_file_magick = image_read(Grp1_cluster_file)
    #image_info(Grp1_cluster_file_magick)
    #Grp1_cluster_file_magick = image_crop(Grp1_cluster_file_magick, "2000x900")
    image_write(Grp1_cluster_file_magick, path = "Grp1_cluster_file.jpeg", format = "jpeg")
    #Read the group 1 scatter plot
    Grp1_scatter_file_magick = image_read(Grp1_scatter_file)
    #image_info(Grp1_cluster_file_magick)
    image_write(Grp1_scatter_file_magick, path = "Grp1_scatter_file.jpeg", format = "jpeg")
    
    #Read the group 2 cluster PCP
    Grp2_cluster_file_magick = image_read(Grp2_cluster_file)
    #image_info(Grp1_cluster_file_magick)
    #Grp2_cluster_file_magick = image_crop(Grp2_cluster_file_magick, "2000x900")
    image_write(Grp2_cluster_file_magick, path = "Grp2_cluster_file.jpeg", format = "jpeg")
    #Read the group 1 scatter plot
    Grp2_scatter_file_magick = image_read(Grp2_scatter_file)
    #image_info(Grp1_cluster_file_magick)
    image_write(Grp2_scatter_file_magick, path = "Grp2_scatter_file.jpeg", format = "jpeg")
    
    #Get the file for these temporary images
    Grp1_cluster_file = paste(dir, "/Grp1_cluster_file.jpeg", sep = "")
    Grp2_cluster_file = paste(dir, "/Grp2_cluster_file.jpeg", sep = "")
    #Get the file for these temporary images
    Grp1_scatter_file = paste(dir, "/Grp1_scatter_file.jpeg", sep = "")
    Grp2_scatter_file = paste(dir, "/Grp2_scatter_file.jpeg", sep = "")
    
    #Create slide title
    slide_title = paste(matching_data[,1][i], " VS ", matching_data[,2][i], sep = "")
    
    doc <- doc %>%
      add_slide(layout = "Two Content", master = "Office Theme") %>%
      #ph_with_text(type = "title", str = slide_title) %>%
      #ph_with_img(type = "body", str = "body (index 1) is text", index = 1) %>% 
      # ph_with_img(type = "body", index = 1, src = Grp1_cluster_file, height = 3.5, width = 4.58 ) %>%
      # ph_with_img(type = "body", index = 2, src = Grp2_cluster_file, height = 3.5, width = 4.58 )
      ph_with_img_at(src = Grp1_cluster_file, height = 3.4, width = 5.1, left = 0.1, top = 0) %>%
      ph_with_img_at(src = Grp2_cluster_file, height = 3.4, width = 5.1, left = 0.1, top = 3.4)%>%
      
      ph_with_img_at(src = Grp1_scatter_file, height = 2, width = 2, left = 5.32, top = 0.55) %>%
      ph_with_img_at(src = Grp2_scatter_file, height = 2, width = 2, left = 5.32, top = 3.75)
    #Remove the temporary files
    file.remove(Grp1_cluster_file, Grp2_cluster_file, Grp1_scatter_file, Grp2_scatter_file)
    #Print the index
    print(i)
  }
  #Print out the ppt document
  print(doc, target = ppt_name)
}




MultiplePairwiseCorrelations = function(Multi_list,method = "spearman",include_count=FALSE,export_csv=FALSE){
  #Function for computing multiple pariwise correlations and storing the results in a list
  #Depends on the CorrelationMatching function but will not yet export powerpoints for validation
  
  #Create a list to store the results in 
  results = list()
  #Generate all combinations of pairwise comparisons in the rescaled list of cluster matchings
  comb_list = as.data.frame(lapply(2, FUN = function(x)combn(names(Multi_list), x)))
  #Iterate through each column of comb_list, extract the group names (two rows = two groups)
  for (i in 1:ncol(comb_list)){
    #Get a name for this comparison
    comp_name = paste(comb_list[,i],collapse = "_")
    #Get the groups for this comparison
    grp_a = paste(comb_list[1,i])
    grp_b = paste(comb_list[2,i])
    #Check if exporting a csv
    if (export_csv){
      #Create a csv name if exporting
      csv_name = paste(comp_name,"CorrResults.csv",sep="_")
    }else{
      #Create a null csv name
      csv_name = NULL
    }
    #Extract the groups from these names and run the correlation comparison
    results[[comp_name]] = CorrelationMatching(Multi_list[[grp_a]],Multi_list[[grp_b]],method = method,include_count=include_count,csv_name=csv_name)
  }
  #Return the results list
  return(results)
}









#




