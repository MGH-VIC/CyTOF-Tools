#Function for DAC spadevizR
#Joshua Hess

IterativeDAC = function(ImportDataMaster_list){
  #Run after scatterplot generation only
  
  #Create a temporary dataframe
  tmp_frame = ImportDataMaster_list[["GroupAssignment"]]
  
  InnerDAC = function(comb_list,idx){
    #Function for running spadevizR inside a try() function. This will
    #allow all calculations to be run even if there is an error in the DAC function.
    #This was a problem if all samples contain 0s in the cluster abundances
    
    #Get the names
    cond1name = as.character(comb_list[1,idx])
    cond2name = as.character(comb_list[2,idx])
    #Get the samples in this comparison
    condition1 = ImportDataMaster_list[["DAC"]][[as.character(comb_list[1,idx])]]
    #Get the samples in this comparison
    condition2 = ImportDataMaster_list[["DAC"]][[as.character(comb_list[2,idx])]]
    
    #Conduct the analysis
    dat <- identifyDAC(ImportDataMaster_list[["ImportedData"]], condition1 = condition1, condition2 = condition2,
                       th.pvalue = 0.05, th.fc = 1,
                       method.paired = FALSE, use.percentages = FALSE)
    ###Change the mean and sd in DAC results to reflect the condition names so we can visualize better####
    colnames(dat@results) = c("cluster",
                              paste("mean.",cond1name,sep=""),
                              paste("sd",cond1name,sep=""),
                              paste("mean.",cond2name,sep=""),
                              paste("sd",cond2name,sep=""),
                              "fold.change",
                              "pvalue",
                              "significant")
    #Write csv with the results
    write.csv(dat@results, paste(cond1name,cond2name,"DAC_p_values.csv", sep="_"),
              row.names = FALSE)
    #Export an image
    jpeg(paste(cond1name,"_",cond2name,".jpeg",sep=""),
         width=2000,
         height=1500, 
         res = 300)
    SPADEVizR::plot(dat)
    dev.off()
  }
  
  #Get the unque names in the groups
  groups = unique(tmp_frame[["Group"]])
  #Create a DAC object to store the results in
  ImportDataMaster_list[["DAC"]] = list()
  #Go through each unique group and add to the list
  for (grp in groups){
    #Get the samples that in this group
    ImportDataMaster_list[["DAC"]][[grp]] = tmp_frame[which(tmp_frame[["Group"]]==grp),"sample"]
  }
  #Get all combinations of the groups
  comb_list = as.data.frame(lapply(2, FUN = function(x)combn(groups, x)))
  
  #Iterate through each combination and run the analysis
  for (i in 1:ncol(comb_list)){
    try(InnerDAC(comb_list,i))
  }
  #Return the list with DAC results
  return(ImportDataMaster_list)
}


