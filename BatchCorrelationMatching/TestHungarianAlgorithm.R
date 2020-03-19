#Functions for running correlation between two clusterings
#Joshua Hess

#Check for missing packages and install if needed
list.of.packages <- c("RcppHungarian","rowr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
library(RcppHungarian)
library(rowr)

#Import custom modules
source("ImportData.R")
source("ScatterPlot.R")
source("ParallelCoordinatePlot.R")
source("CorrelationClusterMatching.R")

#Get data for group1 (scatterplot import will require a filled out sheet!)
imported_data = MultipleImportDataMaster(Excel_list = c("Grp 1 D10 T Cells 20181204 K=17.xlsx","Grp 2 D10 T Cells 20181204 K=25.xlsx","Grp 1 D10 T Cells 20181204 K=17.xlsx"),
                                         experiment_names = c("Grp1","Grp2","Grp3"),
                                         scatterplot_import=FALSE,save_new=FALSE,save_as=NULL,pheno_cols=NULL)


#Filter each dataset and rescale
imported_data = MultipleMinMaxRescale(imported_data)
imported_data = MultipleFilterSize(imported_data)

#Match clusters across experiments
Corr_test = MultiplePairwiseCorrelations(imported_data,method = "spearman",include_count=FALSE,export_csv=TRUE)








Corr_results = Corr_test

HungarianOptimization = function(Corr_results){
  #Function for iterating through correlation results and applying hungarian algorithm
  #to each possible subset of clusters from each group normalized by total pairings to find
  #the best possible subset
  
  #Get the names of the comparisons in the multiple pairwise correlations
  comps = names(Corr_results)
  #Create a list to store the Hungarian algorithm results
  results = list()
  
  #Iterate through the comparisons
  for (i in 1:length(comps)){
    #Get the correlation results from this comparison
    tmp_results = Corr_results[[comps[i]]]
    #Get the correlation dataframe from this comparison
    og_corr = tmp_results[["CorrResults"]][,1:3]
    #Create a new datafame
    corr = og_corr
    #Change column names
    colnames(corr) = c("grp_a","grp_b","Coeff")
    #Create new sheet with wide format (replace NA with zero)
    corr = corr %>%
      spread(grp_b, Coeff,0)
    #Change the rownames to equal group 1 assignment
    rownames(corr)=corr[,1]
    #Remove the first row
    corr = corr[,-1]
    #Create matrix and subtract from 1 (Hungrarian is minimzation problem)
    corr = as.matrix(1-corr)
    #Get results from hungarian algorithm
    hung = HungarianSolver(corr)
    #Change the name of the hung
    names(hung) = c("Value","Matches")
    #Use the indexed hung from the Hungarian solver to extract the cluster names
    for (j in 1:nrow(hung[["Matches"]])){
      #Get the cluster from grp a
      clust_a = rownames(corr)[as.numeric(hung[["Matches"]][j,1])]
      #Get the cluster from grp b
      clust_b = colnames(corr)[as.numeric(hung[["Matches"]][j,1])]
      #Extract the corresponding cluster from group one
      hung[["Matches"]][j,1] = clust_a
      #Extract the corresponding cluster from group one
      hung[["Matches"]][j,2] = clust_b
    }
    #Create dataframe from the matches
    hung[["Matches"]] = as.data.frame(hung[["Matches"]])
    #Update the column names of the matches to reflect the group clusters
    colnames(hung[["Matches"]]) = colnames(og_corr)[1:2]
    #Update the results list to include this comparison
    results[[comps[i]]] = hung
  }
  #Now extract the results from each of the comparisons to construct a final dataframe
  test = cbind.fill(results[[1]][["Matches"]],results[[2]][["Matches"]],results[[3]][["Matches"]], fill = NA)
  #Return the results
  return(results)
}





