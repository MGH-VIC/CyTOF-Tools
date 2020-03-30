#Cluster Matching script
#Joshua Hess
#Check for missing packages and install if needed
list.of.packages <- c("devtools","Rcpp","biclust","diptest","evtree","ggdendro","ggfortify","ggplot2","gplots","gdata","ggrepel",
                      "ggRandomForests","gridExtra","gtable","gtools","igraph","MASS","packcircles","plyr","randomForestSRC",
                      "reshape2","pheatmap","readxl","raster","openxlsx","bindrcpp","stringi","statmod","tidyr","plotflow")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#Check for github packages
require('devtools')
list_git = c("tchitchek-lab/SPADEVizR","trinker/plotflow")
new.packages <- list_git[!(list_git %in% installed.packages()[,"Package"])]
if(length(new.packages)) install_github(new.packages,force=TRUE)
#Check for Biomanager packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install(version = "3.10")
list_bio = c("FlowSOM","flowCore","edgeR")
new.packages <- list_bio[!(list_bio %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  for (pack in new.packages){
    BiocManager::install(pack, suppressUpdates = TRUE)
  }
}


require(dplyr)
require(openxlsx)
require(tidyverse)
require('Rcpp')
require('biclust')
require('data.table')
require('diptest')
require('evtree')
require('ggdendro')
require("ggfortify")
require('ggplot2')
require('gplots')
require('gdata')
require('ggrepel')
require('ggRandomForests')
require('gridExtra')
require('gtable')
require('gtools')
require('igraph')
require('MASS')
require('packcircles')
require('plyr')
require("randomForestSRC")
require('reshape2')
require('pheatmap')
require('readxl')
require("raster")
require('openxlsx')
require("FlowSOM")
require('Rcpp')
require("SPADEVizR")
require(statmod)
require("edgeR")
require(RColorBrewer)
require("tidyr")
require(plotflow)

#Import custom modules
source("utils.R") #Sources utils function for phenoviewer_modified



SetupTable = function(excel,save_new=FALSE,save_as = NULL,remove=NULL,remove_marker_string=NULL){
  #Function for setting up an excel document for cluster matching so that you dont have to use
  #Excel functions for everything and that dont work
  
  #Read the data
  data = openxlsx::read.xlsx(excel)
  #Check for samples to remove
  if (!is.null(remove)){
    #Remove the indicated samples from the dataframe
    data = data[-which(data$Term %in% remove),]
  }
  #Get the names of the columns excluding the first two and add custom names
  pheno_colnames = c("Term","Cluster",colnames(data)[3:ncol(data)])
  #Reorder the original sheet columns to be the new ones
  pheno = data[pheno_colnames]
  #Check if removing strings for the markers
  if (!is.null(remove_marker_string)){
    #Get the pattern and remove
    new_names <- str_match(colnames(pheno), paste(remove_marker_string[1], "(.*?)", remove_marker_string[2],sep=""))
    #Replace the NAs (where there is no string match) with the given name
    for (i in 1:nrow(new_names)){
      #Check if na
      if (is.na(new_names[i,2])){
        #Assign new value to be the original
        new_names[i,2] = colnames(pheno)[i]
      }
    }
  }
  #Assign new column names to the pheno table
  colnames(pheno) = new_names[,2]
  #Set up sheet for abundances
  abundance = pheno[c("Term","Cluster","Count")] %>% 
    tidyr::spread(key = Term,value = Count)
  
  #Create list to store all of the data
  list_of_datasets <- list("raw" = data, "pheno" = pheno,"abun"= abundance,"filename"=excel)
  #Write the new datasheets if chosen
  if (save_new){
    write.xlsx(list_of_datasets, file = save_as)
  }
  #Return the datasets
  return(list_of_datasets)
}




ImportSpadeVizR = function(files,scatterplot_import=TRUE,pheno_cols=NULL){
  #Function for iterating over clusters to export parallel coordinate plots using the modified
  #phenoviewer function
  
  #Check if it is a list or not
  if (class(files) == "list"){
    print("Detected a list of data files")
    
    #Set up the data for abundances
    cluster.abundances = as.data.frame(files[["abun"]])
    #Change the rownames to be the cluster ID
    rownames(cluster.abundances) = paste("Cluster",cluster.abundances[["Cluster"]])
    #Remove the cluster column
    cluster.abundances = cluster.abundances[,-which(colnames(cluster.abundances)%in% c("Cluster"))]
    
    #Check to see if exporting a table to fill in for scatterplot generating
    if (scatterplot_import){
      #Get unique column names in abundance sheet
      samnames = as.data.frame(unique(colnames(cluster.abundances)));colnames(samnames) = c("sample")
      #Add column to fill in for the group
      samnames$Group = ''
      #Get the name of the excel sheet and modify for export
      grp_name = paste("groups_",gsub('.xlsx','',files[["filename"]]),'.xlsx',sep='')
      #Export the table as an excel file for the user to fill out
      openxlsx::write.xlsx(samnames,grp_name)
    }else{
      #Just put grp name as null
      grp_name = NULL
    }
    
    #Set up data for phenotypes
    cluster.phenotypes <- as.data.frame(files[["pheno"]])
    #Select only the columns you choose if there is input
    if (!is.null(pheno_cols)){
      cluster.phenotypes = cluster.phenotypes[,pheno_cols]
    }
    #Remove the Count column of the phenotype
    cluster.phenotypes = cluster.phenotypes[,-which(colnames(cluster.phenotypes)%in% c("Count"))]
    #Change the cluster column to include the term cluster
    cluster.phenotypes$Cluster = paste("Cluster",cluster.phenotypes$Cluster)
  } else {
    print("Detected excel file")
    
    #Read the cluster abundance data
    cluster.abundances <- openxlsx::read.xlsx(files, sheet = "abun")
    #Set up the data for abundances
    cluster.abundances = as.data.frame(files[["abun"]])
    #Change the rownames to be the cluster ID
    rownames(cluster.abundances) = paste("Cluster",cluster.abundances[["Cluster"]])
    #Remove the cluster column
    cluster.abundances = cluster.abundances[,-which(colnames(cluster.abundances)%in% c("Cluster"))]
    
    #Read the phenotype data
    cluster.phenotypes <- openxlsx::read.xlsx(files, sheet = "pheno")
    #Set up data for phenotypes
    cluster.phenotypes <- as.data.frame(files[["pheno"]])
    #Select only the columns you choose if there is input
    if (!is.null(pheno_cols)){
      cluster.phenotypes = cluster.phenotypes[,pheno_cols]
    }
    #Remove the Count column of the phenotype
    cluster.phenotypes = cluster.phenotypes[,-which(colnames(cluster.phenotypes)%in% c("Count"))]
    #Change the cluster column to include the term cluster
    cluster.phenotypes$Cluster = paste("Cluster",cluster.phenotypes$Cluster)
  }
  #Import the data
  import_dat = importResultsFromTables(cluster.abundances = cluster.abundances, cluster.phenotypes = cluster.phenotypes)
  
  if (is.null(grp_name)){
    #Create a list to return with all the imported data
    return_list = list(import_dat,cluster.phenotypes,cluster.abundances,files[["raw"]])
    #Change the names of the list
    names(return_list) = c("ImportedData","ClusterPhenotypes","ClusterAbundances","Raw")
  }else{
    #Add the group name to the import object...will need for scatterplots
    return_list = list(import_dat,cluster.phenotypes,cluster.abundances,files[["raw"]],grp_name)
    #Change the names of the list
    names(return_list) = c("ImportedData","ClusterPhenotypes","ClusterAbundances","Raw","GroupNames")
  }
  #Return the data
  return(return_list)
}




ImportDataMaster = function(excel,experiment_name,scatterplot_import=TRUE,save_new=FALSE,save_as=NULL,remove=NULL,remove_marker_string=NULL,pheno_cols=NULL){
  #Function for combining SetupTable and Import SpadevizR functions for easy importing to R
  
  #Import data using SetupTable function
  list_of_files = SetupTable(excel,save_new,save_as,remove,remove_marker_string)
  #Import data to spadevizR
  return_list = ImportSpadeVizR(list_of_files,scatterplot_import,pheno_cols)
  #Add the experiment name to this list
  return_list[["ExpName"]] = experiment_name
  #Return the data
  return(return_list)
}




MultipleImportDataMaster = function(Excel_list,experiment_names,scatterplot_import=TRUE,save_new=FALSE,
                                        save_as=NULL,remove=NULL,remove_marker_string=NULL,pheno_cols=NULL){
  #Function for combining the data import process
  
  #Import each excel file using the ImportDataMaster function
  results_list = list()
  for (i in 1:length(Excel_list)){
    #Read the file and add to the results list
    results_list[[experiment_names[i]]] = ImportDataMaster(Excel_list[[i]],experiment_names[[i]],scatterplot_import,
                                                           save_new,save_as,remove,pheno_cols)

  }
  #Return the data list
  return(results_list)
}









#
