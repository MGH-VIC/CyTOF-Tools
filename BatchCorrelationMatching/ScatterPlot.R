#Cluster Scatterplot script
#Joshua Hess


#Check for missing packages and install if needed
# list.of.packages <- c("devtools","Rcpp","biclust","diptest","evtree","ggdendro","ggfortify","ggplot2","gplots","gdata","ggrepel",
#                       "ggRandomForests","gridExtra","gtable","gtools","igraph","MASS","packcircles","plyr","randomForestSRC",
#                       "reshape2","pheatmap","readxl","raster","openxlsx","bindrcpp","stringi","statmod")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
#Check for github packages
# require('devtools')
# list_git = c("tchitchek-lab/SPADEVizR")
# new.packages <- list_git[!(list_git %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install_github(new.packages)
# #Check for Biomanager packages
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# list_bio = c("FlowSOM","flowCore","edgeR")
# new.packages <- list_bio[!(list_bio %in% installed.packages()[,"Package"])]
# if(length(new.packages)){
#   for (pack in new.packages){
#     biocLite("flowCore", suppressUpdates = TRUE)
#   }
# }


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
require(stringr)
require(tidyverse)

#Import custom modules
source("utils.R") #Sources utils function for phenoviewer_modified


ScatterPlot = function(ImportDataMaster_list,out_dir){
  #Function for generating scatterplots from ImportDataMaster object
  #Note: The group assignment must be filled out before this step!
  
  #Get the primary directory
  PrimaryDirectory = getwd()
  
  #copy the datasheet from SpadvizR import object
  data = ImportDataMaster_list[["ClusterAbundances"]]
  #Change the cluster names to include a dash
  rownames(data) = paste(gsub(" ","_",rownames(data)))
  #Get the column sums for cell counts per sample
  sum_counts_sample <- colSums(data)
  #Create relative cell counts
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      data[i,j] = data[i,j]/sum_counts_sample[j]*100
    }
  }
  
  #transpose the data for ploting
  data <- t(data)
  data <- as.data.frame(data)
  
  #group assignment
  group_data <- openxlsx::read.xlsx(ImportDataMaster_list[["GroupNames"]])
  #Add the group assignment to the data object
  ImportDataMaster_list[["GroupAssignment"]] = group_data
  group_data$sample <- trim(group_data$sample)
  group_data$sample = gsub(" ", ".", group_data$sample, fixed = TRUE)
  #Add group column to the datasheet
  data$Group <- group_data$Group#[match(rownames(data), group_data$sample)]
  #Order the rows by alphabetical group name
  data  = data[order(data$Group),]
  #Reorder the sheet to put the group column first
  data <- data[, c(ncol(data), 1:(ncol(data)-1))]
  
  dir.create(out_dir, showWarnings = FALSE)
  setwd(out_dir)
  x_order = factor(data$Group, levels = unique(data$Group),ordered=TRUE)
  for(i in 2:ncol(data)){
    scatter_plot <- 
      ggplot(data, aes_string(x = x_order, fill = "Group", y = colnames(data)[i]))+ 
      geom_dotplot(binaxis = "y", stackdir = "centerwhole") +
      stat_summary(fun.y = "median", size=0.5, geom = 'line', aes(group=1))+
      stat_summary(
        fun.ymin = function(z) { quantile(z,0.25) },
        fun.ymax = function(z) { quantile(z,0.75) },
        fun.y = median,
        width = 0.2,
        geom = "errorbar") + 
      theme(axis.text.x = element_text(size = 25, face = "bold", vjust = 1.0, hjust = 1.0, angle = 45)) +
      theme(axis.text.y = element_text(size = 20, face = "bold", vjust = 0.5, hjust = 0.5, angle = 0)) +
      theme(legend.position = "none")
    ggsave(scatter_plot,
           width = 20,
           height = 15,
           dpi = 300,
           filename = paste(gsub("_"," ",colnames(data)[i]), ".jpeg", sep = ""))
  }
  setwd(PrimaryDirectory)
  #Add the output directory to the list
  ImportDataMaster_list[["ScatterPlotFiles"]] = out_dir
  #Return the directory of the output
  return(ImportDataMaster_list)
}







#