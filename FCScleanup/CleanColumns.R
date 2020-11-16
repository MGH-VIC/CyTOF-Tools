#Clean up column names of FCS files to make them match
#Joshua Hess

# Load packages
library(openxlsx)
library(flowCore)
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Biobase")
library(Biobase)


# Set working directory
#setwd("Z:/HDI-004-MP-Kidney Tranplant/Experiments/20191205/FCS-renamed")
#setwd("Z:/HDI-004-MP-Kidney Tranplant/Experiments/20191205/FCS - Dec 2019") 
setwd("Z:/HDI-004-MP-Kidney Tranplant/Experiments/20190221/FCS")
PrimaryDirectory <- getwd()

# Find file names of original csv files in the current working directory
FileNames <- list.files(path=PrimaryDirectory, pattern = ".fcs$")

#Read a single file and export the names
data = read.FCS(FileNames[1], transformation = NULL)
#Get the names of the file
#old_names = as.data.frame(data@parameters@data$desc)
#nas = as.data.frame(data@parameters@data$name)
#Get the NA values
#old_names[is.na(old_names)] = nas[is.na(old_names)]
#Change the column name
#colnames(old_names) = c("Feb")
#Write out the old names to file
#write.csv(old_names,"Z:/HDI-004-MP-Kidney Tranplant/Experiments/Study Analysis of 20191205 and 20190221 From Dec 2019 Onwards/CytoNorm/File Labels - Feb.csv")

#Read the new column names for the fcs files
names = read.csv("Z:/HDI-004-MP-Kidney Tranplant/Experiments/Study Analysis of 20191205 and 20190221 From Dec 2019 Onwards/CytoNorm/File Labels - Feb.csv")
names = names[["Feb"]]
#Set output directory
#outDir = "Z:/HDI-004-MP-Kidney Tranplant/Experiments/20191205/FCS-renamed"
outDir = "Z:/HDI-004-MP-Kidney Tranplant/Experiments/20190221/FCS-renamed"

#Iterate through files and change names to the appropriate format
for(i in 1:length(FileNames)){

  #Read the data
  data = read.FCS(FileNames[i], transformation = NULL)
    
  # Convert data to matrix
  data <- as.matrix(data@exprs)
    
  # Create FCS file metadata - column names with descriptions
  metadata <- data.frame(name=dimnames(data)[[2]],
                           desc=names)
    
  # Create FCS file metadata - ranges, min, and max settings
  metadata$range <- apply(apply(data,2,range),2,diff)
  metadata$minRange <- apply(data,2,min)
  metadata$maxRange <- apply(data,2,max)
    
  # Create flowframe 
  data.ff <- new("flowFrame",
                   exprs=data,
                   parameters=AnnotatedDataFrame(metadata)
  )
    
  # Save flowframe as .fcs file -- save data (with new tSNE parameters) as FCS
  write.FCS(data.ff, paste(outDir,"/",gsub(".fcs", "-renamed", FileNames[i]), ".fcs",sep=""))
  
}



