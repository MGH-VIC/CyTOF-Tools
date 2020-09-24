#flowSOM analysis
#Joshua Hess

library(openCyto)
library(FlowSOM)
library(flowCore)
library(dplyr)
library(ggplot2)
library(stringr)
library(pheatmap)
library(tibble)
library(doParallel)
library(openxlsx)
library(utils)
library(rowr)
library(stats)
library(multcompView)
library(tools)
library(tibble)
library(openxlsx)
source("clustering_data_import.R")


#Set your working directory as the file that stores your data
setwd("C:/Users/Ruxandra Sirbulescu/Desktop/Trauma-live-cells/...")

#Input name for analysis result (Can help to add dates on the end)
analysis_name = "20190710"

#Get names of the csv files in your directory (Change this to "*.fcs" for fcs files)
files_list = list.files(pattern="*.fcs")

#Read the full files in your dataset (all files in your files_list)
temp_data = lapply(files_list,ReadFiles)
full_data = do.call(rbind,temp_data)

#Get normalization parameters (percentiles for each parameter that you input for the full dataset)
norm_parameter = GetNormParameters(full_data,percentile = 0.995, cols_to_use = c(3,18:52,60))

#Filter your data to the percentile that you choose (Will throw out the top percentile)
#Think about whether you want to do this for the full, combined datasets, or for each individual dataset
filtered_input = FilterData(data_to_filter = full_data,percentile = 0.995,cols_to_use = c(3,18:52,60))

#subsample the full data
subsampled_data = DataSubsample(filtered_input,perc_of_total = 0.1)

#Run the data normalization function for the samples
#!Must have cols_to_use the same as for get.normalize function!
#Will normalize your data to the percentile that you chose for get.normalize function
#normed_input = data.normalize(mydata = subsampled_data,norm_input = norm_parameter, cols_to_use = c(5:6))

#Convert data to flow frame so we can input to flowSOM
data_fsom = flowCore::flowFrame(as.matrix(subsampled_data))

#Transform data and Run flowSOM
#Here you will want to choose xdim and ydim as the same. The function only takes in square grids. You choose final number of clusters in the 
#metaclustering step
asinh_cofactor = 5
input_data <- FlowSOM::ReadInput(data_fsom, transform = TRUE, scale = TRUE,compensate = TRUE,
                                 toTransform = c(3,18:52,60),
                                 transformFunction = function(data_fsom){
                                   data_fsom = asinh(data_fsom/asinh_cofactor)
                                 })
set.seed(123121)
fsom <- FlowSOM::BuildSOM(input_data, colsToUse = c(3,18:52,60),
                          xdim=9,ydim=9)

#Build Minimum spanning tree
MST_fsom = FlowSOM::BuildMST(fsom = fsom)
#Perform metaclustering for final clusters
#Options are metaClustering_consensus,metaClustering_hclust, metaClustering_kmeans,metaClustering_som
#nClus gives you the final clustering
metaclus = FlowSOM::MetaClustering(MST_fsom$map$codes,
                         "metaClustering_kmeans",nClus = 63)

#Append Cell data to include a column for Cluster Assignments
meta_assign <- metaclus[MST_fsom$map$mapping[,1]]
Cell_data = as.data.frame(cbind(fsom$data, fsom$map$mapping[,1],as.data.frame(meta_assign)))
colnames(Cell_data)[(ncol(Cell_data)-1):ncol(Cell_data)] = c("Cluster","MetaCluster")

#Extract Median Marker Intensity for Each cluster
Med_intensity = as.data.frame(fsom$map$medianValues)
rownames(Med_intensity) = paste("Cluster", seq(1:nrow(Med_intensity)))

#Plot heatmap of median marker intensity per cluster
jpeg(paste("Median Intensity Heatmap_",analysis_name,".jpeg", sep = ""), width = 2000, height = 3250, res = 180)
pheatmap(Med_intensity[,c(3,18:52,60)],
         cluster_cols = FALSE,
         cluster_rows = TRUE,
         clustering_distance_rows = "euclidean",
         clustering_method = "ward.D2",
         cellwidth = 15,
         cellheight = 15)
dev.off()

#Plot heatmap of median marker intensity per metacluster
med_meta = aggregate(Cell_data[,c(3,18:52,60)], list(Cell_data$MetaCluster), median)
med_meta = column_to_rownames(med_meta, var = "Group.1")
rownames(med_meta)=paste("Meta Cluster",rownames(med_meta),sep = " ")

jpeg(paste("Median Meta Intensity Heatmap_",analysis_name,".jpeg", sep = ""), width = 2000, height = 3250, res = 180)
pheatmap(med_meta[,1:ncol(med_meta)],
         cluster_cols = FALSE,
         cluster_rows = TRUE,
         clustering_distance_rows = "euclidean",
         clustering_method = "ward.D2",
         cellwidth = 15,
         cellheight = 15)
dev.off()

#Scale each channel for each cell as z-score
scaled_dat = Cell_data
scaled_dat[c(3,18:52,60)] <- lapply(Cell_data[c(3,18:52,60)], function(x) c(scale(x)))
#Aggregate "scaled_dat" columns x:y, by scaled_dat$Cluster, function is median
heatmap_input_z = aggregate(scaled_dat[, c(3,18:52,60)], list(scaled_dat$Cluster), median) 
heatmap_input_z = column_to_rownames(heatmap_input_z, var = "Group.1")
rownames(heatmap_input_z)=paste("Cluster",rownames(heatmap_input_z),sep = " ")

#Plot the median of the z-scored data
jpeg(paste("z_score Median Intensity Heatmap_",analysis_name,".jpeg", sep = ""), width = 2000, height = 3250, res = 180)
pheatmap(heatmap_input_z,
         cluster_cols = FALSE,
         cluster_rows = TRUE,
         clustering_distance_rows = "euclidean",
         clustering_method = "ward.D2",
         cellwidth = 15,
         cellheight = 15)
dev.off()

#Now plot the heatmap z scored for the metaclusters
z_inp = aggregate(scaled_dat[, c(3,18:52,60)], list(scaled_dat$MetaCluster), median)
z_inp = column_to_rownames(z_inp, var = "Group.1")
rownames(z_inp)=paste("Meta Cluster",rownames(z_inp),sep = " ")

#Plot the median of the z-scored data
jpeg(paste("z_score Median Meta Intensity Heatmap_",analysis_name,".jpeg", sep = ""), width = 2000, height = 3250, res = 180)
pheatmap(z_inp,
         cluster_cols = FALSE,
         cluster_rows = TRUE,
         clustering_distance_rows = "euclidean",
         clustering_method = "ward.D2",
         cellwidth = 15,
         cellheight = 15)
dev.off()

#Export Results for all data
# Create a blank workbook
OUT <- createWorkbook()

# Add sheets to the workbook
addWorksheet(OUT, "Cell Data")
#addWorksheet(OUT, "Scaled Cell Data")
addWorksheet(OUT, "Median Cell Data_Clusters")
addWorksheet(OUT, "Median Cell Data_Metaclusters")
#addWorksheet(OUT, "Median scaled Data_Clusters")
#addWorksheet(OUT, "Median scaled Data_Metaclusters")

# Write the data to the sheets
writeData(OUT, sheet = "Cell Data", x = Cell_data,rowNames = TRUE)
#writeData(OUT, sheet = "Scaled Cell Data", x = scaled_dat,rowNames = TRUE)
writeData(OUT, sheet = "Median Cell Data_Clusters", x = Med_intensity,rowNames = TRUE)
writeData(OUT, sheet = "Median Cell Data_Metaclusters", x = med_meta,rowNames = TRUE)
#writeData(OUT, sheet = "Median scaled Data_Clusters", x = heatmap_input_z,rowNames = TRUE)
#writeData(OUT, sheet = "Median scaled Data_Metaclusters", x = z_inp,rowNames = TRUE)

# Export the file
saveWorkbook(OUT, "flowSOM_summary.xlsx")


#Plot the clustered data
# jpeg(paste("MST_16clusters_",analysis_name,".jpeg", sep = ""), width = 2000, height = 2000, res = 180)
# PlotNumbers(MST_fsom)
# dev.off()

#Plot results
# jpeg(paste("MST_16clusters_color_",analysis_name,".jpeg", sep = ""), width = 2000, height = 2000, res = 180)
# PlotStars(MST_fsom)
# dev.off()

