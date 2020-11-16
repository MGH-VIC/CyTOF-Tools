#Master script for Correlation Analysis
#Joshua Hess

#Get the functions that you will need
source("ModifiedPheatmap.R")
source('CorrelationAnalysis.R')


#------------------Correlation Analysis----------------------------
#Enter the name of your data sheet
filename="Template.xlsx"
#Enter the name of the sheet of the data sheet you are using for calculations
sheetname = "Sheet1"
#Enter correlation type
corr_type="spearman"
#Do you want to export the p values and correlation coefficient data?
export=TRUE
#If you are exporting, what do you want to call the correlation coefficient sheet?
r_export_name = "Testing R value.xlsx"
#If you are exporting, what do you want to call the p-value sheet?
p_export_name = "Testing P value.xlsx"
#Do you want to add a prefix to cluster names? If not, leave as NULL
cluster_prefix=NULL
#Which columns are you using as cells for calculation?
cell_cols = 20:31
#Which columns are you using as clinical measures? If none, use NULL and calculation will only be run for cells
not_cell_cols = NULL
#Which column indicates the groups you are using? 
group_col = 3
#Which groups are you wanting to use? (Ex. groups = c(1,2) if numbered Ex. groups = c("Vaccinated","Naive") if strings)
groups = c(1,2)
#Run the correlation calculation
corr_results = CorrCalculation(filename,sheetname,group_col,cell_cols,not_cell_cols,corr_type,export,
                               r_export_name,p_export_name,cluster_prefix)



#------------------Optimal Cluster Selection Analysis----------------------------
#What do you want to save the results as? (Must end with ".txt"!)
export_name = "Testing Optimal Clustering.txt"
#Are you inputting a manual matrix? Use NULL if you are using this after running the correlation analysis, it will input the data automatically
#for clusters vs not clusters. If using only clusters, set this value to corr_results[[5]]
corr_matrix=NULL
#What distance metric are you using for clustering?
distance = "euclidean"
#What clustering method?
method = "ward.D2"
#What is the minimum number of clusters that you want to consider?
min_clust = 5
#What is the maximum number of clusters that you want to consider?
max_clust = 10

#Run the clustering calculation
optimal_clust = OptimalClustering(corr_results,export_name,corr_matrix=NULL,distance,method,min_clust,max_clust)





#------------------Create a heatmap from the correlation data----------------------------
#This function will need to be individualized, so you will have to optimize the layout manually. All you do is change filename output and 
#other parameters. You will not need to change the 'optimal_clust[[1]]' parameter. That is just a dataframe
jpeg(paste("D10-D51_correlation_heatmap_9modules.jpeg"),units = "cm",res = 200,width = 50,height = 90)
pheatmap_modified(
  #dplyr::select(input_data_clustering_final, -c("Count", "IFNg", "CD184", "CD14", "IL17a")),
  corr_results[[3]],
    scale = "none",
  color = colorRampPalette(c("#663366", "white", "#990000"))(100),
  border_color = "Black",
  clustering_distance_rows = "euclidean",
  cluster_cols = F,
  clustering_method = "ward.D2",
  cellwidth = 25,
  cellheight = 10,
  treeheight_row = 200,
  treeheight_col = 50,
  fontsize = 9.5,
  cutree_rows = 2.5,
  display_numbers = F,
  number_color = "white"
  #width = 800
)
dev.off()



#