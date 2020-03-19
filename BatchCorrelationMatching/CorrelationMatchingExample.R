#Example script for cluster matching with correlation
#Joshua Hess

#Import custom modules
source("ImportData.R")
source("ParallelCoordinatePlot.R")
source("ScatterPlot.R")
source("CorrelationClusterMatching.R")

#Get data for group1 (scatterplot import will require a filled out sheet!)
grp1 = ImportDataMaster(excel = "Grp 1 D10 T Cells 20181204 K=17.xlsx",scatterplot_import=TRUE,save_new=FALSE,save_as=NULL,pheno_cols=NULL)
#Get data for group2 (scatterplot import will require a filled out sheet!)
grp2 = ImportDataMaster(excel = "Grp 2 D10 T Cells 20181204 K=25.xlsx",scatterplot_import=TRUE,save_new=FALSE,save_as=NULL,pheno_cols=NULL)

#Export parallel coordinate plots
grp1 = ParallelCoordinatePlots(grp1,out_dir="Group1PCP")
#Export scatterplots
grp1 = ScatterPlot(grp1,out_dir="Group1Scatter")
#Rescale the data for matching
grp1 = MinMaxRescale(grp1, experiment_name = "Grp1")
#Filter the data with a minimum count
grp1 = FilterSize(grp1, min_size = 2000)

#Export parallel coordinate plots
grp2 = ParallelCoordinatePlots(grp2,out_dir="Group2PCP")
#Export scatterplots
grp2 = ScatterPlot(grp2,out_dir="Group2Scatter")
#Rescale the data for matching
grp2 = MinMaxRescale(grp2, experiment_name = "Grp2")
#Filter the data with a minimum count
grp2 = FilterSize(grp2, min_size = 2000)

#Match clusters across experiments
Corr_test = CorrelationMatching(grp1,grp2,method = "spearman",include_count=FALSE,csv_name="Grp1_Grp2_ClusterCorrelations.csv")
Corr_test = FilterCorrResults(Corr_test,top_n=5,csv_name = "Grp1_Grp2_ClusterCorrelationsFiltered.csv")

#Export the powerpoint for manual matching
GenerateMatchingPPT(Corr_list = Corr_test,ppt_name = "Grp1_Grp2.pptx")




########################
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






