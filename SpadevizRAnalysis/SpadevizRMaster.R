#Example script for cluster matching with correlation
#Joshua Hess

#Import custom modules
source("ImportData.R")
source("ParallelCoordinatePlot.R")
source("ScatterPlot.R")
source("SpadevizRAnalysis.R")
source("PhenoPPT.R")

#Change directories to Results folder
home_dir = getwd()
setwd("../Results/")

#Get data for group1 (scatterplot import will require a filled out sheet!)
grp1 = ImportDataMaster(excel = "Flugen Pilot 25K Cells K50 Group Stats 20200212.xlsx",
                        scatterplot_import=TRUE,save_new=FALSE,save_as=NULL,pheno_cols=c(1:10,12:49),
                        experiment_name = "Pilot",remove_marker_string = c("_",":"),
                        remove=c("export_191218_1_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191218_6_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191218_11_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191218_16_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191220_21_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191218_31_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191220_26_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191220_32_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191220_33_00_cells_found_1_CD45+ CD66b- Leukocytes",
                                 "export_191220_34_00_cells_found_1_CD45+ CD66b- Leukocytes"))

#Export parallel coordinate plots
grp1 = ParallelCoordinatePlots(grp1,out_dir="PCP")
#Export scatterplots
grp1 = ScatterPlot(grp1,out_dir="ScatterPlots")
#Create a powerpoint with phenotypes
GeneratePPT(grp1,"ClusterProfilesPPT.pptx")

#Create directory for spadevizR analysis
dir.create("SpadevizR Analysis and Volcano Plots", showWarnings = FALSE)
setwd("SpadevizR Analysis and Volcano Plots")
#Identify differentially abundant clusters
grp1 = IterativeDAC(grp1)


