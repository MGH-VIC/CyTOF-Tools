#Trajectory Analysis for Triplets

#Read in Functions for loading dataset
source("ReadData.R")
source("RunTrajectory.R")

#Load Data for SCORPIUS
CD = DataLoad(pattern = "\\.csv$",subsample=TRUE,fraction=0.01,number=NULL,
              by='PatientId',Monocle = FALSE,SCORPIUS = TRUE)
#Run SCORPIUS by Patient
output = RunSCORPIUS(CD = Test,distance="euclidean",dim=3,
                     show_by = "GroupID",export_graph = TRUE,
                     export_heat = TRUE,var_imp = TRUE,threads = 30,
                     num_perm = 10,
                     ntrees = 10000)
#Load Data for SCORPIUS
CD_2 = DataLoad(pattern = "\\.csv$",subsample=TRUE,fraction=0.01,number=NULL,
                by='all',Monocle = FALSE,SCORPIUS = TRUE)
#Run SCORPIUS for all
output2 = RunSCORPIUS(CD = Test,distance="euclidean",dim=3,
                     show_by = "GroupID",export_graph = TRUE,
                     export_heat = TRUE,var_imp = TRUE,threads = 30,
                     num_perm = 10,
                     ntrees = 10000)