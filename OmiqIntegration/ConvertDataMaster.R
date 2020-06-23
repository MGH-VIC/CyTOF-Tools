#Module for converting data from Omiq to SpadeVizR format

#Load custom module
source("ConvertData.R")

#Input directory to your csv files
csv_dir = "Z:/Raju Paul/Q vax Human/Normalization/CytoNorm/Omiq Analysis/06042020/CSV FlowSOM Export/G1,G5 154,IFNg CSV"
#Input how you want to concatenate files (by cluster -- header in csvs)
concat_by ="fsom-metaclust-k=20"
#Input resulting excel file name and path
out_name = "Z:/Raju Paul/Q vax Human/Normalization/CytoNorm/Omiq Analysis/06042020/CSV FlowSOM Export/SpadeVizR/Grp1Grp5flowSOM.xlsx"

#Convert the data from Omiq clustering
ConvertOmiqClusterCSVs(csv_dir,concat_by,out_name)