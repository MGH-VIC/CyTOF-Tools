#Single-Cell Trajectory Analysis
#Functions for Reading Data
#Triplets Melanoma
#Joshua Hess

#Check for missing packages and install if needed
source("http://bioconductor.org/biocLite.R")
biocLite()
BioC.packages = c("monocle")
new.BioC.packages <- BioC.packages[!(BioC.packages %in% installed.packages()[,"Package"])]
if(length(new.BioC.packages)>0) biocLite(new.BioC.packages)
list.of.packages <- c("dplyr","openxlsx","magrittr","tools","LaF","tidyr","varhandle","SCORPIUS","matrixStats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)
#Load packages
library(dplyr)
library(matrixStats)
library(openxlsx)
library(magrittr)
library(monocle)
library(tools)
library(LaF)
library(tidyr)
library(varhandle)
library(SCORPIUS)

#Ensure that we are using digits if we need to convert to numeric
options(digits = 20)

#Function for loading the data
DataLoad = function(pattern,subsample=FALSE,fraction=NULL,number=NULL,by='SampleId',
                    Monocle=FALSE, SCORPIUS = TRUE,remove=NULL,drop=NULL){
  #This function will load the data below your current working directory
  #that match your condition. This is an overarching method for loading
  #and compiling it to a single dataframe.
  
  #Note: Set your working directory to a higher directory than your files for
  #easy searching!
  
  #Get list of files that meet your pattern
  root = list.files(path = ".", pattern = pattern,full.names = TRUE,recursive = TRUE)
  
  #Check to see if you are subsampling
  if (subsample){
    print('Detected command for subsampling...')
    #Make sure percentage argument is numeric and between 0 and 1
    if (!is.null(fraction)){
      if (!is.numeric(fraction)){
        stop("'subsample' and 'fraction' arguments were given, but 'fraction' is not numeric")
      }
      if (!(fraction >=0 && fraction <=1)){
        stop("'fraction' argument must be between 0 and 1")
      }
      #Read the subsampling data
      print('Taking percentages of data...')
      data = lapply(root,function(x){
        print(paste("Reading",x,"...",sep = " "))
        tmp = DataSubsample(x,perc = fraction)
        print(paste('Finished reading',x,sep = " "))
        return(tmp)
      })
    }else if (!is.null(number)){
      if (!is.numeric(number)){
        stop("'subsample' and 'number' arguments were given, but 'number' is not numeric")
      }
      #Read the subsampling data
      print('Taking given number of samples in data...')
      data = lapply(root,function(x){
        print(paste("Reading",x,"...",sep = " "))
        tmp = DataSubsample(x,number_size = number)
        print(paste('Finished reading',x,sep = " "))
        return(tmp)
      })
    }else {
      stop("Detected arguments for subsampling for none for 'perc' or 'number'")
    }
  }else{
    #If no subsampling input read the full csv files 
    print('Detected no subsampling...')
    data = lapply(root,function(x){
      print(paste("Reading",x,"...",sep = " "))
      tmp = read.csv(x,header = TRUE)
      tmp = DataClean(x,tmp,remove=remove,drop=drop)
      print(paste('Finished reading',x,sep = " "))
      return(tmp)
    })
  }
  
  #Check to see how the data is getting aggregated
  if (!(by %in% c("SampleId", "GroupId", "PatientId", "all"))){
    stop("'by' argument for aggregation is invalid. Must be 'SampleId', 'GroupId', 'PatientId', or 'all'")
  }else if (by %in% c("all")){
    #Combine all data if by is "all"
    agg_list = list(do.call(rbind,data))
    #Set names for the full list
    fin_names = c("all")
  }else{
    #Get the unique name in each SampleID column
    tmp_names = lapply(data,function(x){
      as.character(unique(x[by]))})
    #Set the names in your data frame
    names(data) = tmp_names
    #Apply a function to group by
    agg_list = lapply(unique(names(data)),function(x){
      idx = data[which(names(data) == x)]
      return(do.call(rbind,idx))
    })
    #Now set the names of your list so that we now how we aggregated
    fin_names = unique(names(data))
  }
  #Do the transformations on the data to get our newCellData Object for Monocle
  print('Combining data and tranforming into a appropriate data object...')
  if (Monocle){
    print('Monocle Formatting...')
    CD = lapply(agg_list,CreateCellDataMonocle)
  }else if (SCORPIUS){
    print('Returning DataFrames for SCORPIUS')
    CD = lapply(agg_list,CreateCellDataSCORPIUS)
  }
  #Change names in our CD object so we know how we aggregated data
  names(CD) = fin_names
  #Return our Cell Data list
  return(CD)
}



CreateCellDataSCORPIUS = function(df){
  #Function for creating data tables for SCORPIUS single-cell trajectory
  pd =  dplyr::select(df,CellId,SampleId,GroupId,PatientId)
  #Remove redundant features in our expression matrix and get the appropriate format
  df = df %>%
    #Remove ID features
    dplyr::select(-CellId,-SampleId,-GroupId,-PatientId)
  #Return a list object
  dat_list = list(df,pd)
  #Rename the list
  names(dat_list) = list("Expression","Info")
  return(dat_list)
}

CreateCellDataMonocle = function(df){
  #This function will create a new Cell data object for input to monocle
  #This function is made to run after the DataClean function
  
  #Create an Annotated Data Frame for ID data
  pd = new("AnnotatedDataFrame", data =  dplyr::select(df,CellId,SampleId,GroupId,PatientId))
  #Remove redundant features in our expression matrix and get the appropriate format
  df = df %>%
    #Remove ID features
    dplyr::select(-CellId,-SampleId,-GroupId,-PatientId)%>%
    #Transpose and create matrix from data
    t()%>%
    as.matrix()
  #Create new CellDataObject
  CD = newCellDataSet(df,phenoData = pd)
  #Return the CellDataObject
  return(CD)
}

DataSubsample = function(file_name,perc=NULL,number_size=NULL){
  #This function will read your csv file and will format the data
  #table according to your input parameters
  
  #Get the size of csv file
  df_size = LaF::determine_nlines(file_name)
  #Get the size of the sample based on user input
  if (!(is.null(perc))){
    sample_size = round((df_size-1)*perc)
  }else if (!(is.null(number_size))){
    #If the numbe size you set is too large, take all samples
    if (number_size > df_size-1){
      number_size = df_size-1
    }
    sample_size = number_size
  }
  #Generate random numbers for lines in csv (cells). Rescict to 2:... to skip header
  set.seed(1432)
  random_lines = sample(c(2:df_size),size = sample_size)
  #Get our random lines
  lines = LaF::get_lines(file_name,random_lines)
  #Get the header line (line number 1)
  header = LaF::get_lines(file_name,1)
  #Concatenate the header and csv lines
  dat_list = lapply(c(header,lines),function(x){
    do.call(rbind, strsplit(x,","))})
  df = as.data.frame(do.call(rbind,dat_list[2:length(dat_list)])) %>%
    magrittr::set_colnames(dat_list[[1]])
  
  #Run the datacleaning function
  df = DataClean(file_name = file_name, df = df)
  #Reutrn the subsampled data frame object
  return(df)
}

DataClean = function(file_name,df,remove = NULL, drop = NULL){
  #This is a function for cleaning your data to get rid of unwanted columns
  #and column names. Modify this for changing up your data names
  
  #Remove the file extension from the csv filename to get sample names
  samp = strsplit(tools::file_path_sans_ext(basename(file_name)),"_")[[1]][1]
  grp = gsub('[[:digit:]]+', '', samp)
  patient = gsub('[[:alpha:]]+', '', samp)
  #Run the data cleaning pipe
  df = df%>%
    #Select only columns that contain the prefix 'Cell'
    dplyr::select(starts_with("Cell")) %>%
    #Remove columns that contain 'Hoechst'
    dplyr::select(-contains("Hoechst")) %>%
    #Remove the sample name from dataframe column names
    dplyr::rename_all(funs(stringr::str_remove_all(.,paste("Cell_",samp,sep = "")))) %>%
    #Remove columns that end with the '_2' string (S100_2,VEGFR_2,SMA_2)
    dplyr::select(-ends_with("_2"))%>%
    #Add in sample name to the dataframe
    mutate(SampleId = samp) %>%
    #Add in group name to the dataframe
    mutate(GroupId = grp)%>%
    #Add in patient ID to the dataframe
    mutate(PatientId = patient)%>%
    #Ensure that the columns are the correct data type
    mutate_if(is.factor,unfactor)%>%
    #Ensure that the CellID column is an int
    mutate_at(vars(CellId),as.integer)
  if (!remove(is.null)){
    df = extract(!names(df) %in% drop)
  }
  #Reurn the cleaned dataframe
  return(df)
}






