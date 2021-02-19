#Functions for Running LDA
#Joshua Hess
#Check for missing packages and install if needed
list.of.packages <- c("caret", "doParallel","MLmetrics","varhandle","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(caret)
require(doParallel)
require(MLmetrics)
require(varhandle)
require(openxlsx)
require(sjmisc)
require(rlist)
#Import custom function
source("ReadData.R")



#Linear Discriminant Analysis function
RunLDA = function(input,cv_type,export_coeff,num_cores){
  #Function for running LDA after reading data. If not using ReadData.R function, the dataframe must be compatible with the
  #given format
  
  #input: ReadData.R object or a compatible dataframe
  #cv_type: "bootstrap" or "repeated"...bootstrap cross validation or repeated
  #export_coeff: TRUE or FALSE. Export excel sheet with LDA coefficients?
  #num_cores: Ex: 20...Number of cores to use for calculations
  
  #Check to see if getting data from ReadData Function
  if(class(input) == "list"){
    print("Detected a list of data files")
    #Get the cluster data and combine it with the group data
    input = as.data.frame(cbind(input[["Clusters"]],input[["Group"]]))
    #Fix the column names for the group column
    colnames(input)[ncol(input)] = "Group"
  }
  
  #Run parallel for speed
  registerDoParallel(cores = num_cores)
  
  #Set up 'Group' column of data to be a factor
  input$Group = as.factor(input$Group)
  levels(input$Group) <- make.names(levels(factor(input$Group)))
  input[,-ncol(input)] = sapply(input[,-ncol(input)], as.numeric)
  
  #Set up cross validation parameters - bootstrap CV or repeated CV
  if (cv_type == "bootstrap"){
    custom <- caret::trainControl(method = "boot",
                                  number = 1000,
                                  p = 0.75,
                                  verboseIter = TRUE, 
                                  summaryFunction = multiClassSummary,
                                  savePredictions="final",returnData = TRUE,returnResamp = "all")
  } else if (cv_type == "repeated"){
    custom <- caret::trainControl(method = "repeatedcv",
                                  number = 4,
                                  repeats = 5,
                                  verboseIter = TRUE, 
                                  summaryFunction = multiClassSummary,
                                  savePredictions="final",returnData = TRUE,returnResamp = "all")
  }
  
  #Run lda
  set.seed(433)
  LDA <- caret::train(Group ~ . ,
                      input,
                      method = 'lda',
                      trControl = custom)

  #Extract LDA coefficients
  CoefficientsLDA <- as.data.frame(LDA$finalModel$scaling)
  #Replace the column names
  rownames(CoefficientsLDA) = stringr::str_replace_all(rownames(CoefficientsLDA),
                                                      "[`]", "")
  colnames(CoefficientsLDA)[1] = "Coefficients of Linear Discriminants"
  #Export the coefficients
  if (export_coeff){
    write.xlsx(CoefficientsLDA, "Coeff_LDA.xlsx", row.names = TRUE)
  }
  #Return the LDA object with coefficients for ease
  LDA_list = list(LDA,CoefficientsLDA)
  #Change the names
  names(LDA_list) = c("LDA","CoefficientsLDA")
  #Return
  return(LDA_list)
}


#Function for iterative classification
RunIterativeLDA = function(input,cv_type,export_results,num_cores){
  #Function for running LDA over each cluster in the dataset. Input must be a ReadData.R object or a
  #dataframe compatible with the code.
  
  #input: ReadData.R object or dataframe
  #cv_type: "bootstrap" or "repeated"...bootstrap cross validation or repeated
  #export_results: TRUE or FALSE? Export all the results as an excel sheet?
  #num_cores: Ex: 20...Number of cores to use for calculations
  
  #Check to see if getting data from ReadData Function
  if(class(input) == "list"){
    print("Detected a list of data files")
    #Change column names to be cluster appended
    colnames(input[["Clusters"]]) = paste("Cluster",colnames(input[["Clusters"]]))
    
    #Convert columns to numeric
    for (i in 1:ncol(input[["Clusters"]])){
      input[["Clusters"]][i]= sapply(input[["Clusters"]][i],as.numeric)
    }

    #Get the cluster data and combine it with the group data
    input = as.data.frame(cbind(input[["Clusters"]],input[["Group"]]))
    #Fix the column names for the group column
    colnames(input)[ncol(input)] = "Group"
    #Get the number of columns
    num_cols = ncol(input)-1
    #Get the names of the clusters
    nms = colnames(input)[1:num_cols]
  }else{
    #Get the number of columns
    num_cols = ncol(input)-1
    #Get the names of the clusters
    nms = colnames(input)[1:num_cols]
  }

  #Loop for running all of the models that you indicated
  result_list = list()
  #Create a counter
  t = 1
  #Create list of working items
  works =  c()
  #Iterate
  for (k in 1:num_cols){

    #Obtain only the combinations from the data in the list element
    tmp_subset = as.data.frame(input[,k])
    colnames(tmp_subset) = colnames(input)[k]
    tmp_subset = cbind(tmp_subset,input[["Group"]])
    #Fix the column names for the group column
    colnames(tmp_subset)[ncol(tmp_subset)] = "Group"
    
    #Print Updates
    print(paste("Running LDA for",colnames(tmp_subset)[1]))
    print(paste('Cluster ',k,"/",num_cols,sep=''))
    
    #Try to run LDA (if does not converge, model will not be added to the list of results)
    try_list = try(
        {
      #Run the LDA function
      tmp_model = RunLDA(input = tmp_subset, cv_type, export_coeff=FALSE,num_cores)
      }
    ,silent=TRUE
    )
    #Check if this cluster produces an error
    if (!sjmisc::str_contains(try_list[[1]],"Error")){
      #Add the list_per_range to the result_list so we can store all ranges in a single structure
      result_list[[t]]= tmp_model
      #Add the index to the list
      works[t] = k 
      #Update the index
      t = t +1
    }
  }

  #Get the names for your list object
  names(result_list) = nms[works]
  
  return_list = list()
  print('Concatenating Results...')
  for (j in 1:length(result_list)){

    #Extract hold out statistics from Confusion Matrix
    tmp_confusion = confusionMatrix(result_list[[j]][["LDA"]]$pred$pred,
                                    result_list[[j]][["LDA"]]$pred$obs,mode = "everything",
                                    positive = result_list[[j]][["LDA"]]$levels[2])
    tmp_confusion_frame = cbind(as.data.frame(t(tmp_confusion$overall)),as.data.frame(t(tmp_confusion$byClass)))
    colnames(tmp_confusion_frame) = paste("Resamples_",colnames(tmp_confusion_frame),sep = "")
    
    #Add the best model to the dataframe
    tmp_confusion_frame$Index = paste("result_list","[[",j,"]][[LDA]]","$bestTune",sep = "")
    tmp_confusion_frame$Model = paste(as.character(result_list[[j]][["LDA"]]$coefnames),collapse = "")
    tmp_confusion_frame$Model = gsub("`", "", tmp_confusion_frame$Model)
    
    #Ensure that bestTune is the model we chose!!!
    tmp_confusion_frame$bestTune_validation = paste(result_list[[j]][["LDA"]]$bestTune,collapse = " , ")
    
    #Get the coefficient from the LDA model
    #tmp_confusion_frame$Coefficient = result_list[[j]][["LDA"]]$finalModel$scaling
    
    #Combine resampled stats with the full stats model
    final_results_frame = as.data.frame(tmp_confusion_frame)
    
    #add the model to the list
    return_list[[j]] = final_results_frame
  }
  return_table = do.call(rbind,return_list)
  print('Finished Combining Results')
  if (export_results){
    write.xlsx(return_table,'Individual_LDA_results.xlsx')
  }
  #Return the table 
  return(return_table)
}




#Function for filtering the variables input to LDA based on accuracy
FilterIterativeLDA = function(input,return_table,top_n){
  #Function for filter results from RunIterativeLDA function and extracting
  #top n variables to keep based on classification accuracy
  
  #input: ReadData.R object or dataframe
  #return_table: returned object from RunIterativeLDA
  #top_n: keep top n performers based on accuracy of classification
  
  #Change column names to be cluster appended
  colnames(input[["Clusters"]]) = paste("Cluster",colnames(input[["Clusters"]]))
  
  #Convert columns to numeric
  for (i in 1:ncol(input[["Clusters"]])){
    input[["Clusters"]][i]= sapply(input[["Clusters"]][i],as.numeric)
  }
  
  #Filter the return table
  filt = return_table %>%
    arrange(desc(Resamples_Accuracy))%>%
    dplyr::select(contains("Model"))%>%
    #Take top n rows ordered by Model
    slice(1:top_n)
  #Get names of the variables in top_n accuracy
  filt_names = c(filt$Model,"Group")
  #Filter the input to contain only the variables in top_n
  if(class(input) == "list"){
    #ReadData.R object
    input[["Clusters"]] = input[["Clusters"]][,which(colnames(input[["Clusters"]]) %in% filt_names)]
  }else{
    #If dataframe
    input = input[,which(colnames(input) %in% filt_names)]
  }
  #Return the new input object
  return(input)
}



#Function for assessing model permutations
PermutativeLDA = function(input,cv_type,max_size,export_results,num_cores){
  #Function for running through all combinations of variables and running LDA
  
  #input: ReadData.R object or dataframe
  #cv_type: "bootstrap" or "repeated"...bootstrap cross validation or repeated
  #max_size: Maximum number of variables per calculation (more = more run time)
  #export_results: TRUE or FALSE...export a data table with results
  #num_cores: Ex: 20...Number of cores to use for calculations
  
  #Check to see if getting data from ReadData Function
  if(class(input) == "list"){
    print("Detected a list of data files")
    #Get the cluster data and combine it with the group data
    input = as.data.frame(cbind(input[["Clusters"]],input[["Group"]]))
    #Fix the column names for the group column
    colnames(input)[ncol(input)] = "Group"
  }
  
  #Form a list containing the set of combinations (-those colnames="Group")
  comb_list = lapply(seq_len(max_size), 
                     FUN = function(x)combn(colnames(input)[-which(colnames(input) %in% c("Group"))], x))
  #Get the size of the combination list
  size = 0
  for (i in 1:length(comb_list)){
    size = size + ncol(comb_list[[i]])
  }
  
  #Loop for running all of the models indicated in comb_list
  result_list = list()
  for (k in 1:max_size){
    #Obtain only the combinations from the data in the list element
    tmp_subset = as.data.frame(comb_list[[k]])
    #Get a list of the variable names
    nms = c(apply(tmp_subset,2,FUN=function(x)paste(as.character(x),collapse="")))
    #Run the classifiers over each column (why the 2 is there)
    result_list[[k]] = apply(tmp_subset, 2, FUN = function(x){
      #Create a list of names from column of comb_list with "Group" attached
      tmp_nms = append(as.character(x),"Group", after = length(as.character(x)))
      #Create dataframe with only those names as input
      inLDA = input[,which(colnames(input) %in% tmp_nms)]
      #Run the LDA function
      RunLDA(inLDA,cv_type,FALSE,num_cores)
    }
    )
    #Change the names of the list to reflect cell populations
    names(result_list[[k]]) = nms
    #Print update on what size model is being used
    print(paste('Finihsed Models ',k,'/',max_size,sep = ""))
    #Close connections
    closeAllConnections()
  }
  
  #Extract the highest accuracy model by cross validation from each result in result_list
  export_list = list(0)
  t=1
  #j=number of variables in model
  for (j in 1:length(result_list)){
    #k=the index for a particular model used (with j variables)
    for (k in 1:length(result_list[[j]])){
        #Extract hold out statistics from Confusion Matrix
        tmp_confusion = confusionMatrix(result_list[[j]][[k]][["LDA"]]$pred$pred,
                                        result_list[[j]][[k]][["LDA"]]$pred$obs,mode = "everything",
                                        positive = result_list[[j]][[k]][["LDA"]]$levels[2])
        tmp_confusion_frame = cbind(as.data.frame(t(tmp_confusion$overall)),as.data.frame(t(tmp_confusion$byClass)))
        colnames(tmp_confusion_frame) = paste("Resamples_",colnames(tmp_confusion_frame),sep = "")
        
        #Add the best model to the bayes_export_table dataframe
        tmp_confusion_frame$Index = paste("result_list","[[",j,"]]","[[",k,"]]","[[LDA]]","$bestTune",sep = "")
        tmp_confusion_frame$Model = paste(as.character(result_list[[j]][[k]][["LDA"]]$coefnames),collapse = "")
        #tmp_confusion_frame$Model = gsub("`", "", tmp_confusion_frame$Model)
        
        #Ensure that bestTune is the model we chose!!!
        tmp_confusion_frame$bestTune_validation = paste(result_list[[j]][[k]][["LDA"]]$bestTune,collapse = " , ")
        #Get the coefficient from the LDA model
        tmp_confusion_frame$Coefficient = paste(as.character(result_list[[j]][[k]][["LDA"]]$finalModel$scaling),collapse = " ")
        
        #Combine resampled stats with the full stats model
        results_frame = as.data.frame(tmp_confusion_frame)
        
        #Take only the first model to keep resuts clean as a table (will be the same model chosen by caret 'bestTune')
        export_list[[t]] = results_frame[1,]
        #Update the index
        t = t+1
    }
  }
  #Combine the list of results into a dataframe
  export_table = do.call(rbind,export_list)
  
  #Logical to export the results talbe
  if (export_results){
    write.xlsx(export_table,paste("LDA_results_perm.xlsx"))
  }
  #Return the result_list so that you can extract any model you wish
  fin_list = list(result_list,export_table,input)
  #Change the names
  names(fin_list) = c("ResultsList","ResultsTable","DataUsed")
  return(fin_list)
}


#Function for plotting projection for each LDA model from PermutativeLDA
ProjectionsPermutativeLDA = function(input, og_dat = NULL, new_groups = NULL){
  #Function for plotting projection for each LDA model from PermutativeLDA
  
  #input: Object return by PermutativeLDA object
  #og_dat: Original ReadData.R object
  #new_groups: new groups for LDA projection (same as ReadData.R)
  
  #Check for new classes
  if (!is.null(og_dat)){
    if (!is.null(new_groups)){
      #Change the value of the data object to take new classes
      og_dat[["Args"]][["groups"]] = new_groups
    }
    #Change the groups arguments in ReadData.R Object
    read_new = do.call(ReadData,og_dat[["Args"]])
    #Change column names to be cluster appended
    colnames(read_new[["Clusters"]]) = paste("Cluster",colnames(read_new[["Clusters"]]))
    
    #Convert columns to numeric
    for (i in 1:ncol(read_new[["Clusters"]])){
      read_new[["Clusters"]][i] = sapply(read_new[["Clusters"]][i],as.numeric)
    }
    
    #Get the cluster data and combine it with the group data
    in_dat = as.data.frame(cbind(read_new[["Clusters"]],read_new[["Group"]]))
    #Change column names
    colnames(in_dat)[ncol(in_dat)] = "Group"
    #Mutate data
    # input[,-ncol(input)] = sapply(input[,-ncol(input)], as.numeric)
    
  }else{
    #Set the in_dat as the DataUsed in the input
    in_dat = input[["DataUsed"]]
  }
  
  #Get the current working directory
  home_dir = getwd()
  #Create a new directory to store the results
  dir.create("LDA Projections", showWarnings = FALSE)
  #Move to the new directory
  setwd("LDA Projections")
  #Extract the predictions and plots for all models
  table_stats = list()
  #Set index
  index = 1
  #Loop
  for (i in 1:length(input[["ResultsList"]])){
    for (j in 1:length(input[["ResultsList"]][[i]])){
        
        #Extract the model
        mod = input[["ResultsList"]][[i]][[j]][["LDA"]]$finalModel
        #Get the cluster names in this model
        clusters = gsub("`","",mod[["xNames"]])
        #Create an object that combines cluster names with "Group" column name
        cols = c(clusters,"Group")
        #Insert the predictors that were used for this model for all test subjects
        mod_dat = in_dat[,which(colnames(in_dat)%in%cols)]
        
        #Get only the cluster data for input to the LDA predictions
        pred_dat = as.data.frame(mod_dat[,which(names(mod_dat)%in%clusters)])
        pred_dat = as.data.frame(sapply(pred_dat, as.numeric))
        
        #Reset the column names (If the #cols=1, then R drops the column names)
        if (ncol(pred_dat)<2){
          colnames(pred_dat) = paste("`",clusters,"`",sep = "")
        }else{
          colnames(pred_dat) = paste("`",colnames(pred_dat),"`",sep = "")
        }
        
        #Predict the dataset on the LDA model
        plda <- predict(object = mod,
                        newdata = pred_dat)
        
        #Extract the position for the LD1 for this group
        dataset = data.frame(Group = mod_dat[,"Group"], lda = plda$x,Y = 0)
        #Set up 'Group' column of data to be a factor
        dataset$Group = as.factor(dataset$Group)
        levels(dataset$Group) <- make.names(levels(factor(dataset$Group)))
        
        #Get the group stats for the LD1 position
        stats = dataset %>%
          group_by(Group) %>%
          summarise(Mean=mean(LD1), SD=sd(LD1))
        stats = as.data.frame(stats)
        stats$Model[length(unique(stats[["Group"]]))] = paste(mod[["xNames"]],collapse = "")
        
        #Add stats to the list
        table_stats[[index]] = stats
        
        #Draw density estimate
        ggplot(dataset, aes(x = LD1))+
          labs(y = paste("Denisity Estimate", sep=""))+
          xlim(-15,15)+
          geom_density(aes(fill = Group), alpha=0.75)+
          theme_bw()
        ggsave(paste(paste(clusters,collapse = " "),".jpeg",sep = ""))
        
        #Move the index up
        index = index +1
      }
  }
  #Combine all of the LD statistics into one sheet
  table_stats = do.call(rbind,table_stats)
  #Change back to the home directory
  setwd(home_dir)
  #Return the results
  return(table_stats)
}






















#
