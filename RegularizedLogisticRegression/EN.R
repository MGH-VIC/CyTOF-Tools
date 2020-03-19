#Functions for Running Regularized Logistic Regression
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
#Import custom function
source("ReadData.R")


#Logistic Elastic Net Function
RunEN = function(input,cv_type,export_coeff,export_coeff_img,export_cv,export_cv_img,num_cores,top_n=NULL){
  
  #Function for running EN after reading data. If not using ReadData.R function, the dataframe must be compatible with the
  #given format
  
  #input: ReadData.R object or a compatible dataframe
  #cv_type: "bootstrap" or "repeated"...bootstrap cross validation or repeated
  #export_coeff: TRUE or FALSE. Export excel sheet with EN coefficients?
  #export_coeff_img: TRUE or FALSE. Export waterfall plot with EN coefficients?
  #export_cv: TRUE or FALSE. Export excel sheet with cross-validation summary?
  #export_cv: TRUE or FALSE. Export image with cross-validation summary?
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
  input = input %>%
    mutate_at(vars(-Group),funs(c(unfactor(.))))
  
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
                                  repeats = 100,
                                  verboseIter = TRUE, 
                                  summaryFunction = multiClassSummary,
                                  savePredictions="final",returnData = TRUE,returnResamp = "all")
  }
  
  #Run logistic elastic net regression
  set.seed(433)
  ElasticNet <- caret::train(Group ~ . ,
                             input,
                             method = 'glmnet',family = "binomial", standardize = FALSE,
                             tuneGrid = expand.grid(
                               lambda = seq(0, 1,length = 50),
                               alpha = seq(0,1,length = 50)),
                             trControl = custom)
  #Plot cross validation
  if (export_cv_img){
    ggplot(data = ElasticNet) +
      theme(text = element_text(size=17),
            axis.text.x = element_text(face="bold", color="black", size=23, angle=0),
            axis.text.y = element_text(face="bold", color="black", size=23, angle=0),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid = element_line(colour = "grey"),
            plot.margin = unit(c(2,2,2,2), "cm"),
            axis.title.y = element_text(angle = 90, vjust = 10, hjust = 0.5, size = 25),
            axis.title.x = element_text(angle = 0, vjust = -10, hjust = 0.5, size = 25),
            legend.title = element_text(size=19),
            legend.text = element_text(size=19))
    ggsave(paste(levels(input$Group)[1],
                 levels(input$Group)[2],"Cross_valid.jpeg",sep = "_"),
           plot = last_plot(), units = "mm",
           width = 500, height = 300, limitsize = FALSE)
  }
  
  #Export cross validation summary
  if (export_cv){
    cvSummary = as.data.frame(ElasticNet$results)
    write.xlsx(cvSummary, paste(levels(input$Group)[1],
                                levels(input$Group)[2],
                                "Cross_valid_summary.xlsx",sep = "_"))
  }
  
  #Extract log odds coefficients
  bestEN <- ElasticNet$finalModel
  CoefficientsEN = as.matrix(coef(bestEN, s = ElasticNet$bestTune$lambda))
  CoefficientsEN = as.data.frame(CoefficientsEN)
  rownames(CoefficientsEN) = stringr::str_replace_all(rownames(CoefficientsEN),
                                                      "[`]", "")
  colnames(CoefficientsEN)[1] = "Change in Log Odds per Unit Increase"
  if (export_coeff){
    write.xlsx(CoefficientsEN, paste(levels(input$Group)[1],
                                     levels(input$Group)[2],
                                     "Coeff.xlsx",sep = "_"), 
               row.names = TRUE)
    #Remove the intercept
    CoefficientsEN = CoefficientsEN[2:nrow(CoefficientsEN),,drop = FALSE]
    #Check for subsetting
    if (!is.null(top_n)){
      #Filter the coefficients
      CoefficientsEN = FilterCoefficients(CoefficientsEN,top_n)
      write.xlsx(CoefficientsEN, paste(levels(input$Group)[1],
                                       levels(input$Group)[2],
                                       "Coeff_filtered.xlsx",sep = "_"), 
                 row.names = TRUE)
    }
  }

  
  #Set up Coefficients waterfall plot data
  if (export_coeff_img){
    CoefficientsEN = subset(CoefficientsEN, CoefficientsEN$`Change in Log Odds per Unit Increase`!=0)
    CoefficientsEN = CoefficientsEN[order(-CoefficientsEN$`Change in Log Odds per Unit Increase`),
                                    , drop = FALSE]
    
    CoefficientsEN$`Group` <- rownames(CoefficientsEN)  # create new column for car names
    CoefficientsEN$Group <- ifelse(CoefficientsEN$`Change in Log Odds per Unit Increase` < 0,
                                   levels(input$Group)[1],
                                   levels(input$Group)[2])
    
    #Plot coefficients in waterfall plot
    logoddsvalue = CoefficientsEN$`Change in Log Odds per Unit Increase`
    features = rownames(CoefficientsEN)
    features = factor(features, levels = features[order(logoddsvalue)])
    axval = (max(abs(logoddsvalue)) + 0.03)
    
    plot_coef = ggplot(CoefficientsEN, aes(x=features, y=logoddsvalue)) + 
      geom_bar(stat='identity', aes(fill=Group), width=.6)  +
      scale_fill_manual(name="Group", 
                        labels = c(levels(input$Group)[1],levels(input$Group)[2]), 
                        values = c("Black","Grey")
      ) + 
      labs(subtitle="", title= "") +
      theme(text = element_text(size=0.5),                         
            axis.text.x = element_text(face="bold", color="black", size=2.5, angle=0),
            axis.text.y = element_text(face="bold", color="black", size=2.5, angle=0),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black", size = 0.15),
            panel.grid = element_line(colour = "grey", size = 0.1),
            plot.margin = unit(c(2,2,2,2), "mm"),
            axis.title.y = element_text(angle = 90, vjust = 1, hjust = 0.5, size = 4),
            axis.title.x = element_text(angle = 0, vjust = -0.3, hjust = 0.5, size = 4),
            plot.title = element_text(vjust=3, hjust = 0),
            plot.subtitle = element_text(vjust = 3),
            legend.title = element_text(size=2), 
            legend.text = element_text(size=2),
            legend.key.size = unit(0.3,"line"),
            legend.margin = ggplot2::margin(t=-0.5, unit = "mm")) +
      labs(x = "Cell Type", y = expression(paste(Delta, "Log Odds/","Unit Increase"))) + 
      scale_y_continuous(limits = c(-axval, axval)) +
      coord_flip()
    ggsave(paste(levels(input$Group)[1],
                 levels(input$Group)[2],
                 "coeff_plot.jpeg",sep="_"),plot = plot_coef, units = "mm", width = 50, height = 50) 
  }
  #Create a list to return
  return_list = list(ElasticNet,bestEN,CoefficientsEN,input)
  #Change the names
  names(return_list) = c("ElasticNetTrain","BestEN","CoefficientsBestEN","Input")
  #Return
  return(return_list)
}


#Function for filtering the variables input to LDA based on accuracy
FilterCoefficients = function(input,top_n){
  #Function for filter results from RunIterativeLDA function and extracting
  #top n variables to keep based on classification accuracy
  
  #input: ReadData.R object or dataframe
  #return_table: returned object from RunIterativeLDA
  #top_n: keep top n performers based on accuracy of classification
  
  #Set up dataframe to have temporary column (dplyr removes rownames)
  input["tmp"] = rownames(input)
  #Filter the return table
  filt = input %>%
    arrange(desc(abs(`Change in Log Odds per Unit Increase`)))%>%
    #Take top n rows ordered by coefficient
    slice(1:top_n)
  #Change back the rownames
  rownames(filt) = filt[["tmp"]]
  #Remove temporary column
  filt = filt[1]
  #Return the new input object
  return(filt)
}







#