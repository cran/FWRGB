#'@title Fresh Weight Determination from the Projected Area of the Plant
#'
#' @description The projected area obtained from the visual image of the plant is used as input to the machine learning model (Linear Model, Artificial Neural Network, and Support Vector Regression) to determine the plant's fresh weight.
#'
#' @param Biomass ground truth fresh biomass of the above ground plant parts for the model development
#' @param PA a numeric value representing projected area (in cm square) of the above ground plant parts from the visual image of the plant
#' @param model the applied machine learning model i.e., "lm" or, "ANN" or,"SVM"
#' @param newPA  out of sample projected area (in cm square) of the above ground plant parts from the visual image of the plant
#'
#' @return Fresh weight in gm
#'
#' @import  "neuralnet"
#'
#' @import  "e1071"
#'
#' @importFrom "stats"  "lm"  "predict"
#'
#' @export
#'
#' @examples
#' y=c(28.3,19.8,13,17.4,13.8,18.5,8.5,19.1)# Ground truth fresh biomass
#' x=c(4426.7,2993.9,1913.5,1966.3,2008.8,2297.6,1564.9,2541.6)# projected area of the plant
#' x1=c(3683.062,2548.309,2677.843,2669.239,1933.728)# out of sample projected area (in cm square)
#'
#' Biomass.Estimation(Biomass=y,PA=x,model="ANN",newPA = x1)
#'
#'@references
#'  Patil, S. B., & Bodhe, S. K. (2011). Betel leaf area measurement using image processing. \emph{International Journal on Computer Science and Engineering}, 3(7), 2656-2660.
#' \cr Misra, T., Marwaha, S., Arora, A., Ray, M.,Kumar, S., Kumar, S. (2021). Leaf area assessment using image processing and support vector regression in rice. \emph{Indian Journal of Agricultural Sciences}, 91 (3), 388–92.
#'
#' @keywords FreshWeight ImageAnalysis RGB
#'
#' @export
#'
Biomass.Estimation<-function(Biomass,PA,model=c("lm", "ANN","SVM"), newPA){
  fit <- lm(Biomass ~ PA)
  kk=summary(fit)
  kk1=fit$fitted.values
  RMSE_lm=sqrt(mean((Biomass - kk1)^2))
  x2=data.frame(newPA)
  colnames(x2) <- "PA"
  kk2=predict(fit,newdata=x2)
  hh1=cbind(Biomass ,PA)
  data=data.frame(hh1)
  maxs <- apply(data, 2, max)
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  nn <- neuralnet(Biomass~PA,data=scaled,hidden=2,linear.output=T)
  pr.nn <- compute(nn,scaled[2])
  kk1_ann<- pr.nn$net.result*(max(data$Biomass)-min(data$Biomass))+min(data$Biomass)
  RMSE_ANN<-sqrt(mean((data$Biomass - kk1_ann)^2))
  scaled1 <- as.data.frame(scale(newPA, center = min(newPA), scale = max(newPA) - min(newPA)))
  pr.nn1 <- compute(nn,scaled1)
  kk2_ann<- pr.nn1$net.result*(max(data$Biomass)-min(data$Biomass))+min(data$Biomass)
  svm_model <- svm(Biomass ~ PA)
  kk_svm=summary(svm_model)
  kk1_svm=svm_model$fitted
  RMSE_SVM<-sqrt(mean((Biomass - kk1_svm)^2))
  x22=data.frame(newPA)
  colnames(x22) <- "PA"
  kk2_SVM<-predict(svm_model, newdata =x22)
  if (model=="lm") {return(list(modelsummary=kk,fitted.values=kk1,  RMSE=RMSE_lm,Predicted.values=kk2))}
  if (model=="ANN") {return(list(plot(nn),fitted.values=kk1_ann, RMSE=RMSE_ANN, Predicted.values=kk2_ann))}
  if (model=="SVM") {return(list(modelsummary=kk_svm,fitted.values=kk1_svm, RMSE=RMSE_SVM, Predicted.values=kk2_SVM))}
  else {return("not a valid model")}
}


