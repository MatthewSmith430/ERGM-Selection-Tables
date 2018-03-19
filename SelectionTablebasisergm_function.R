#' @title selectionTable.basis.ergm
#'
#' @description Creates selection table dataframe
#' @param net network object
#' @param model ERGM model
#' @param vname Attribute Name
#' @param effNames Effect names
#' @param levls Levels
#' @param levls.alt ALter Levels (by defult the same as the levls/ego levels)
#' @param silent silent (default is FALSE)
#' @export
#' @return Selection Table Dataframe

selectionTable.basis.ergm<- function(net, model,vname, effNames,
                                     levls, levls.alt=levls,
                                     silent=FALSE){

  ATTR<-network::get.vertex.attribute(net,vname)
  vmean<-mean(ATTR)

  Delta  <- range(ATTR)

  coefDF<-model$coef
  coefDF<-as.data.frame(coefDF)
  coefDF<-tibble::rownames_to_column(coefDF,var="coef")
  coef_filter<-coefDF[coefDF$coef %in% effNames, ]
  vtheta<-coef_filter$coefDF
  # vtheta contains the parameter values in x
  K <- length(levls)
  KA <- length(levls.alt)
  valter <- rep(levls.alt,K)
  vego <- rep(levls,each=KA)
  fact <- 1:K
  ego <- factor(rep(fact,each=KA))
  coeffs <- matrix(NA, K*KA, length(coef_filter$coef))
  coeffs[,1] <- (valter - vego)*(valter - vego)
  coeffs[,2]<- (valter - vmean)*(valter - vmean)
  coeffs[,3]<- (valter - vmean)
  coeffs[,4]<- (vego - vmean)
  coeffs[,5]<- (vego - vmean)*(vego - vmean)

  select <- coeffs %*% vtheta
  df <- data.frame(ego,vego,valter,select)
  list(df=df, veff=effNames, vtheta=vtheta, vmean=vmean, #vsmean=vsmean,
       Delta=Delta, coeffs=coeffs)
}
