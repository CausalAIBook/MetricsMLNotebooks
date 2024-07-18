## RDFlex
## R package for "Flexible Covariate Adjustments in Regression Discontinuity Designs" 
## Authors: Claudia Noack, Tomasz Olma, and Christoph Rothe


############################# Required Packages ################################
################################################################################

# RD estimation
req.pcgs1 <- c("rdrobust", "RDHonest")
sapply(req.pcgs1, requireNamespace)

# SuperLearner and Machine Learning Methods
req.pcgs2 <- c("SuperLearner", "xgboost", "grf", "nnet", "e1071", "ranger", "hdm")
sapply(req.pcgs2, requireNamespace)
library(SuperLearner)


########################### Arguments for RDFlex ###############################
################################################################################

# Data
#' @param Y is the outcome variable 
#' @param X is the running variable 
#' @param D is the treatment indicator. Set D=NULL (default) to run a sharp RD 
#' @param Z is a matrix with predetermined covariates
#' @param Z.lasso is a matrix with additional transformations of Z for Lasso estimation
#' @param cutoff is the cutoff value

# First-stage parameters
#' @param K is the number of folds for cross-fitting.
#' @param h.fs is the bandwidth to restrict the sample in the first stage. If h.fs=NULL, then the whole sample is used.
#' @param ml.methods is a list specifying algorithms to be used in SuperLearner and screening algorithms. See example.

# Second-stage parameters
#' @param kernel specifies the ternel function for the second stage: "triangular" (default option), "epanechnikov" or "uniform".
#' @param h is the bandwidth for the 2nd stage RD regression. If h=NULL, then chosen to be MSE optimal. Methods based on rdrobust, use the same bandwidth for the bias correction (b=h or rho=1).
#' @param M is the bound on the second derivative for RDHonest. Multiple constants can be supplied as a vector.
#' @param RD.method "RDHonest", "rdrobust", or "rdus" (undersmoothing)
#' @param se.method specifies either "nn" (nearest neighbors) or "EHW" (Eicker-Huber-White) standard error for the RD regression
#' @param alpha determines confidence level 1-alpha for confidene intervals

# Aggregaion across many split
#' @param B specifies how many different splits to use

#' Technical Parameter
#' #'@param n.cores specifies the number of cores to use.  
############################# Functions ########################################
################################################################################


###################  Functions for the first stage #############################

# SuperLearner adjustments
first.stage.sl <- function(dd, sl.methods, Res) {
  
  est_cov <- as.data.frame(cbind(dd$Z))
  new_cov <- as.data.frame(cbind(dd$Zhat))
  
  family <- list()
  family$family <- ifelse(length(table(dd$outcome))>2, "gaussian", "binomial")
  

  m.superlearner <- SuperLearner::SuperLearner(Y=dd$outcome, X=est_cov, newX=new_cov, 
                                                family=family, SL.library = sl.methods)
  
  
  pred <- m.superlearner$SL.predict
  object <- m.superlearner
  
  return(list(pred=pred,object=object)  )
}

stage1 <- function(d, outcome="Y", base.covs, K, h.fs, sl.methods){
# d is a list with Y, X, D, Z, and fold
# Outcome can be either "Y" or "D"
# base.covs indicates which columns in Z are baseline covariates (not transformations)
# Cutoff should be normalized to zero
# sl.methods is of the form list(SL.mean=c("SL.mean","Res"), SL.lm=c("SL.lm","Res"), c("SL2.rlasso","All") )
  
  
  # Select the outcome variable
  d$outcome <- d[[outcome]]
  
  # Indictor for being to the right of the cutoff
  d$DX <- (0 <= d$X)
  
  # Indicator for observations to be used in the first stage (uniform weights)
  if (is.null(h.fs)) {
    d$w <- rep(1,times=length(d$X))
  } else {
    d$w <- (abs(d$X)<h.fs)
  }
  
  # "Screening algorithm" to restrict covariates
  temp.base.covs <- rep(FALSE, times=dim(d$Z)[2])
  temp.base.covs[base.covs] <- TRUE
  Res <- function(...) {temp.base.covs}

  # Create names of the form "method_screenalgorithm" for SuperLearner
  sl.methods.names <- paste0(sapply(sl.methods,"[[",1),"_",sapply(sl.methods,"[[",2))
  
  # Names of all methods used to construct adjustment terms
  names.methods = c(sl.methods.names, "sl") 

  # List to store first-stage predictions
  fs.pred <- list()
  for(m.name in names.methods) fs.pred[[m.name]] <- matrix(NA, ncol=2, nrow=d$n)
  
  # First-stage object
  fs.object.sl  <- list()
  
  ## Cross-fitting: d already includes d$fold

  # Temporary data for a given fold; 
  d0 <- d1 <-list(NA)
  
  # Main loop for cross-fitting
  for(k in 1:K){
    print(k)
    id=(d$fold!=k)
    
    kd0 <- paste0("fold",k,"d0")
    kd1 <- paste0("fold",k,"d1")
    
    # Data used for estimation for the kth fold
    d0$outcome=d$outcome[d$DX==0 & id & d$w>0]; d0$X=d$X[d$DX==0 & id & d$w>0];  d0$Z= d$Z[d$DX==0 & id & d$w>0,]
    d1$outcome=d$outcome[d$DX==1 & id & d$w>0]; d1$X=d$X[d$DX==1 & id & d$w>0];  d1$Z= d$Z[d$DX==1 & id & d$w>0,]
    
    # Covariates used for prediction for the kth fold
    d1$Zhat=d0$Zhat=d$Z[!id, ]
    
    # SuperLearner adjustments
    fs.object.sl[[kd0]]   <- first.stage.sl(d0, sl.methods=sl.methods, Res=Res)
    fs.object.sl[[kd1]]   <- first.stage.sl(d1, sl.methods=sl.methods, Res=Res)
    
    fs.pred[["sl"]][!id, ]    <- cbind(fs.object.sl[[kd0]]$pred, 
                                       fs.object.sl[[kd1]]$pred)
    
    # Single-method ML adjustments are retrieved from SuperLearner object
    for (m.name in sl.methods.names) {
      fs.pred[[m.name]][!id, ] <- cbind(fs.object.sl[[kd0]]$object$library.predict[,m.name], 
                                        fs.object.sl[[kd1]]$object$library.predict[,m.name])
      
    }
    
  }
  
  # Generate the adjustment terms and modified outcomes
  d[[paste0("eta_",outcome)]] <- d[[paste0("M_",outcome)]] <- list()

  for (m.name in names.methods){
    d[[paste0("eta_",outcome)]][[m.name]] <- (fs.pred[[m.name]][,1] + fs.pred[[m.name]][,2])/2
    d[[paste0("M_",outcome)]][[m.name]] <- d$outcome - d[[paste0("eta_",outcome)]][[m.name]]
  }
  d$outcome <- NULL
  
  # Weights in SuperLearner by fold
  fs.weights.sl <- cbind(sapply( names(fs.object.sl), function(fold)  fs.object.sl[[fold]]$object$coef))
  
  return(list(d=d, fs.weights.sl=fs.weights.sl, fs.object.sl=fs.object.sl))
}


#################### Functions for the second stage ############################

# Wrappers for RDHonest, rdrobust and Undersmoothing: when h is null, choose the MSE-optimal one

RDHonest2 <- function(y, x, d=NULL, fuzzy=FALSE, m, h=NULL, se.method="nn", kernel="triangular", alpha) {
  
  form <- if (fuzzy) formula(y~d|x) else formula(y~x)
  
  if (is.null(h)) {
    temp <- RDHonest::RDHonest(form, M=m, opt.criterion = "MSE", 
                               se.method = se.method, kern=kernel, alpha=alpha)
  } else {
    temp <- RDHonest::RDHonest(form, M=m, h=h, se.method = se.method,
                               kern=kernel, alpha=alpha)
  }
  return(temp)
}


rdrobust2 <- function(y, x, d=NULL, fuzzy=FALSE, m, h=NULL, se.method="nn", kernel="triangular", alpha)  {
  
  vce <- ifelse(se.method=="nn", "nn", "hc3")
  d <- if (fuzzy) d else NULL
  
  if (is.null(h)) {
    temp <- rdrobust::rdrobust(y=y, x=x, fuzzy=d, bwselect = "mserd", rho=1, 
                               vce = vce, kernel=kernel, level=100*(1-alpha))
  } else {
    temp <- rdrobust::rdrobust(y=y, x=x, fuzzy=d, h=h, vce = vce,
                               kernel=kernel, level=100*(1-alpha))
  }
  return(temp)
}

rdus2 <- function(y, x, d=NULL, fuzzy=FALSE, m, h=NULL, se.method="nn", kernel="triangular", alpha)  {
  
  vce <- ifelse(se.method=="nn", "nn", "hc3")
  d <- if (fuzzy) d else NULL
  
  if (is.null(h)) h <- rdrobust::rdbwselect(y=y, x=x, fuzzy=d, bwselect = "mserd",
                                            vce = vce,  kernel=kernel)$bws[1]*length(y)^(-1/20) 
  temp <- rdrobust::rdrobust(y=y, x=x, fuzzy=d, h=h,  vce = vce,  kernel=kernel,
                             level=100*(1-alpha))
  
  return(temp)
}


stage2 <- function(d, h, RD.method, M, fuzzy, se.method, kernel, alpha) {
# d is generated by stage1() and includes M_Y (and M_D if fuzzy)  
  
  results <- list()
  for (mn in names(d$M_Y)) {
    results[[mn]] <- do.call(paste0(RD.method,"2"), list(y=d$M_Y[[mn]], x=d$X, 
                                                         d=d$M_D[[mn]], fuzzy=fuzzy, 
                                                         m=M, h=h, se.method=se.method,
                                                         kernel=kernel, alpha=alpha)) 
  }

  return(results)
    }
    

############ Functions for aggregation across many splits ######################


aggregate.many.splits.main <- function(results, RD.method, fs.method, alpha, fuzzy){
# results is a list of results from stage2()
# Aggregates results for one fs.method
      
  if(RD.method=="RDHonest"){
    
    a <- results[[1]][[fs.method]]
    
    results <- lapply(results, FUN=function(x) x[[fs.method]])
    
    estimates <- sapply(results, function(tt) tt$coef$estimate)
    a$coef$estimate <- median(estimates)
    
    a$coef$maximum.bias <- median(sapply(results, function(tt) tt$coef$maximum.bias))
    a$coef$bandwidth <- median(sapply(results, function(tt) tt$coef$bandwidth))
    
    sd <- sapply(results, function(tt) tt$coef$std.error)
    a$coef$std.error <- sqrt(median(sd^2 + (estimates-a$coef$estimate)^2))
    
    a$coef$conf.low.onesided <- a$coef$estimate-a$coef$maximum.bias-
                                        stats::qnorm(1-alpha)*a$coef$std.error
    a$coef$conf.high.onesided <- a$coef$estimate+a$coef$maximum.bias+
                                        stats::qnorm(1-alpha)*a$coef$std.error
    
    a$coef$leverage <- median(sapply(results, function(tt) tt$coef$leverage))
    
    a$coef$cv <- sqrt(stats::qchisq(0.95, ncp=(a$coef$maximum.bias/a$coef$std.error)^2,
                                         df=1 ))
    
    
    a$coef$hl <- a$coef$cv*a$coef$std.error
    
    a$coef$lower <- a$coef$estimate-a$coef$hl
    a$coef$upper <- a$coef$estimate+a$coef$hl  
    
    if(fuzzy){
       a$coef$first.stage <- median(sapply(results, function(tt) tt$coef$first.stage))
       a$coef$M <- median(sapply(results, function(tt) tt$coef$M))
        }
  }
  
  if(RD.method=="rdrobust" | RD.method=="rdus"){
    results <- lapply(results, FUN=function(x) x[[fs.method]])
    
    a <- results[[1]]
  
    # Median component-wise for a list of vectors
    vec.median <- function(name){ 
      temp <- sapply(results, function(tt) tt[[name]])
      return(matrixStats::rowMedians(temp))}
    
    # Median component-wise for a list of matrices
    mat.median <- function(name){ 
      temp <- lapply(results, function(tt) unlist(tt[[name]]))
      return(apply(do.call(abind::abind, c(temp, list(along=3))),
                                       1:2, median))}

    
    # Median sample sizes, bandwidths, and coefficients
    a$N_h <- vec.median("N_h")
    a$N_b <- vec.median("N_b")
    a$bws <- mat.median("bws")
    a$coef <- vec.median("coef")
    
    # Aggregate std. error
    ests <- sapply(results, function(tt) tt$coef)
    ses  <- sapply(results, function(tt) tt$se)
    a$se <- sqrt(matrixStats::rowMedians(ses^2 + (ests-matrixStats::rowMedians(ests))^2))  
    
    # Estimate: coefficients and std. errors
    a$Estimate[1:2] <- a$coef[c(1,2)]
    a$Estimate[3:4] <- a$se[c(1,3)]
   
    a$beta_Y_p_l <- vec.median("beta_Y_p_l")
    a$beta_Y_p_r <- vec.median("beta_Y_p_r")
    a$bias <- vec.median("bias")
    
    
    var.aggregation <-function(beta.name, var.name){
      
      temp.var <-lapply(results, function(tt) unlist(tt[[var.name]]))
      
      temp.est <-sapply(results, function(tt) unlist(tt[[beta.name]]))
      temp.est <- sapply(1:ncol(temp.est), function(i) temp.est[,i]%*%t(temp.est[,i]), simplify = FALSE)
      
      temp.median.est <- apply(do.call(abind::abind, c(temp.est, list(along=3))),
                               1:2, median)
      
      temp.final=list()
      for(i in 1:length(temp.var)){
        temp.final[[i]] <- temp.var[[i]] + (temp.est[[i]]-temp.median.est)^2
      }
      
      return(apply(do.call(abind::abind, c(temp.final, list(along=3))),
                   1:2, median))
    }
    
    a$V_cl_l <- var.aggregation("beta_Y_p_l", "V_cl_l")
    a$V_cl_r <- var.aggregation("beta_Y_p_r", "V_cl_r")
    a$V_rb_l <- var.aggregation("beta_Y_p_l", "V_rb_l")
    a$V_rb_r <- var.aggregation("beta_Y_p_r", "V_rb_r")
    

    a$z <-    a$coef/a$se 
    a$pv <-  2*pnorm(-abs( a$z))
    
    
    a$ci <-cbind( a$coef - qnorm(1-alpha/2)*a$se,
                  a$coef - qnorm(1-alpha/2)*a$se)  
  
    
    if(fuzzy){
      a$beta_T_p_l <- vec.median("beta_T_p_l")
      a$beta_T_p_r <- vec.median("beta_T_p_r")
      
      a$tau_T <- vec.median("tau_T")
      ests_T  <- sapply(results, function(tt) tt$tau_T)
      ses_T   <- sapply(results, function(tt) tt$se_T)
      a$se    <- sqrt(matrixStats::rowMedians(ses_T^2 + (ests_T-matrixStats::rowMedians(ests_T))^2))  
      a$t_T   <- a$tau_T/a$se_T 
      a$pv_T  <- 2*pnorm(-abs(a$t_T))
      a$ci_T  <- cbind(a$tau_T - qnorm(1-alpha/2)*a$se_T,
                       a$tau_T - qnorm(1-alpha/2)*a$se_T)
    }
    
    # constant: N, M, p, q, c, kernel, all, vce, bwselect, level, masspoints, rdmodel
    # beta_covs, call, cl
    }
  return(a)
}

aggregate.many.splits <- function(results, RD.method, alpha, fuzzy){
  
  agg.results <- list()
  
  for (fs.method in names(results[[1]]) ) {
    agg.results[[fs.method]] <- aggregate.many.splits.main(results, RD.method, 
                                                           fs.method, alpha, fuzzy)
      }
  
 return(agg.results) 
    }


######################## Main functions ########################################


RDFlex.main <- function(d, base.covs, kernel="triangular", 
                        K=10, h=NULL, h.fs=NULL, M=NULL, fuzzy,
                        sl.methods, RD.method, se.method="nn", alpha=0.05) {
  
    # Random split into folds
    d$fold <- sample(rep(1:K, times=(d$n/K+1))[1:d$n])
  
    # Generate adjustment terms
    d <- stage1(d, outcome="Y", base.covs, K, h.fs, sl.methods)$d
    if (fuzzy) d <- stage1(d, outcome="D", base.covs, K, h.fs, sl.methods)$d
    
    # Second stage
    tt <- stage2(d, h, RD.method, M, fuzzy, se.method, kernel, alpha) 
  
    return(tt)
    }




RDFlex <- function(Y, X, D=NULL, Z, Z.lasso=NULL, cutoff=0, kernel="triangular", 
                        K=10, B=3, h=NULL, h.fs=NULL, M=NULL, 
                        ml.methods=list(neuralnets=TRUE,
                                        randomforest=TRUE,
                                        boostedtree=TRUE,
                                        lasso=TRUE,
                                        extra=NULL),
                        RD.method, se.method="nn", alpha=0.05, n.cores=1){
  
  
  ### Prepare data
  
  # Create list with data for RDFlex.main
  d <- list()
  d$Y <- Y; d$X <- X; d$D <- D; d$Z <- as.matrix(cbind(Z, Z.lasso))
  d$n <- length(d$Y)
  # Indices of baseline covariates in Z
  base.covs <- 1:ncol(Z)
  
  # Indicator whether design is fuzzy
  fuzzy <- !is.null(d$D)
  
  # Normalize cutoff to zero
  d$X <- d$X - cutoff

  
  #### Check whether inputs are correct  #######
  
  exit=0
  
  # Check data completeness
  na.ok <- complete.cases(d$X) & complete.cases(d$Y) & complete.cases(d$Z)
  if(fuzzy) na.ok & complete.cases(d$D)
  
  if(!all(na.ok)){
    warning("Missing data is not supported.")
    exit=1
    }
  
  n <- length(d$Y)
  if( (length(d$X)!=d$n | nrow(d$Z)!=d$n)|
      ifelse(is.null(d$Z.lasso), FALSE, nrow(d$Z.lasso)!=d$n)|
      ifelse(fuzzy, length(d$D)!=d$n, FALSE)){
      warning("Data should be of equal size.")
    exit=1
  }
  
  
  if (kernel!="uniform" &  kernel!="triangular" & kernel!="epanechnikov"){
    warning("Kernel incorrectly specified. It is set to triangular")
    kernel="triangular"
  }
  
  if (RD.method!="RDHonest" & RD.method!="rdrobust" & RD.method!="rdus"){
    warning("RD method is incorrectly specified.")  
    exit = 1
  }
  
  if (se.method!="nn" & se.method!="EHW"){ 
    se.method="nn"
    warning("se.method is incorrectly specified. 
             It is set to the default value.")
  }
  
  if ( 0 <= min(d$X)  | 0 >= max(d$X)){
    warning("Cutoff should be set within the range of X")
    exit = 1
  }
  
  if (alpha>1 | alpha<=0){
    warning("alpha should be set between 0 and 1")
    exit = 1
  }
  
  
  if (!is.null(h)){
    if(!is.numeric(h)| (h<=0)){warning("h should be numeric or NULL and greater than 0. 
                                       It is set to NULL")
      h=NULL}
  }
  
  
  if (!is.null(h.fs)){
    if(!is.numeric(h.fs)| (h.fs<=0)){warning("h.fs should be numeric or NULL and greater than 0. 
                                       It is set to NULL")
      h.fs=NULL}
  }
  
  
  if(d$n<max(100, K)){
    if(d$n<K)   warning("Number of folds is larger than sample size")
    else   warning("Not enough observations for reliable inference")
    exit=1  }
  
  if(!is.logical(ml.methods$neuralnets)|!is.logical(ml.methods$randomforest)|
     !is.logical(ml.methods$boostedtree)| !is.logical(ml.methods$lasso)){
    warning("ml.methods incorrectly specified. Should be logical")
    exit=1 
  }
  
  if(!is.null(ml.methods$extra)){
    if(!is.list(ml.methods$extra)){
    warning("ml.methods$extra incorrectly specified.
            Should be a list")
    exit=1
    
    }else{
    
      for (j in 1:length(ml.methods$extra)) {
        if(!exists(ml.methods$extra[[j]][1], mode = "function")){
        warning("ml.methods$extra incorrectly specified.
                 First Element in the list should be a function.")
        exit=1
        }
        }
      
      for (j in 1:length(ml.methods$extra)) {
      if(!(ml.methods$extra[[j]][2] %in% c("All", "Res"))){
        warning("ml.methods$extra incorrectly specified.
                 Second Element should be either All or Res")
        exit=1
      }
    }
      
      }
  }
  
  if (exit>0) stop()
  
  
  #### Define learners ####
  
  # Mean and Linear Regression Adjustments (always include)
  sl.methods <- list(SL.mean=c("SL.mean","Res"), SL.lm=c("SL.lm","Res"))
  
  # Neural Nets
  if(ml.methods$neuralnets) sl.methods <- add.Learner(sl.methods, name="SL2.nnet", 
                                        tune = list(size = c(5), decay = 0.1), Zname="Res")
  
  # Random Forest
  if(ml.methods$randomforest) sl.methods <- add.Learner(sl.methods, name="SL2.randomForest", 
                                          tune = list(ntree = c(500)), Zname="Res")
  
  # Boosted Tree
  if(ml.methods$boostedtree) sl.methods <- add.Learner(sl.methods, name="SL2.gbm",
                                         tune = list(gbm.trees = c(100), 
                                                     interaction.depth = c(1),
                                                     shrinkage = c(0.1) ), Zname="Res") 
  
  # rlasso
  if(ml.methods$lasso) sl.methods$rlasso  <- c("SL2.rlasso","All") #rlasso
  
  sl.methods <- c(sl.methods, ml.methods$extra)


  ### Estimation
  if (n.cores==1) {
    results <- list()
    start <- proc.time()
    for(b in 1:B){
      print(paste("Split number", b))
      results[[b]] <- RDFlex.main(d, base.covs, kernel=kernel, K=K, h=h, h.fs=h.fs,
                              M=M, fuzzy=fuzzy, sl.methods=sl.methods, RD.method, se.method=se.method, alpha)
    }
    proc.time() - start
  
  } else {

    library(doSNOW)
    # List of functions to export
    funs_exp <- c("RDHonest2", "rdrobust2", "rdus2", "RDFlex.main", "stage1", "stage2", "first.stage.sl",
                  "SL2.nnet", "SL2.randomForest", "SL2.gbm", "SL2.rlasso")
    
    if(ml.methods$neuralnets) funs_exp <- c(funs_exp, "SL2.nnet_1")
    if(ml.methods$randomforest) funs_exp <- c(funs_exp, "SL2.randomForest_1")
    if(ml.methods$boostedtree) funs_exp <- c(funs_exp, "SL2.gbm_1")
    for (i in 1:length(ml.methods$extra)) funs_exp <- c(funs_exp, ml.methods$extra[[i]][1]) 
    
    
    cl<-parallel::makeCluster(n.cores)
    doSNOW::registerDoSNOW(cl)
    
    results <- foreach(b=1:B,  
                       .packages = c("SuperLearner", req.pcgs1, req.pcgs2), .export=funs_exp) %dopar% { 
      RDFlex.main(d, base.covs, kernel=kernel, K=K, h=h, h.fs=h.fs,
                    M=M, fuzzy=fuzzy, sl.methods=sl.methods, RD.method, se.method=se.method, alpha)
    }
    stopCluster(cl)
  }
  
  
  agg.results <- aggregate.many.splits(results, RD.method, alpha=alpha, fuzzy=fuzzy)
  print(agg.results$sl)
  
  return(agg.results)
  }