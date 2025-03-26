#---------------------------------------------
# Preliminaries                            ####
options(future.globals.maxSize = 8000 * 1024^2)
library('magrittr');library(future.apply);library(dplyr);library(sfaR) ;library(micEcon);library(frontier)
library(purrr);library(data.table);library(MatchIt);library(randomForest);library(CBPS);library(dbarts)
library(optmatch);library(Matching);library(rgenoud);library(quadprog);library(car)
#---------------------------------------------
# Minor                                    ####
mode <- function(x,na.rm = T) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}
#---------------------------------------------
# DATA Prep                                ####
Fxn_DATA_Prep <- function(DATA){
  DATA$Weight <- DATA$WeightHH
  
  # DATA$Y  <- DATA$HrvstKg #/1000
  # DATA$I1 <- DATA$Area #/1000
  # DATA$I2 <- DATA$SeedKg #/1000
  # DATA$I3 <- DATA$HHLaborAE #/1000
  # DATA$I4 <- DATA$HirdHr #/1000
  # DATA$I5 <- DATA$PestLt #/1000
  # DATA$I6 <- DATA$FertKg #/1000
  
  for(vv in c("AgeYr")){ # "Y", "I1", 
    DATA[,paste0("ln",vv)] <- log(DATA[,vv])
  }
  
  for(vv in c("YerEdu")){ # "I2", "I3", "I4", "I5", "I6", 
    DATA[,paste0("ln",vv)] <- log(DATA[,vv] + 0.00001)
  }
  
  for( vv in c("EduLevel", "Survey", "Region", "Ecozon", "Locality", "Ethnic", 
               "Season", "EduCat", "Head", "Religion", "Marital", "CropID")){
    tryCatch({ DATA[,vv] <- haven::as_factor(DATA[,vv]) }, error=function(e){})
  }
  
  tryCatch({
    DATA$EduLevel <- ifelse(as.character((DATA$EduLevel)) %in% "None","0",as.character((DATA$EduLevel)))
    DATA$EduLevel <- ifelse(DATA$EduLevel %in% "Primary","1",DATA$EduLevel)
    DATA$EduLevel <- ifelse(DATA$EduLevel %in% "JSS","2",DATA$EduLevel)
    DATA$EduLevel <- ifelse(DATA$EduLevel %in% "SSS","3",DATA$EduLevel)
    DATA$EduLevel <- ifelse(DATA$EduLevel %in% "Post SSS","4",DATA$EduLevel)
    DATA$EduLevel <- as.numeric(DATA$EduLevel)
  }, error=function(e){})
  
  DATA$CropID <- as.character(DATA$CropID)
  DATA$Ecozon <- as.character(DATA$Ecozon)
  DATA$Survey <- as.character(DATA$Survey)
  DATA$Region <- as.character(DATA$Region)
  
  return(DATA)
}

#---------------------------------------------
# MSF specs                                ####

Fxn_SPECS <- function(TechVarlist,mainF=2,mainD=1){
  SPECS <- as.data.frame(
    data.table::rbindlist(
      future_lapply(1:length(FXNFORMS),function(f){data.frame(level="Pooled",f=f, d=1:length(DISTFORMS))}), fill = TRUE))
  SPECS <- rbind(SPECS[SPECS$f %in% mainF,],SPECS[SPECS$d %in% mainD,])
  SPECS$disasg <- "CropID"
  SPECS <- unique(SPECS)
  SPECS <- unique(
    rbind(SPECS,
          data.frame(disasg="CropID"   ,level=c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"),SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD),c("f","d")]),
          as.data.frame(
            data.table::rbindlist(
              lapply(
                c("Female","Region" ,"Ecozon" ,"EduCat" ,"EduLevel" ,"AgeCat"),
                function(w){
                  tryCatch({
                    DONE <- data.frame(disasg=w   ,level=unique(DATA[,w])   ,SPECS[(SPECS$f %in% mainF & SPECS$d %in% mainD),c("f","d")])
                    return(DONE)
                  }, error = function(e){return(NULL)})
                }), fill = TRUE))))
  
  SPECS$TechVar <- TechVarlist[1]
  
  if(length(TechVarlist)>1){
    SPECS <- unique(
      rbind(SPECS,
            data.frame(TechVar=TechVarlist[2:length(TechVarlist)],
                       SPECS[(SPECS$disasg %in% "CropID" & SPECS$level %in% "Pooled" & SPECS$f %in% mainF & SPECS$d %in% mainD),c("disasg","level","f","d")])))
  }
  
  SPECS <- rbind(SPECS[SPECS$d %in% mainD,],SPECS[!SPECS$d %in% mainD,])
  
  return(SPECS)
}

#---------------------------------------------
# Draw/match specs                         ####
Fxn_draw_spec <- function(drawN,DATA,myseed=1113023){
  # drawN <- 100
  drawlist <- as.data.frame(
    data.table::rbindlist(
      lapply(
        unique(DATA$Survey),
        function(glss,myseed){
          set.seed(myseed)
          return(data.frame(glss=glss,ID = 0:drawN,EaId = c(0,sample(DATA[DATA$Survey %in% glss,"EaId"],size=drawN, replace=TRUE))))
        },myseed=myseed), fill = TRUE))
  drawlist <- drawlist %>%  tidyr::spread(glss, EaId)
  
  m.specs <- as.data.frame(
    data.table::rbindlist(
      lapply(
        unique(drawlist$ID),
        function(w){
          DONE <- NULL
          tryCatch({
            DONE <- data.frame(boot=w,rbind(
              # Nearest Neighbor Matching
              data.frame(method = "nearest", distance = c("euclidean","scaled_euclidean","mahalanobis","robust_mahalanobis") , link = NA),
              data.frame(
                method = "nearest",
                distance = "glm" ,
                link = c("logit","probit","cloglog","cauchit")) #
            ))
          }, error=function(e){})
          return(DONE)
        }), fill = TRUE))
  
  m.specs$ARRAY <- 1:nrow(m.specs)
  print(head(m.specs))
  
  return(list(m.specs=m.specs,drawlist=drawlist))
}
#---------------------------------------------
# Sampels                                  ####

Fxn_Sampels <- function(DATA,Emch,Scle,Fixd,m.specs,i,drawlist){
  m.data <- DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)] 
  
  m.data <- m.data[complete.cases(m.data[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
  
  m.data <- dplyr::inner_join(doBy::summaryBy(list("Weight",c("Surveyx", "EaId")),data=m.data,FUN=sum,na.rm=T),m.data,by=c("Surveyx", "EaId"))
  names(m.data)[names(m.data) %in% "Weight.sum"] <- "alloc"
 
  # Drawing a stratified bootstrap sample
  m.data <- m.data[!m.data$EaId %in% c(t(drawlist[drawlist$ID %in% m.specs$boot[i],2:ncol(drawlist)])),]
  
  m.data <- dplyr::inner_join(doBy::summaryBy(list("Weight",c("Surveyx", "EaId")),data=m.data,FUN=sum,na.rm=T),m.data,by=c("Surveyx", "EaId"))
  names(m.data)[names(m.data) %in% "Weight.sum"] <- "allocj"
  
  m.data$pWeight <- m.data$Weight*(m.data$alloc/m.data$allocj)
  
  row.names(m.data)<-1:nrow(m.data)
  m.data$MtchId<-1:nrow(m.data)
  
  if(m.specs$link[i] %in% NA){
    cat(crayon::green("method =",m.specs$method[i],",distance =",m.specs$distance[i],Sys.time()),fill=T)
    m.out <- matchit(as.formula(Match.formula),
                     exact    = as.formula(paste0("~",Emch.formula)),
                     data     = m.data,
                     method   = paste0(m.specs$method[i]),
                     distance = paste0(m.specs$distance[i]),
                     estimand = "ATT",
                     s.weights = "pWeight",
                     ratio    = 1,
                     m.order  = ifelse(paste0(m.specs$distance[i]) %in% "glm","largest","closest"),
                     replace  = T,
                     reuse.max = 1,
                     tol = 1e-7)
  }else{
    cat(crayon::green("method =",m.specs$method[i],",distance =",m.specs$distance[i],"link=",m.specs$link[i],Sys.time()),fill=T)
    m.out <- matchit(as.formula(Match.formula),
                     exact    = as.formula(paste0("~",Emch.formula)),
                     data     = m.data,
                     method   = paste0(m.specs$method[i]),
                     distance = paste0(m.specs$distance[i]),
                     link     = paste0(m.specs$link[i]),
                     estimand = "ATT",
                     s.weights = "pWeight",
                     ratio    = 1,
                     #caliper  = 0.10,
                     #discard  = "both",
                     m.order  = ifelse(paste0(m.specs$distance[i]) %in% "glm","largest","closest"),
                     replace  = T,
                     reuse.max = 1,
                     tol = 1e-7)
  }
  
  md <- match.data(m.out)[c("Surveyx","EaId","HhId","Mid","UID","weights","pWeight")]
  DONE <- list(m.specs=m.specs[i,],m.out=m.out,md=md,df=m.data[c("Surveyx","EaId","HhId","Mid","UID","pWeight")])
  return(DONE)
}
#---------------------------------------------
# Covariate balance                        ####
Fxn_Covariate_balance <- function(){
  m.specs <- readRDS("results/mspecs.rds")
  m.specs <- m.specs[m.specs$boot %in% 0,]
  
  bal_tab <- as.data.frame(
    data.table::rbindlist(
      lapply(
        m.specs$ARRAY,
        function(i){
          # i <- 1
          DONE <- NULL
          tryCatch({ 
            m.out <-  readRDS(paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))
            bal_tab <- cobalt::bal.tab(m.out$m.out, un = TRUE,abs=TRUE, stats = c("m", "v", "ks"))$Balance
            bal_tab$Coef <- rownames(bal_tab)
            bal_tab <- bal_tab %>%  tidyr::gather(stat, value, c("Diff.Un","V.Ratio.Un","KS.Un","Diff.Adj","V.Ratio.Adj","KS.Adj"))
            bal_tab$stat <- gsub("V.Ratio","V_Ratio",bal_tab$stat)
            bal_tab <- tidyr::separate(bal_tab,"stat",into = c("stat","sample"),sep="[.]")
            
            DONE <- data.frame(m.out$m.specs,bal_tab)
          }, error=function(e){})
          return(DONE)
        }), fill = TRUE))
  
  rate <- bal_tab %>%  tidyr::spread(stat, value)
  rate$rate <- ((rate$Diff-0)^2 + (rate$KS-0)^2 + (rate$V_Ratio-1)^2)/3
  rate <- doBy::summaryBy(rate+Diff+V_Ratio+KS~ARRAY+method+distance+link+sample,data=rate,FUN=mean,na.rm=T)
  rate <- rate[order(-rate$rate,rate$ARRAY),]
  rate <- rate[rate$sample %in% "Adj",]
  rate$name <- ifelse(rate$link %in% "logit","Logit [PS]",NA)
  rate$name <- ifelse(rate$link %in% "cauchit","Cauchit [PS]",rate$name)
  rate$name <- ifelse(rate$link %in% "probit","Probit [PS]",rate$name)
  rate$name <- ifelse(rate$link %in% "cloglog","Complementary Log-Log [PS]",rate$name)
  
  rate$name <- ifelse(rate$distance %in% "euclidean","Euclidean",rate$name)
  rate$name <- ifelse(rate$distance %in% "robust_mahalanobis","Robust Mahalanobis",rate$name)
  rate$name <- ifelse(rate$distance %in% "scaled_euclidean","Scaled Euclidean",rate$name)
  rate$name <- ifelse(rate$distance %in% "mahalanobis","Mahalanobis",rate$name)
  rate$ID <- 1:nrow(rate)
  saveRDS(rate,file="results/mspecs_ranking.rds")
  saveRDS(rate[nrow(rate),],file="results/mspecs_optimal.rds")
  saveRDS(bal_tab,file="results/balance_table.rds")
  return("done")
}
#---------------------------------------------
# Functional forms                         ####
Fxn_SF_forms <- function(nX=5,TREND=FALSE){
  
  lnInputs  <- 0
  Inputs    <- 0
  Quadratic <- 0
  Translog  <- 0
  for(i in 1:nX){
    Inputs    <- paste0(Inputs,"+I",i)
    lnInputs  <- paste0(lnInputs,"+lnI",i)
    Quadratic <- paste0(Quadratic,"+I(1/2*I",i,"*I",i,")")
    Translog  <- paste0(Translog,"+I(1/2*lnI",i,"*lnI",i,")")
    for(j in 1:nX){
      if(i<j) Translog <- paste0(Translog,"+lnI",i,"*lnI",j)
      if(i<j) Quadratic <- paste0(Quadratic,"+I",i,"*I",j)
    }
  }
  
  FXNFORMS <- list(
    CD=gsub("0[+]","",lnInputs),                                        # Cobb-Douglas Production Function
    TL=paste0(gsub("0[+]","",lnInputs),"+",gsub("0[+]","",Translog)),   # Translog Production Function
    # LN=gsub("0[+]","",Inputs),                                          # Linear Production Function
    # QD=paste0(gsub("0[+]","",Inputs),"+",gsub("0[+]","",Quadratic)),    # Quadratic Production Function
    # GP=paste0(gsub("0[+]","",lnInputs),"+Y"),                           # Generalized Production Function
    # TP=paste0(gsub("0[+]","",lnInputs),"+",gsub("0[+]","",Inputs)),      # Transcendental Production Function
    NULL
  )
  
  FXNFORMS <- FXNFORMS[lengths(FXNFORMS) != 0]
  
  if(TREND) FXNFORMS$TP <- gsub(paste0("[+]I",nX),"",FXNFORMS$TP)
  
  DISTFORMS <- list(
    hnormal        = list("hnormal",scaling=FALSE),        # the half normal distribution (Aigner et al. 1977, Meeusen and Vandenbroeck 1977)
    tnormal        = list("tnormal",scaling=FALSE),        # the truncated normal distribution (Stevenson 1980)
    exponential    = list("exponential",scaling=FALSE),    # the exponential distribution
    tslaplace      = list("tslaplace",scaling=FALSE),      # the truncated skewed Laplace distribution (Wang 2012).
    genexponential = list("genexponential",scaling=FALSE), # the generalized exponential distribution (Papadopoulos 2020)
    tnormal_scaled = list("tnormal",scaling=TRUE),         # the truncated normal distribution (Stevenson 1980) with the scaling property model (Wang and Schmidt 2002)  !!!
    rayleigh       = list("rayleigh",scaling=FALSE),        # the Rayleigh distribution (Hajargasht 2015)
 
    uniform        = list("uniform",scaling=FALSE),        # the uniform distribution (Li 1996, Nguyen 2010)
    gamma          = list("gamma",scaling=FALSE),          # the Gamma distribution (Greene 2003)  !!!
    lognormal      = list("lognormal",scaling=FALSE),      # the log normal distribution (Migon and Medici 2001,Wang and Ye 2020)  !!!
    weibull        = list("weibull",scaling=FALSE)         # the Weibull distribution (Tsionas 2007)  !!!
  )
  return(list(FXNFORMS=FXNFORMS,DISTFORMS=DISTFORMS))
}

#---------------------------------------------
# Equation editor                          ####
Fxn.equation_editor <- function(
    data,
    FXN,
    outcome,
    slope_shifter="NONE",
    intercept_shifters=NULL,
    ulist=NULL,
    vlist=NULL){
  
  #---------------------------------------------------
  # Production function                            ####
  shifters <- "~1"
  
  if(!is.null(intercept_shifters$Svarlist)){
    for(sv in intercept_shifters$Svarlist){
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        shifters <- paste0(shifters,"+",sv)
      }
    }
  }
  if(!is.null(intercept_shifters$Fvarlist)){
    for(fv in intercept_shifters$Fvarlist){
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        shifters <- paste0(shifters,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  
  if(is.null(intercept_shifters$Svarlist) & is.null(intercept_shifters$Fvarlist)){
    prodfxn <- as.formula(paste0(outcome," ~",FXN[[1]]))
  }else{
    shifters <- gsub("~1[+]","",shifters)
    prodfxn <- as.formula(paste0(outcome,"~",FXN[[1]],"+",shifters))
    
    if(!slope_shifter %in% "NONE"){
      prodfxn <- as.formula(paste0(paste0(outcome,"~factor(",slope_shifter,")*(",FXN[[1]],") - (",FXN[[1]],")"),"+",shifters))
    }
  }
  
  #---------------------------------------------------
  # Inefficiency function                          ####
  
  uequ <- "~1"
  if(!is.null(ulist$Svarlist)){
    for(sv in ulist$Svarlist){
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        uequ <- paste0(uequ,"+",sv)
      }
    }
  }
  if(!is.null(ulist$Fvarlist)){
    for(fv in ulist$Fvarlist){
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        uequ <- paste0(uequ,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  if(is.null(ulist$Svarlist) & is.null(ulist$Fvarlist)){
    uequ <- NULL
  }else{
    uequ <- gsub("~1[+]","~",uequ)
    uequ <- as.formula(uequ)
  }
  
  #---------------------------------------------------
  # Production Risk function                       ####
  
  vequ <- "~1"
  if(!is.null(vlist$Svarlist)){
    for(sv in vlist$Svarlist){
      if(abs((sd(data[,sv],na.rm=T)/mean(data[,sv],na.rm=T))) > 0.001){ 
        vequ <- paste0(vequ,"+",sv)
      }
    }
  }
  if(!is.null(vlist$Fvarlist)){
    for(fv in vlist$Fvarlist){
      if(length(unique(as.character(data[,fv]))) > 1 & min(table(as.character(data[,fv]))/nrow(data))>0.0001){
        vequ <- paste0(vequ,"+",paste0("factor(",fv,")"))
      } 
    }
  }
  if(is.null(vlist$Svarlist) & is.null(vlist$Fvarlist)){
    vequ <- NULL
  }else{
    vequ <- gsub("~1[+]","~",vequ)
    vequ <- as.formula(vequ)
  }
  
  #---------------------------------------------------
  
  return(list(prodfxn=prodfxn,uequ=uequ,vequ=vequ))
}
#---------------------------------------------
# Coef and vcov Organizer                  ####
Fxn.fit_organizer <- function(fit,nX,FXN){
  
  if(names(FXN) %in% c("TL","QD","CD","LN")){
    tf <- ifelse(names(FXN) %in% c("TL","CD"),"lnI","I")
    est_list <- c("(Intercept)",names(coef(fit))[names(coef(fit)) %in% paste0(tf,1:nX)])
    est_name <- c(paste0("a_",0:nX))
    xNames <- paste0(tf,1:nX)
    for( i in 1:nX) {
      for (j in 1:nX) {
        if(i == j){
          est_name <- c(est_name,paste0("b_",i,"_",j))
          if(names(FXN) %in% c("TL","QD")) est_list <- c(est_list,paste0("I(1/2 * ",tf,i," * ",tf,j,")"))
        }
        if(i <  j){
          est_name <- c(est_name,paste0("b_",i,"_",j))
          if(names(FXN) %in% c("TL","QD")) est_list <- c(est_list,paste0(tf,i,":",tf,j))
        }
      }
    }
  }
  
  est_coef <- coef(fit)[est_list]
  est_vcov <- vcov(fit)[est_list,est_list]
  
  if(names(FXN) %in% c("CD","LN")){
    est_coef <- c(est_coef,rep(0,(length(est_name)-length(est_coef))))
    est_vcov_cdLN <- matrix(ncol=length(est_coef),nrow=length(est_coef),data=0)
    for(ii in 1:nrow(est_vcov)){
      for(jj in 1:nrow(est_vcov)){
        est_vcov_cdLN[[ii,jj]] <- est_vcov[[ii,jj]]
      } 
    }
    est_vcov <- est_vcov_cdLN
    rm(est_vcov_cdLN)
  }
  
  colnames(est_vcov) <- rownames(est_vcov) <- names(est_coef) <- est_name
  
  return(list(est_coef=est_coef,est_vcov=est_vcov,est_list=est_list))
}
#---------------------------------------------
# sfaR Summary                             ####

Fxn.sfaR_Summary <- function(fit){
  
  mlRes <- as.data.frame(summary(fit)$mlRes)
  names(mlRes) <- c("Estimate","StdError","Zvalue","Pvalue")
  mlRes$CoefName <- rownames(mlRes)
  
  #Variances
  TEST <- data.frame(
    Nobs = ifelse(is.null(summary(fit)$Nobs),NA,summary(fit)$Nobs),
    nXvar = ifelse(is.null(summary(fit)$nXvar),NA,summary(fit)$nXvar),
    nuZUvar = ifelse(is.null(summary(fit)$nuZUvar),NA,summary(fit)$nuZUvar),
    nvZVvar = ifelse(is.null(summary(fit)$nvZVvar),NA,summary(fit)$nvZVvar),
    mlLoglik = ifelse(is.null(summary(fit)$mlLoglik),NA,summary(fit)$mlLoglik),
    AIC = ifelse(is.null(summary(fit)$AIC),NA,summary(fit)$AIC),
    BIC = ifelse(is.null(summary(fit)$BIC),NA,summary(fit)$BIC),
    HQIC = ifelse(is.null(summary(fit)$HQIC),NA,summary(fit)$HQIC),
    sigmavSq = ifelse(is.null(summary(fit)$sigmavSq),NA,summary(fit)$sigmavSq),
    sigmauSq = ifelse(is.null(summary(fit)$sigmauSq),NA,summary(fit)$sigmauSq),
    Varu = ifelse(is.null(summary(fit)$Varu),NA,summary(fit)$Varu),
    Eu = ifelse(is.null(summary(fit)$Eu),NA,summary(fit)$Eu),
    Expu = ifelse(is.null(summary(fit)$Expu),NA,summary(fit)$Expu))
  
  TEST$Sigma <- sqrt(TEST$sigmauSq+TEST$sigmavSq)
  TEST$Gamma <- TEST$sigmauSq/(TEST$Sigma^2 )
  #Log-likelihood value of the M(S)L estimation.
  TEST$mlLoglik <- fit$mlLoglik
  #Numeric. Skewness of the residuals of the OLS estimation.
  TEST$olsSkew <- fit$olsSkew    
  #Logical. Indicating whether the residuals of the OLS estimation have the expected skewness.
  TEST$olsM3Okay <- as.numeric(fit$olsM3Okay %in% "Residuals have the expected skeweness olsM3Okay")
  TEST <- data.frame(coef=t(TEST[1,]),name=names(TEST))
  names(TEST) <- c("Estimate","CoefName")
  TEST$Pvalue <- NA
  #Coelli’s test for OLS residuals skewness. (See Coelli, 1995).
  TEST <- rbind(TEST,data.frame(CoefName="CoelliM3Test",Estimate=fit$CoelliM3Test[1],Pvalue=fit$CoelliM3Test[2]))
  #D’Agostino’s test for OLS residuals skewness. (See D’Agostino and Pearson,1973).
  TEST <- rbind(TEST,data.frame(CoefName=c("AgostinoOmn","AgostinoSkw","AgostinoKrt"),
                                Estimate=fit$AgostinoTest@test$statistic,
                                Pvalue=fit$AgostinoTest@test$p.value))
  #Likelihood Ratio Test of Inefficiency
  TEST <- rbind(TEST,data.frame(
    CoefName=c("LRInef"),
    Estimate=summary(fit)$chisq ,
    Pvalue=ifelse(summary(fit)$chisq>= emdbook::qchibarsq(0.99,  df = summary(fit)$df),0.01,
                  ifelse(summary(fit)$chisq>=emdbook::qchibarsq(0.95, df = summary(fit)$df),0.05,
                         ifelse(summary(fit)$chisq>=emdbook::qchibarsq(0.90, df = summary(fit)$df),0.10,
                                1)))))
  mlRes <- as.data.frame(data.table::rbindlist(list(mlRes,TEST), fill = TRUE))
  return(mlRes)
}
#---------------------------------------------
# SF Work Horse                            ####

Fxn.SF_WorkHorse_FT <- function(
    data,yvar,xlist,wvar=NULL,d,f,
    slope_shifter="NONE",
    intercept_shifters= NULL,
    ulist = NULL,
    vlist = NULL,
    UID,
    TREND=FALSE){
  
  # data <- arms[arms$FMTYPOL %in% 7,] ; f <- 3
  
  nX <- length(xlist)
  xNames <- paste0("I",1:nX)
  
  SF_forms  <- Fxn_SF_forms(nX=nX,TREND=TREND)
  FXN       <- SF_forms$FXNFORMS[f]
  udist     <- SF_forms$DISTFORMS[d][[1]][[1]]
  scaling   <- SF_forms$DISTFORMS[d][[1]][[2]]
  logDepVar <- names(FXN) %in% c("CD","TL","GP","TP")
  if(logDepVar %in% TRUE) xNames <- paste0("ln",xNames)
  
  if(is.null(wvar))  data$weights <- 1
  if(!is.null(wvar)) data$weights <- data[,wvar]
  data$Y       <- data[,yvar]
  if(logDepVar %in% TRUE) data$lnY <- log(data$Y)
  for(i in 1:nX){
    data[,paste0("I",i)]   <- data[,xlist[i]]  
    if(logDepVar %in% TRUE) data[,paste0("lnI",i)] <- log(data[,paste0("I",i)] + 0.00001)
  }
  
  if(TREND %in% TRUE){
    data[,paste0("I",nX)]   <- data[,xlist[nX]]
    if(logDepVar %in% TRUE) data[,paste0("lnI",nX)] <- data[,xlist[nX]]
  }
  
  #Unrestricted Stochastic Frontier Estimation
  equations <- Fxn.equation_editor(outcome=ifelse(logDepVar,"lnY","Y"),
                                   data=data,FXN=FXN,slope_shifter=slope_shifter,
                                   intercept_shifters=intercept_shifters,ulist=ulist,vlist=vlist)
  
  sf <- list(optStatus="")
  for(sf_gradtol in c(1e-6,1e-3)){
    for(sf_tol in c(1e-12,1e-6)){
      for(sf_method in c('nr','nm','bfgs','bhhh','cg','sann','ucminf','mla','sr1','sparse','nlminb')){
        if(!sf$optStatus %in% "successful convergence "){
          tryCatch({
            if(is.null(equations$uequ)  &  is.null(equations$vequ)){
              sf <- sfacross(formula = equations$prodfxn, udist = udist,
                             scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                             gradtol=sf_gradtol,tol=sf_tol)
            }
            if(!is.null(equations$uequ) &  is.null(equations$vequ)){
              sf <- sfacross(formula = equations$prodfxn, udist = udist, uhet = equations$uequ, muhet = equations$uequ,
                             scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                             gradtol=sf_gradtol,tol=sf_tol)
            }
            if(is.null(equations$uequ)  & !is.null(equations$vequ)){
              sf <- sfacross(formula = equations$prodfxn, udist = udist, vhet = equations$vequ,
                             scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                             gradtol=sf_gradtol,tol=sf_tol)
            }
            if(!is.null(equations$uequ) & !is.null(equations$vequ)){
              sf <- sfacross(formula = equations$prodfxn, uhet = equations$uequ, muhet = equations$uequ, 
                             vhet = equations$vequ, udist = udist,scaling = scaling, S = 1, method = sf_method, 
                             logDepVar=logDepVar, data = data,gradtol=sf_gradtol,tol=sf_tol)
            }
          }, error=function(e){})
        }
      }
    }
  }
  
  # summary(sf)
  ef <- sfaR::efficiencies(sf)
  ef <- data.frame(data[row.names(ef),c(UID,"weights")],ef)
  ef$mlFitted <- sf$dataTable$mlFitted
  if(logDepVar %in% TRUE){ ef$mlFitted <- exp(ef$mlFitted)}
  
  data <- data[row.names(ef),]
  
  rk <- data.frame(data[c(UID,"weights")])
  if(logDepVar %in% TRUE){
    rk$ybar  <- ef$mlFitted*exp(-ef$u)
    rk$vrbr <- (rk$ybar-exp(sf$dataTable$lnY))^2
  }
  if(!logDepVar %in% TRUE){
    rk$ybar <- ef$mlFitted - ef$u
    rk$vrbr <- (rk$ybar-sf$dataTable$Y)^2
  }
  rk$risk <- sqrt(rk$vrbr)/rk$ybar
  rk <- rk[c(UID,"weights","risk")]
  
  sf_res <- Fxn.sfaR_Summary(sf)
  
  fit_organizer <- Fxn.fit_organizer(fit=sf,nX=nX,FXN=FXN)
  est_coef <- fit_organizer$est_coef
  est_vcov <- fit_organizer$est_vcov
  est_list <- fit_organizer$est_list
  
  el <- translogEla(xNames=xNames,data = data, coef = est_coef,dataLogged =logDepVar)
  names(el) <- paste0("el",1:ncol(el))
  if(!TREND %in% TRUE){el[,paste0("el",(ncol(el)+1))] <- rowSums(el)}
  if(TREND %in% TRUE ){el[,paste0("el",(ncol(el)+1))] <- rowSums(el[1:(ncol(el)-1)])}
  
  el <- data.frame(data[c(UID,"weights")],el)
  
  # Test Curvature & Monotonicity
  if(names(FXN) %in% c("TL","QD")){
    mono_obs <- translogCheckMono(xNames = xNames,data = data, coef = est_coef,dataLogged =logDepVar)$obs
    curv_obs <- translogCheckCurvature(xNames = xNames,data=data, est_coef, convexity = F, quasi = TRUE ,dataLogged =logDepVar)$obs
    curv <- mean(curv_obs,na.rm=T) # quasiconcave
    mono <- mean(mono_obs,na.rm=T)
  }
  
  if(names(FXN) %in% c("CD","LN")){
    curv <- mono <- as.numeric(mean(as.numeric(est_coef[2:(nX+1)] > 0),na.rm=T) %in% 1)
    # if(curv %in% 1){
    #   mono_obs <- rep(1,nrow(data))
    #   curv_obs <- rep(1,nrow(data))
    # }else{
    #   mono_obs <- rep(0,nrow(data))
    #   curv_obs <- rep(0,nrow(data))
    # }
  }
  
  mc <- data.frame(CoefName=c("mono","curv"),Estimate=c(mono,curv),StdError=NA,Zvalue=NA,Pvalue=NA)
  
  if(mono<0.80){
    
    if(names(FXN) %in% c("TL","QD")){
      # Use Minimum Distance estimation of Restricted production function coefficients 
      monoRestr <- translogMonoRestr(xNames = xNames,data=data,dataLogged = logDepVar)
      monoRestr <- monoRestr[rowMeans(is.finite(monoRestr)) %in% 1,]
      
      sf_minDist <- NULL
      
      if(is.null(sf_minDist)){
        tryCatch({
          inv_est_vcov <- solve(est_vcov)
          sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
          if(is.null(sf_minDist)){
            tryCatch({
              # Find nearest positive definite matrix
              inv_est_vcov <- corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist   <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
            }, error=function(e){})
          }
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)){
        tryCatch({
          inv_est_vcov <- Matrix::solve(est_vcov)
          sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
          if(is.null(sf_minDist)){
            tryCatch({
              # Find nearest positive definite matrix
              inv_est_vcov <-corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
            }, error=function(e){})
          }
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)){
        tryCatch({
          inv_est_vcov <- MASS::ginv(est_vcov)
          sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
          
          if(is.null(sf_minDist)){
            tryCatch({
              # Find nearest positive definite matrix
              inv_est_vcov <-corpcor::make.positive.definite(inv_est_vcov)
              sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
            }, error=function(e){})
          }
          
        }, error=function(e){})
      }
      
      if(is.null(sf_minDist)){
        tryCatch({
          inv_est_vcov <- matrix(0,nrow(est_vcov),nrow(est_vcov))
          diag(inv_est_vcov) <- 1
          sf_minDist <- solve.QP(Dmat=inv_est_vcov,dvec=rep(0,length(est_coef)),Amat=t(monoRestr),bvec=-monoRestr%*%est_coef)
        }, error=function(e){})
      }
      
      est_coefc  <- sf_minDist$solution + est_coef
      
      # Fitted frontire output of the restricted model (assuming efficiency == 1)
      lcFitted <- translogCalc( xNames = xNames,data=data,coef=est_coefc,dataLogged = logDepVar)
      # list(est_coefc=est_coefc, lcFitted = lcFitted)
    }
    
    if(names(FXN) %in% c("CD","LN")){
      Y <- all.vars(Fxn.equation_editor(outcome=ifelse(logDepVar,"lnY","Y"),data=data,FXN=FXN)$prodfxn)[1]
      X <- all.vars(Fxn.equation_editor(outcome=ifelse(logDepVar,"lnY","Y"),data=data,FXN=FXN)$prodfxn[-2])
      data.sem <- data
      data.sem[,X] <- 0
      fit.sem <- lm(equations$prodfxn,data=data)
      fit.sem$coefficients <- coef(sf)
      data.sem$const <- predict(fit.sem,data.sem)
      data.sem[,X] <- data[,X]
      fit.sem <- lavaan::lavaan(model = paste(paste0(Y,' ~ a_c*const+ ',paste0(paste0(paste0("a_",1:length(X)),"^2*",X),collapse = "+")),
                                              paste0(Y,' ~ a_0*1'),paste0(Y,' ~~ ',Y), sep = ' \n '), 
                                constraints ="a_c==1",
                                data = data.sem, model.type = "sem", estimator="ML")
      lcFitted <- lavaan::lavPredictY(fit.sem) - data.sem$const
      fit.sem  <- lavaan::summary(fit.sem,standardized = TRUE)
      fit.sem  <- fit.sem$pe[grepl("a_",fit.sem$pe$label),c("label","est")]
      fit.sem  <- fit.sem[!fit.sem$label %in% "a_c",]
      fit.sem  <- fit.sem[order(fit.sem$label),]
      est_coefc <- est_coef
      for(xx in fit.sem$label){
        est_coefc[xx] <- fit.sem[fit.sem$label %in% xx, "est"]^2
      }
      
      # list(est_coefc=est_coefc, lcFitted = lcFitted)
    }
    
    # Estimate stochatic frontier model with the constrained frontier
    data$lcFitted <- lcFitted 
    equations <- Fxn.equation_editor(outcome=ifelse(logDepVar,"lnY","Y"),
                                     data=data,FXN="lcFitted",slope_shifter=slope_shifter,
                                     intercept_shifters=intercept_shifters,ulist=ulist,vlist=vlist)
    
    sfc <- list(optStatus="")
    for(sf_gradtol in c(1e-6,1e-3)){
      for(sf_tol in c(1e-12,1e-6)){
        for(sf_method in c('nr','nm','bfgs','bhhh','cg','sann','ucminf','mla','sr1','sparse','nlminb')){
          if(!sfc$optStatus %in% "successful convergence "){
            tryCatch({
              if(is.null(equations$uequ)  &  is.null(equations$vequ)){
                sfc <- sfacross(formula = equations$prodfxn, udist = udist,
                                scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                                gradtol=sf_gradtol,tol=sf_tol)
              }
              if(!is.null(equations$uequ) &  is.null(equations$vequ)){
                sfc <- sfacross(formula = equations$prodfxn, udist = udist, uhet = equations$uequ, muhet = equations$uequ,
                                scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                                gradtol=sf_gradtol,tol=sf_tol)
              }
              if(is.null(equations$uequ)  & !is.null(equations$vequ)){
                sfc <- sfacross(formula = equations$prodfxn, udist = udist, vhet = equations$vequ,
                                scaling = scaling, S = 1, method = sf_method, logDepVar=logDepVar, data = data,
                                gradtol=sf_gradtol,tol=sf_tol)
              }
              if(!is.null(equations$uequ) & !is.null(equations$vequ)){
                sfc <- sfacross(formula = equations$prodfxn, uhet = equations$uequ, muhet = equations$uequ, 
                                vhet = equations$vequ, udist = udist,scaling = scaling, S = 1, method = sf_method, 
                                logDepVar=logDepVar, data = data,gradtol=sf_gradtol,tol=sf_tol)
              }
            }, error=function(e){})
          }
        }
      }
    }
    
    # summary(sfc)
    est_coefca    <- est_coefc*coef(sfc)["lcFitted"]
    est_coefca[1] <- est_coefca[1] + coef(sfc)["(Intercept)"]
    
    efc <- sfaR::efficiencies(sfc)
    #names(efc) <- paste0(names(efc),"_c")
    efc <- data.frame(data[row.names(efc),c(UID,"weights")],efc)
    efc$mlFitted <- sfc$dataTable$mlFitted
    if(logDepVar %in% TRUE){ efc$mlFitted <- exp(efc$mlFitted)}
    
    data <- data[row.names(efc),]
    
    rkc <- data.frame(data[c(UID,"weights")])
    if(logDepVar %in% TRUE){
      rkc$ybar  <- efc$mlFitted*exp(-efc$u)
      rkc$vrbr <- (rkc$ybar-exp(sf$dataTable$lnY))^2
    }
    if(!logDepVar %in% TRUE){
      rkc$ybar <- efc$mlFitted - efc$u
      rkc$vrbr <- (rkc$ybar-sf$dataTable$Y)^2
    }
    rkc$risk <- sqrt(rkc$vrbr)/rkc$ybar
    rkc <- rkc[c(UID,"weights","risk")]
    
    elc <- translogEla(xNames=xNames,data = data, coef = est_coefca,dataLogged =logDepVar)
    names(elc) <- paste0("el",1:ncol(elc))
    if(!TREND %in% TRUE){elc[,paste0("el",(ncol(elc)+1))] <- rowSums(elc)}
    if(TREND %in% TRUE ){elc[,paste0("el",(ncol(elc)+1))] <- rowSums(elc[1:(ncol(elc)-1)])}
    elc <- data.frame(data[c(UID,"weights")],elc)
    
    sfc_res <- dplyr::full_join(data.frame(CoefName=est_list,est_coefca=names(est_coefca)[1:length(est_list)],Estimate_c=est_coefca[1:length(est_list)]),
                                Fxn.sfaR_Summary(sfc),by="CoefName")
    sfc_res <- sfc_res[!sfc_res$CoefName %in% c("lcFitted"),]
    sfc_res$Estimate_c <- ifelse(sfc_res$Estimate_c %in% NA,sfc_res$Estimate,sfc_res$Estimate_c)
    sfc_res <- sfc_res[c("CoefName","Estimate_c","Pvalue")]
    names(sfc_res) <- c("CoefName","Estimate","Pvalue")
    
    if(names(FXN) %in% c("TL","QD")){
      curv_c <- mean(translogCheckCurvature(xNames = xNames,data=data, est_coefca, convexity = F, quasi = TRUE ,dataLogged =logDepVar)$obs,na.rm=T)
      mono_c <- mean(translogCheckMono(xNames = xNames,data = data, coef = est_coefca,dataLogged =logDepVar)$obs,na.rm=T)
    }
    
    if(names(FXN) %in% c("CD","LN")){
      curv_c <- mono_c <- as.numeric(mean(as.numeric(est_coefca[2:(nX+1)] > 0),na.rm=T) %in% 1)
    }
    
    mc_c <- data.frame(CoefName=c("mono","curv"),Estimate=c(mono_c,curv_c),StdError=NA,Zvalue=NA,Pvalue=NA)
    
  }else{
    sfc_res <- sf_res
    mc_c    <- mc
    elc     <- el
    efc     <- ef
    rkc     <- rk
  }
  
  sf_res$restrict  <- "Unrestricted"
  sfc_res$restrict <- "Restricted"
  mc$restrict   <- "Unrestricted"
  mc_c$restrict <- "Restricted"
  final_sf <- as.data.frame(data.table::rbindlist(list(sf_res,sfc_res,mc,mc_c), fill = TRUE))
  
  el$restrict  <- "Unrestricted"
  elc$restrict <- "Restricted"
  final_el <- as.data.frame(data.table::rbindlist(list(el,elc), fill = TRUE))
  
  ef$restrict  <- "Unrestricted"
  efc$restrict <- "Restricted"
  final_ef <- as.data.frame(data.table::rbindlist(list(ef,efc), fill = TRUE))
  
  rk$restrict  <- "Unrestricted"
  rkc$restrict <- "Restricted"
  final_rk <- as.data.frame(data.table::rbindlist(list(rk,rkc), fill = TRUE))
  
  res <- list(sf=final_sf,ef=final_ef,el=final_el,rk=final_rk)
  
  return(res)
}

#---------------------------------------------
# MSF Work Horse                           ####
Fxn.MSF_WorkHorse_FT <- function(
    data ,
    yvar,
    xlist,
    ulist= NULL,
    vlist=NULL,
    wvar= NULL,
    slope_shifter= "NONE",
    intercept_shifters= NULL,
    f,
    d,
    UID,
    TREND=FALSE,
    tvar=NULL,
    nnm= NULL,
    ulistM=NULL,
    intercept_shiftersM=NULL){
  # data <- DATA #[DATA$FMTYPOL %in% 7,]
  #---------------------------------------------------
  # Preliminaries                                  ####
  data <- data[!data[,wvar] %in% 0,]
  
  if(!is.null(tvar)){
    data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,tvar]))))
    TechList  <- unique(data$Tech)
    
    TechKey <- unique(data[c("Tech",tvar)])
    TechKey <- TechKey[order(TechKey$Tech),]
  }
  #---------------------------------------------------
  # SF Estimation [Naive TE]                       ####
  cat(crayon::green("SF Estimation [Naive TE]",Sys.time()),fill=T)
  sf_Naive <- Fxn.SF_WorkHorse_FT(
    data=data,
    yvar=yvar,
    xlist=xlist,wvar=wvar,d=d,f=f,
    slope_shifter=slope_shifter,
    intercept_shifters=intercept_shifters,
    ulist=ulist,
    vlist=vlist,
    UID=UID,
    TREND=TREND)
  if(is.null(tvar)){ 
    score <- sf_Naive$ef[names(sf_Naive$ef)[names(sf_Naive$ef) %in% c(UID,"teBC","teJLMS","teMO","restrict")]]
    score <- score %>% tidyr::gather(estType, TE, names(score)[!names(score) %in% c(UID,"restrict")])
    score$sample <- "unmatched"
    score <- dplyr::inner_join(data[c(UID,wvar)],score,by=UID)
    score$Tech <- -999
    names(score)[names(score) %in% wvar] <- "weights"
  }
  # cc <- sf_Naive$ef
  # cc <- cc[cc$CoefName %in% "Nobs",]
  # cc
  #---------------------------------------------------
  # MSF Estimation                                 ####
  sf_Group <- NULL;mflist<-NULL
  if(!is.null(tvar)){
    #-------------------------------------------------
    # Group SF Estimation [Group TE]               ####
    
    sf_Group <- future_lapply(
      TechList,
      function(tech){
        DONE <- NULL
        #tryCatch({ 
          # tech <- 1
          cat(crayon::green("Group SF",tech,Sys.time()),fill=T)
          
          sfi <- Fxn.SF_WorkHorse_FT(
            data=data[data$Tech %in% tech,],
            yvar=yvar,
            xlist=xlist,wvar=wvar,d=d,f=f,
            slope_shifter=slope_shifter,
            intercept_shifters=intercept_shifters,
            ulist=ulist,
            vlist=vlist,
            UID=UID,
            TREND=TREND)
          
          DONE <- sfi
        #}, error=function(e){})
        return(DONE)})
    
    sf_Group <- Filter(Negate(is.null), sf_Group)
    
    #-------------------------------------------------
    # Meta SF Estimation unmatched [TGR]           ####
    
    cat(crayon::green("unmatched Meta SF",Sys.time()),fill=T)
    
    mf.data <- as.data.frame(
      data.table::rbindlist(
        lapply(
          1:length(sf_Group),
          function(tech){
            # tech <- 1
            mf.data <- sf_Group[[tech]]$ef
            mf.data$Yhat <- mf.data$mlFitted
            mf.data <- mf.data[mf.data$restrict %in% "Restricted",]
            return(mf.data[c(UID,"Yhat")])
          }), fill = TRUE))
    
    mf.data <- dplyr::inner_join(data,mf.data,by=UID)
    
    if(is.null(ulistM)){
      ulistM <- ulist 
    }
    
    if(is.null(intercept_shiftersM)){
      intercept_shiftersM <- intercept_shifters
    }
    
    mflist <- list(
      unmatched=Fxn.SF_WorkHorse_FT(
        data=mf.data,
        yvar="Yhat",
        xlist=xlist,wvar=wvar,d=d,f=f,
        slope_shifter=slope_shifter,
        intercept_shifters=intercept_shiftersM,
        ulist=ulistM,
        UID=UID,
        TREND=TREND))
    
    #-------------------------------------------------
    # Meta SF Estimation matched [TGR]             ####
    if(!is.null(nnm)){
      
      mspecs_path    <- "results/matching/"
      mspecs         <- readRDS("results/mspecs.rds")
      mspecs_optimal <- readRDS("results/mspecs_optimal.rds")[c("ARRAY","method","distance","link")]
      mspecs_fullset <- mspecs[!grepl("linear",mspecs$link),]
      mspecs_fullset <- mspecs_fullset[mspecs_fullset$boot %in% 0,c("ARRAY","method","distance","link")] #!!!
      
      m.specs <- mspecs[mspecs$boot %in% 0,] #!!!
      m.specs <- m.specs[m.specs$method %in% mspecs_fullset$method,]
      m.specs <- m.specs[m.specs$distance %in% mspecs_fullset$distance,]
      m.specs <- m.specs[m.specs$link %in% mspecs_fullset$link,]
      m.specs$name <- ifelse(m.specs$link %in% NA,m.specs$distance,m.specs$link)
      
      if(nnm %in% "optimal"){
        m.specs <- m.specs[m.specs$method %in% mspecs_optimal$method,]
        m.specs <- m.specs[m.specs$distance %in% mspecs_optimal$distance,]
        m.specs <- m.specs[m.specs$link %in% mspecs_optimal$link,]
      }
      
      for(mm in c(1:nrow(m.specs))){
        tryCatch({
          # mm <- 1
          cat(crayon::green("matched Meta SF-",as.character(m.specs$name[mm]),Sys.time()),fill=T)
          mfm.data <- dplyr::inner_join(unique(readRDS(paste0(mspecs_path,"Match",stringr::str_pad(m.specs$ARRAY[mm],4,pad="0"),".rds"))$md),
                                        mf.data,by=c("Surveyx","EaId", "HhId", "Mid","UID"))
          mfm <- Fxn.SF_WorkHorse_FT(
            data=mfm.data,
            yvar="Yhat",
            xlist=xlist,wvar=wvar,d=d,f=f,
            slope_shifter=slope_shifter,
            intercept_shifters=intercept_shiftersM,
            ulist=ulist,
            UID=UID,
            TREND=TREND)
          
          mflist[[as.character(m.specs$name[mm])]] <- mfm
          rm(mfm,mfm.data)
        }, error=function(e){})
      }
    }
    #-------------------------------------------------
    # Calculate scors                              ####
    cat(crayon::green("Calculate scors",Sys.time()),fill=T)
    TE0 <- sf_Naive$ef[names(sf_Naive$ef)[names(sf_Naive$ef) %in% c(UID,"restrict","teBC","teJLMS","teMO")]]
    TE0 <- TE0 %>% tidyr::gather(estType, TE0, names(TE0)[!names(TE0) %in% c(UID,"restrict")])
    
    TE <- as.data.frame(data.table::rbindlist(lapply(1:length(sf_Group),function(tech){sf_Group[[tech]]$ef}), fill = TRUE))
    TE <- TE[names(TE)[names(TE) %in% c(UID,"restrict","teBC","teJLMS","teMO")]]
    TE <- TE %>% tidyr::gather(estType, TE, names(TE)[!names(TE) %in% c(UID,"restrict")])
    
    score <- as.data.frame(
      data.table::rbindlist(
        lapply(
          names(mflist),
          function(sample){
            DONE <- NULL
            tryCatch({ 
              # sample <- "euclidean"
              smf_score <- mflist[[sample]]$ef[names( mflist[[sample]]$ef)[names(mflist[[sample]]$ef) %in% c(UID,"restrict","teBC","teJLMS","teMO","weights")]]
              smf_score <- smf_score[smf_score$restrict %in% "Restricted",names(smf_score)[names(smf_score) %in% c(UID,"weights","teBC","teJLMS","teMO")]]
              smf_score <- smf_score %>% tidyr::gather(estType, TGR, names(smf_score)[!names(smf_score) %in% c(UID,"weights")])
              
              smf_score <- dplyr::inner_join(dplyr::inner_join(TE0,TE,by=c(UID,"restrict","estType")) ,smf_score,by=c(UID,"estType"))
              smf_score$MTE <- smf_score$TE*smf_score$TGR
              smf_score$sample <- sample
              DONE <- smf_score
            }, error=function(e){})
            return(DONE)
          }), fill = TRUE))
    
    score <- dplyr::inner_join(data[c(UID,"Tech")],score,by=UID)
    
    #-------------------------------------------------
  }
  #---------------------------------------------------
  # Distribution bars- Scores                      ####
  cat(crayon::green("Distribution bars- Scores",Sys.time()),fill=T)
  if(!is.null(tvar)){
    dataFrq <- score %>% tidyr::gather(type, value, c("TE0","TE","TGR","MTE"))
    dataFrq <- dataFrq[!dataFrq$value %in% c(NA,Inf,-Inf,NaN),]
    dataFrq00 <- dataFrq
    dataFrq00$Tech <- -999
    dataFrq <- rbind(dataFrq,dataFrq00)
  }else{
    dataFrq <- score %>% tidyr::gather(type, value, c("TE"))
    dataFrq <- dataFrq[!dataFrq$value %in% c(NA,Inf,-Inf,NaN),]
  }
  
  dataFrq00 <- dataFrq
  dataFrq00$Survey <- "GLSS0"
  dataFrq <- rbind(dataFrq,dataFrq00)
  rm(dataFrq00)
  
  dataFrq$range <- cut(dataFrq$value,seq(0,1,0.05))
  dataFrq$count <- 1
  
  count <- dataFrq %>% group_by(Survey,sample,Tech,type,estType,restrict,range) %>%
    summarise(count = sum(count)) %>% as.data.frame(.)
  
  count_sum <- dataFrq %>% group_by(Survey,sample,Tech,type,estType,restrict) %>%
    summarise(count_sum = sum(count)) %>% as.data.frame(.)
  
  weights <- dataFrq %>% group_by(Survey,sample,Tech,type,estType,restrict,range) %>%
    summarise(weights = sum(weights)) %>% as.data.frame(.)
  
  weights_sum <- dataFrq %>% group_by(Survey,sample,Tech,type,estType,restrict) %>%
    summarise(weights_sum = sum(weights)) %>% as.data.frame(.)
  
  dataFrq <- dplyr::inner_join(dplyr::inner_join(count,count_sum,by=c("Survey","sample","Tech","type","estType","restrict")),
                               dplyr::inner_join(weights,weights_sum,by=c("Survey","sample","Tech","type","estType","restrict")),
                               by=c("Survey","sample","Tech","type","estType","restrict","range"))
  
  dataFrq$est_weight <- dataFrq$weights/dataFrq$weights_sum
  dataFrq$est_count  <- dataFrq$count/dataFrq$count_sum
  dataFrq$Frqlevel   <- as.integer(dataFrq$range)
  ef_dist <- dataFrq[c("Survey","sample","Tech","type","estType","restrict","range","Frqlevel","est_count","est_weight")]
  
  # check <- unique(ef_dist[c("sample","Tech","type","estType")])
  # table(check$type,check$sample)
  
  rm(dataFrq)
  #---------------------------------------------------
  # Summary Scores                                 ####
  cat(crayon::green("Summary Scores",Sys.time()),fill=T)
  if(!is.null(tvar)){
    Estescors <- score %>% tidyr::gather(type, value, c("TE0","TE","TGR","MTE"))
    Estescors <- Estescors[!Estescors$value %in% c(NA,Inf,-Inf,NaN),]
    Estescors00 <- Estescors
    Estescors00$Tech <- -999
    Estescors <- rbind(Estescors,Estescors00)
  }else{
    Estescors <- score %>% tidyr::gather(type, value, c("TE"))
    Estescors <- Estescors[!Estescors$value %in% c(NA,Inf,-Inf,NaN),]
  }
  
  Estescors00 <- Estescors
  Estescors00$Survey <- "GLSS0"
  Estescors <- rbind(Estescors,Estescors00)
  rm(Estescors00)
  
  Estescors <- Estescors %>%
    group_by(Survey, sample, Tech, type, estType, restrict) %>%
    summarise(wmean  = weighted.mean(value, weights, na.rm = TRUE),
              mean   = mean(value,na.rm = TRUE),
              median = median(value,na.rm = TRUE),
              mode   = mode(value, na.rm = TRUE)) %>%
    as.data.frame()
  
  Estescors <- Estescors %>% tidyr::gather(stat, Estimate, c("wmean","mean","median","mode"))
  Estescors$CoefName <- "efficiency"
  
  if(!is.null(tvar)){
    EstescorsGAP <- Estescors[Estescors$Tech %in% TechKey$Tech,]
    EstescorsGAP <- dplyr::inner_join(
      EstescorsGAP[!EstescorsGAP$Tech %in% min(TechKey$Tech),c("sample","Survey","type","estType","restrict","Tech","stat","Estimate")],
      doBy::summaryBy(list("Estimate",c("sample","Survey","type","estType","restrict","stat")),
                      data=EstescorsGAP[EstescorsGAP$Tech %in% min(TechKey$Tech),],FUN=c(mean),na.rm=T),
      by=c("sample","Survey","type","estType","restrict","stat"))
    
    EstescorsGAP$efficiencyGap_lvl <- EstescorsGAP$Estimate - EstescorsGAP$Estimate.mean
    EstescorsGAP$efficiencyGap_pct <- ((EstescorsGAP$efficiencyGap_lvl/abs(EstescorsGAP$Estimate.mean)))*100
    EstescorsGAP <- EstescorsGAP[c("sample","Survey","type","estType","restrict","Tech","stat","efficiencyGap_lvl","efficiencyGap_pct")]
    EstescorsGAP <- EstescorsGAP %>%  tidyr::gather(CoefName, Estimate, c("efficiencyGap_lvl","efficiencyGap_pct"))
    Estescors <- rbind(Estescors,EstescorsGAP)
  }
  
  # check <- unique(Estescors[c("sample","Tech","type","estType")]);table(check$type,check$sample)
  
  #---------------------------------------------------
  # Summary Estimates                              ####
  cat(crayon::green("Summary Estimates",Sys.time()),fill=T)
  Estimates <- sf_Naive$sf
  Estimates$Tech <- -999
  Estimates$sample <- "unmatched"
 
  if(!is.null(tvar)){
    Estimates <- list(Estimates)
    Estimates[[length(Estimates)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$sf;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Estimates[[length(Estimates)+1]] <- data.frame(
      data.table::rbindlist(
        lapply(names(mflist),function(sample){
          DONE <- NULL
          tryCatch({ 
            smf <- mflist[[sample]]$sf; smf$Tech <- 999
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Estimates <- as.data.frame(data.table::rbindlist(Estimates, fill = TRUE))
    
    lrtest <- Estimates[Estimates$CoefName %in% c("Nobs","nXvar","nuZUvar","nvZVvar","mlLoglik"),]
    lrtest$CoefName <- ifelse(lrtest$CoefName %in% c("nXvar","nuZUvar","nvZVvar"),"npar",lrtest$CoefName)
    lrtest <- doBy::summaryBy(Estimate~Tech+CoefName+sample+restrict,data=lrtest,FUN=sum,keep.names = T,na.rm=T)
    lrtest <- lrtest[order(lrtest$sample,lrtest$Tech,lrtest$CoefName),]
    
    LL_Naive <- lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% -999),c("restrict","Estimate")]
    DF_Naive <- lrtest[(lrtest$CoefName %in% "npar"     & lrtest$Tech %in% -999),c("restrict","Estimate")]
    
    LL_Group <- doBy::summaryBy(Estimate~type+restrict,FUN=sum,keep.names = T,na.rm=T,
                                data=lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% TechList),])
    DF_Group <- doBy::summaryBy(Estimate~type+restrict,FUN=sum,keep.names = T,na.rm=T,
                                data=lrtest[(lrtest$CoefName %in% "npar" & lrtest$Tech %in% TechList),])
    
    LL_Meta <- lrtest[(lrtest$CoefName %in% "mlLoglik" & lrtest$Tech %in% 999),c("restrict","Estimate","sample")]
    DF_Meta <- lrtest[(lrtest$CoefName %in% "npar"     & lrtest$Tech %in% 999),c("restrict","Estimate","sample")]
    
    names(LL_Naive)[names(LL_Naive) %in% "Estimate"] <- "LL_Naive"
    names(DF_Naive)[names(DF_Naive) %in% "Estimate"] <- "DF_Naive"
    names(LL_Group)[names(LL_Group) %in% "Estimate"] <- "LL_Group"
    names(DF_Group)[names(DF_Group) %in% "Estimate"] <- "DF_Group"
    names(LL_Meta)[names(LL_Meta) %in% "Estimate"] <- "LL_Meta"
    names(DF_Meta)[names(DF_Meta) %in% "Estimate"] <- "DF_Meta"
    
    lrtest <- dplyr::inner_join(LL_Meta,DF_Meta,by=c("sample","restrict"))
    lrtest <- dplyr::inner_join(LL_Group,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(DF_Group,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(LL_Naive,lrtest,by=c("restrict"))
    lrtest <- dplyr::inner_join(DF_Naive,lrtest,by=c("restrict"))
    
    lrtest$LL1 <- lrtest$LL_Group + lrtest$LL_Meta
    lrtest$DF1 <- lrtest$DF_Group + lrtest$DF_Meta
    lrtest$LL0 <- lrtest$LL_Naive
    lrtest$DF0 <- lrtest$DF_Naive
    lrtest$Estimate <- -2*(lrtest$LL0-lrtest$LL1)
    lrtest$DF <- lrtest$DF1-lrtest$DF0
    lrtest$Pvalue <- 1-pchisq(lrtest$Estimate, df=lrtest$DF)
    
    lrtest <- lrtest[c("sample","restrict","Estimate","Pvalue")]
    lrtest$CoefName <- "LRT"
    lrtest$Tech <- 999
    
    Estimates <- as.data.frame(data.table::rbindlist(list(Estimates,lrtest), fill = TRUE))
    
  }
  
  # check <- unique(Estimates[c("sample","Tech","CoefName")]);table(check$CoefName,check$sample)
  
  #---------------------------------------------------
  # Summary Elasticity                             ####
  cat(crayon::green("Summary Elasticity",Sys.time()),fill=T)
  Elasticity <- sf_Naive$el
  Elasticity$Tech <- -999
  Elasticity$sample <- "unmatched"

  if(!is.null(tvar)){
    Elasticity <- list(Elasticity)
    Elasticity[[length(Elasticity)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$el;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Elasticity[[length(Elasticity)+1]] <- as.data.frame(
      data.table::rbindlist(
        lapply(names(mflist),function(sample){
          DONE <- NULL
          tryCatch({ 
            # sample <- "euclidean"
            smf <- mflist[[sample]]$el; smf$Tech <- 999
            
            sfx <- as.data.frame(data.table::rbindlist(
              lapply(1:length(sf_Group),function(tech){sf <- sf_Group[[tech]]$el;sf$Tech <- TechList[tech]
              return(sf)
              }), fill = TRUE))
            
            sfx <- dplyr::inner_join(smf[c(UID,"weights")],sfx[c(UID,"restrict","Tech",names(sfx)[grepl("el",names(sfx))])],by=UID)
            
            smf <- as.data.frame(data.table::rbindlist(list(smf,sfx), fill = TRUE))
            
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Elasticity <- as.data.frame(data.table::rbindlist(Elasticity, fill = TRUE))
  }
  
  Elasticity00 <- Elasticity
  Elasticity00$Survey <- "GLSS0"
  Elasticity <- rbind(Elasticity,Elasticity00)
  rm(Elasticity00)
  
  Elasticity <- Elasticity %>% 
    tidyr::gather(
      input, value, names(Elasticity)[grepl("el",names(Elasticity))])
  Elasticity$value <- as.numeric(as.character(Elasticity$value))
  Elasticity <- Elasticity[!Elasticity$value %in% c(NA,Inf,-Inf,NaN),]
  
  Elasticity_sample <- Elasticity
  
  Elasticity <- Elasticity %>%
    group_by(Survey, sample, Tech, input, restrict) %>%
    summarise(wmean  = weighted.mean(value, weights, na.rm = TRUE),
              mean   = mean(value,na.rm = TRUE),
              median = median(value,na.rm = TRUE),
              mode   = mode(value, na.rm = TRUE)) %>%
    as.data.frame()
  
  Elasticity <- Elasticity %>% tidyr::gather(stat, Estimate, c("wmean","mean","median","mode"))
  Elasticity$CoefName <- "elasticity"
  
  if(!is.null(tvar)){
    ElasticityGAP <- Elasticity[Elasticity$Tech %in% TechKey$Tech,]
    ElasticityGAP <- dplyr::inner_join(
      ElasticityGAP[!ElasticityGAP$Tech %in% min(TechKey$Tech),c("sample","Survey","input","restrict","Tech","stat","Estimate")],
      doBy::summaryBy(list("Estimate",c("sample","Survey","input","restrict","stat")),
                      data=ElasticityGAP[ElasticityGAP$Tech %in% min(TechKey$Tech),],FUN=c(mean),na.rm=T),
      by=c("sample","Survey","input","restrict","stat"))
    
    ElasticityGAP$elasticityGap_lvl <- ElasticityGAP$Estimate - ElasticityGAP$Estimate.mean
    ElasticityGAP$elasticityGap_pct <- ((ElasticityGAP$elasticityGap_lvl/abs(ElasticityGAP$Estimate.mean)))*100
    ElasticityGAP <- ElasticityGAP[c("sample","Survey","input","Tech","restrict","stat","elasticityGap_lvl","elasticityGap_pct")]
    ElasticityGAP <- ElasticityGAP %>%  tidyr::gather(CoefName, Estimate, c("elasticityGap_lvl","elasticityGap_pct"))
    Elasticity <- rbind(Elasticity,ElasticityGAP)
  }
  
  # check <- unique(Elasticity[Elasticity$Survey %in% 0,c("sample","Tech","input","stat","CoefName","Survey")]);table(check$input,check$sample)
  
  #---------------------------------------------------
  # Summary Risk                                   ####
  cat(crayon::green("Summary Risk",Sys.time()),fill=T)
  Risk <- sf_Naive$rk
  Risk$Tech <- -999
  Risk$sample <- "unmatched"
  
  if(!is.null(tvar)){
    Risk <- list(Risk)
    Risk[[length(Risk)+1]] <- as.data.frame(data.table::rbindlist(
      lapply(1:length(sf_Group),function(tech){
        sf <- sf_Group[[tech]]$rk;sf$Tech <- TechList[tech];sf$sample <- "unmatched"
        return(sf)
      }), fill = TRUE))
    
    Risk[[length(Risk)+1]] <- as.data.frame(
      data.table::rbindlist(
        lapply(names(mflist)[!names(mflist) %in% "unmatched"],function(sample){
          DONE <- NULL
          tryCatch({ 
            # sample <- "unmatched"
            smf <- mflist[[sample]]$ef[UID]
            smf <- dplyr::inner_join(smf,Risk[[2]],by=UID)
            smf$sample <- sample; DONE <- smf
          }, error=function(e){})
          return(DONE)}), fill = TRUE))
    
    Risk <- as.data.frame(data.table::rbindlist(Risk, fill = TRUE))
    
  }
  
  Risk00 <- Risk
  Risk00$Survey <- "GLSS0"
  Risk <- rbind(Risk,Risk00)
  rm(Risk00)
  
  Risk <- Risk[!Risk$risk %in% c(NA,NaN,Inf,-Inf),]
  
  Risk_sample <- Risk
  
  Risk <- Risk %>%
    group_by(Survey, sample, Tech, restrict) %>%
    summarise(wmean  = weighted.mean(risk, weights, na.rm = TRUE),
              mean   = mean(risk,na.rm = TRUE),
              median = median(risk,na.rm = TRUE),
              mode   = mode(risk, na.rm = TRUE)) %>%
    as.data.frame()
  
  Risk <- Risk %>% tidyr::gather(stat, Estimate, c("wmean","mean","median","mode"))
  Risk$CoefName <- "risk"
  
  if(!is.null(tvar)){
    RiskGAP <- Risk[Risk$Tech %in% TechKey$Tech,]
    RiskGAP <- dplyr::inner_join(
      RiskGAP[!RiskGAP$Tech %in% min(TechKey$Tech),c("sample","Survey","restrict","Tech","stat","Estimate")],
      doBy::summaryBy(list("Estimate",c("sample","Survey","restrict","stat")),
                      data=RiskGAP[RiskGAP$Tech %in% min(TechKey$Tech),],FUN=c(mean),na.rm=T),
      by=c("sample","Survey","restrict","stat"))
    
    RiskGAP$riskGap_lvl <- RiskGAP$Estimate - RiskGAP$Estimate.mean
    RiskGAP$riskGap_pct <- ((RiskGAP$riskGap_lvl/abs(RiskGAP$Estimate.mean)))*100
    RiskGAP <- RiskGAP[c("sample","Survey","restrict","Tech","stat","riskGap_lvl","riskGap_pct")]
    RiskGAP <- RiskGAP %>%  tidyr::gather(CoefName, Estimate, c("riskGap_lvl","riskGap_pct"))
    Risk <- rbind(Risk,RiskGAP)
  }
  # check <- unique(Risk[Risk$Survey %in% 0,c("sample","Tech","input","stat","CoefName","Survey")]);table(check$input,check$sample)
  
  #---------------------------------------------------
  # Distribution bars- Risk                        ####
  cat(crayon::green("Distribution bars- Risk",Sys.time()),fill=T)
  dataFrq <- Risk_sample[!Risk_sample$risk %in% c(NA,Inf,-Inf,NaN),]
  
  dataFrq$range <- cut(dataFrq$risk,seq(0,2,0.05))
  dataFrq$count <- 1
  
  dataFrq00 <- dataFrq
  dataFrq00$Survey <- "GLSS0"
  dataFrq <- rbind(dataFrq,dataFrq00)
  rm(dataFrq00)
  
  count <- dataFrq %>% group_by(Survey,sample,Tech,restrict,range) %>%
    summarise(count = sum(count)) %>% as.data.frame(.)
  
  count_sum <- dataFrq %>% group_by(Survey,sample,Tech,restrict) %>%
    summarise(count_sum = sum(count)) %>% as.data.frame(.)
  
  weights <- dataFrq %>% group_by(Survey,sample,Tech,restrict,range) %>%
    summarise(weights = sum(weights)) %>% as.data.frame(.)
  
  weights_sum <- dataFrq %>% group_by(Survey,sample,Tech,restrict) %>%
    summarise(weights_sum = sum(weights)) %>% as.data.frame(.)
  
  dataFrq <- dplyr::inner_join(dplyr::inner_join(count,count_sum,by=c("Survey","sample","Tech","restrict")),
                               dplyr::inner_join(weights,weights_sum,by=c("Survey","sample","Tech","restrict")),
                               by=c("Survey","sample","Tech","restrict","range"))
  
  dataFrq$est_weight <- dataFrq$weights/dataFrq$weights_sum
  dataFrq$est_count  <- dataFrq$count/dataFrq$count_sum
  dataFrq$Frqlevel   <- as.integer(dataFrq$range)
  
  rk_dist <- dataFrq[c("Survey","sample","Tech","restrict","range","Frqlevel","est_count","est_weight")]
  rk_dist$type <- "risk"
  
  rm(dataFrq)
  #---------------------------------------------------
  # Summary and export                             ####
  cat(crayon::green("Summary and export",Sys.time()),fill=T)
  res <- list(
    sf_estm = Estimates,
    ef_mean = Estescors,
    ef_dist = ef_dist,
    rk_dist = rk_dist,
    el_mean = Elasticity,
    rk_mean = Risk,
    el_samp = Elasticity_sample[!Elasticity_sample$Survey %in% "GLSS0",],
    ef_samp = score,
    rk_samp = Risk_sample[!Risk_sample$Survey %in% "GLSS0",])
  #---------------------------------------------------
  return(res)
  #---------------------------------------------------
}
#---------------------------------------------
# draw estimations                         ####

Fxn_draw_estimations <- function(
    draw,
    drawlist,
    surveyy=FALSE,
    data ,
    yvar,
    xlist,
    ulist= NULL,
    vlist=NULL,
    wvar= NULL,
    slope_shifter= "NONE",
    intercept_shifters= NULL,
    f,
    d,
    UID,
    TREND=FALSE,
    tvar=NULL,
    nnm= NULL,
    ulistM=NULL,
    intercept_shiftersM=NULL,
    disagscors_list=NULL){
  DONE <- NULL
  tryCatch({ 
    # draw <- 0
    #print(draw)
    #---------------------------------------------
    # Data Preparation                         ####
    data <- data[!data$EaId %in% c(t(drawlist[drawlist$ID %in% draw,2:ncol(drawlist)])),]
    #---------------------------------------------
    # Survey estimations                       ####
    cat(crayon::green("Survey estimations",Sys.time()),fill=T)
    data$Surveyy <- data$Surveyx
    if(surveyy %in% FALSE){
      data$Surveyy <- "GLSS0"
    } 
    
    res <- lapply(
      unique(data$Surveyy),
      function(glss,draw){
        tryCatch({ 
          # glss <-"GLSS0"
          res <- Fxn.MSF_WorkHorse_FT(
            data= data[data[,"Surveyy"] %in% glss,],yvar= yvar,xlist= xlist,
            slope_shifter= slope_shifter,intercept_shifters = intercept_shifters,intercept_shiftersM=intercept_shiftersM,
            ulist= ulist,ulistM=ulistM,vlist=vlist,wvar= wvar,f= f,d= d,UID= UID,tvar= tvar,nnm= nnm,TREND= TREND)
          
          function(){
            Main <- res$ef_mean
            Main <- Main[Main$Survey %in% "GLSS0",]
            Main <- Main[!Main$sample %in% "unmatched",]
            Main <- Main[Main$stat %in% "wmean",]
            Main <- Main[Main$CoefName %in% "efficiencyGap_pct",]
            Main <- Main[Main$restrict %in% "Restricted",]
            Main <- Main[Main$estType %in% "teBC",]
            Main[Main$type %in% "TGR",c("sample","type","Tech","Estimate")]
            Main[Main$type %in% "TE",c("sample","type","Tech","Estimate")]
            Main[Main$type %in% "MTE",c("sample","type","Tech","Estimate")]
          }
          
          
          for(outcome in c("sf_estm","el_mean","ef_mean","rk_mean","ef_dist","rk_dist","el_samp","ef_samp","rk_samp")){
            tryCatch({ res[[outcome]][,"Surveyy"] <- glss ;res[[outcome]][,"draw"] <- draw }, error=function(e){})
          }
          
          return(res)
        }, error = function(e){return(NULL)})},draw=draw)
    
    res <- lapply(
      c("sf_estm","el_mean","ef_mean","rk_mean","ef_dist","rk_dist","el_samp","ef_samp","rk_samp"),
      function(outcome){return(as.data.frame(data.table::rbindlist(lapply(1:length(res),function(glss){return(res[[glss]][[outcome]])}), fill = TRUE)))})
    names(res) <- c("sf_estm","el_mean","ef_mean","rk_mean","ef_dist","rk_dist","el_samp","ef_samp","rk_samp")

    #---------------------------------------------
    # Disaggregated score summary              #### 
    cat(crayon::green("Disaggregated score summary",Sys.time()),fill=T)
    disagscors <- NULL
    if(! is.null(disagscors_list)){
      disagscors <- as.data.frame(
        data.table::rbindlist(
          future_lapply(
            disagscors_list,
            function(disagscors_var,scors,data){
              tryCatch({ 
                
                # disagscors_var <- disagscors_list[1];scors <- res$ef_samp
                
                scors00 <- scors
                scors00$Survey <- "GLSS0"
                scors <- rbind(scors,scors00)
                rm(scors00)
                
                disagscors <- scors %>%  tidyr::gather(input, value, c("TE0","TE","TGR","MTE"))
                disagscors <- disagscors[!disagscors$value %in% c(NA,Inf,-Inf,NaN),]
                disagscors <- disagscors[disagscors$estType %in% c("teJLMS","teMO","teBC","teFTu","teFTm"),]
                
                disagscors <- dplyr::inner_join(disagscors,data[c("UID", "Survey", "CropID", "HhId", "EaId", "Mid",disagscors_var)],
                                                by=c("UID", "Survey", "CropID", "HhId", "EaId", "Mid"))
                disagscors$disagscors_level <- disagscors[,disagscors_var]
                disagscors <- disagscors[!disagscors$disagscors_level %in% c(NA,Inf,-Inf,NaN),]
                
                disagscors0 <- disagscors[disagscors$input %in% "TE0",]
                disagscors0$Tech  <- -999
                disagscors <- rbind(disagscors0,disagscors[! disagscors$input %in% "TE0",])
                rm(disagscors0)
                worklist <- unique(disagscors[c("sample","estType","Tech","input","restrict")])
                
                disagscors <- as.data.frame(
                  data.table::rbindlist(
                    lapply(
                      1:nrow(worklist),
                      function(kk,worklist,disagscors){
                        # kk <- 1
                        score.data <- dplyr::inner_join(worklist[kk,],disagscors,by=names(worklist))
                        score.data <- score.data[!score.data$value %in% c(NA,NaN,Inf,-Inf),]
                        score.data0 <- score.data
                        score.data0$Survey <- "GLSS0"
                        score.data <- rbind(score.data0,score.data)
                        rm(score.data0)
                        score.data <- doBy::summaryBy(
                          list("value",c("Survey","disagscors_level")),
                          FUN=function(x){
                            w      <- score.data$Weight
                            wmean  <- sum(x * w) / sum(w)
                            mode   <- function(x,na.rm = T) {ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]}
                            mean   <- mean(x,na.rm=T)
                            median <- median(x,na.rm=T)
                            mode   <- mode(x,na.rm=T)
                            stat   <- c(wmean,mean,median,mode)
                            names(stat) <- c("wmean","mean","median","mode")
                            return(stat)},data=score.data)
                        names(score.data) <- gsub("value.","", names(score.data))
                        score.data <- data.frame(worklist[kk,],score.data)
                        return(score.data)
                      },worklist=worklist,disagscors=disagscors), fill = TRUE))
                
                disagscors$disagscors_var <- ifelse(grepl("CROP_",disagscors_var),"CROP",disagscors_var)
                disagscors$CoefName <- "disag_efficiency"
                return(disagscors)
              }, error = function(e){return(NULL)})
              
              return(DONE)
            },scors=res$ef_samp,data=data), fill = TRUE))
      disagscors$draw    <- draw
    }
    res$disagscors <- disagscors
    #---------------------------------------------
    # Export                                   ####
    if(!draw %in% 0){res[["el_samp"]] <- NULL; res[["ef_samp"]] <- NULL; res[["rk_samp"]] <- NULL}
    return(res)
    #---------------------------------------------
  }, error = function(e){return(NULL)})
  }

#---------------------------------------------
# draw summary                             ####
Fxn.draw_summary <- function(res,TechKey){
  #---------------------------------------------------
  # Summary Estimates                              ####
  sf_estm <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$sf_estm)}), fill = TRUE))
  sf_estm <- sf_estm[!sf_estm$Estimate %in% c(NA,Inf,-Inf,NaN),]
  sf_estm$Survey <- sf_estm$Surveyy

  if(length(unique(sf_estm$Survey))>1){
    sf_estm00 <- sf_estm
    sf_estm00$Survey <- "GLSS0"
    sf_estm <- rbind(sf_estm00,sf_estm)
    rm(sf_estm00)
  }
  
  sf_estmX <- sf_estm[sf_estm$CoefName %in% "Nobs",]
  sf_estmX <- sf_estmX[sf_estmX$draw %in% 0,]
  sf_estmX <- sf_estmX[sf_estmX$restrict %in% "Unrestricted",]
  sf_estmX <- sf_estmX[sf_estmX$Tech %in% 999,]
  sf_estmX <- sf_estmX[sf_estmX$sample %in% "unmatched",]
  
  sf_estm <- dplyr::inner_join(
    sf_estm[sf_estm$draw %in% 0,c("Survey","CoefName","Tech","sample","restrict","Estimate","StdError","Zvalue","Pvalue")],
    doBy::summaryBy(list(c("Estimate"),c("Survey","CoefName","Tech","sample","restrict")),
                    data=sf_estm[!sf_estm$draw %in% 0,],FUN=c(mean,sd,length)),
    by=c("Survey","CoefName","Tech","sample","restrict"))
  sf_estm$jack_zv  <- sf_estm$Estimate/sf_estm$Estimate.sd
  sf_estm$jack_pv  <- round(2 * (1 - pt(abs(sf_estm$jack_zv), df=sf_estm$Estimate.length)),5)
  #---------------------------------------------------
  # Summary Scores                                 ####
  ef_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$ef_mean)}), fill = TRUE))
  ef_mean <- ef_mean[!ef_mean$Estimate %in% c(NA,Inf,-Inf,NaN),]

  ef_mean <- doBy::summaryBy(list(c("Estimate"),c("sample","Tech","type","estType","Survey","stat","CoefName","restrict","draw")),
                  data=ef_mean,FUN=mean,keep.names = T)
  
  ef_mean <- dplyr::inner_join(
    ef_mean[ef_mean$draw %in% 0,c("sample","Tech","type","estType","Survey","stat","CoefName","restrict","Estimate")],
    doBy::summaryBy(list(c("Estimate"),c("sample","Tech","type","estType","Survey","stat","CoefName","restrict")),
                    data=ef_mean[!ef_mean$draw %in% 0,],FUN=c(mean,sd,length)),
    by=c("sample","Tech","type","estType","Survey","stat","CoefName","restrict"))
  
  function(){
    
    check <- ef_mean[(ef_mean$draw %in% 0 & ef_mean$Survey %in% "GLSS0"),]
    check <- check[check$stat %in% "wmean",]
    check <- check[check$estType %in% c("teBC"),]
    check <- check[check$CoefName %in% "efficiencyGap_pct",]
    check <- check[check$restrict %in% "Restricted",]

  }
  
  ef_mean$jack_zv  <- ef_mean$Estimate/ef_mean$Estimate.sd
  ef_mean$jack_pv  <- round(2 * (1 - pt(abs(ef_mean$jack_zv), df=ef_mean$Estimate.length)),5)
  
  #---------------------------------------------------
  # Summary Elasticity                             ####
  el_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$el_mean)}), fill = TRUE))
  table(el_mean$sample,el_mean$input)
  el_mean <- el_mean[!el_mean$Estimate %in% c(NA,Inf,-Inf,NaN),]
  
  el_mean <- doBy::summaryBy(list(c("Estimate"),c("Survey","sample","Tech","input","Survey","stat","CoefName","restrict","draw")),
                             data=el_mean,FUN=mean,keep.names = T)
  
  function(){
    
    check <- el_mean[(el_mean$draw %in% 0 & el_mean$Survey %in% "GLSS0"),]
    check <- check[check$stat %in% "wmean",]
    check <- check[check$input %in% "el1",]
    check <- check[check$CoefName %in% "elasticityGap_pct",]
    check <- check[check$restrict %in% "Restricted",]
    check
  }
  
  el_mean <- dplyr::inner_join(
    el_mean[el_mean$draw %in% 0,c("sample","Tech","input","Survey","stat","CoefName","restrict","Estimate")],
    doBy::summaryBy(list(c("Estimate"),c("sample","Tech","input","Survey","stat","CoefName","restrict")),
                    data=el_mean[!el_mean$draw %in% 0,],FUN=c(mean,sd,length)),
    by=c("sample","Tech","input","Survey","stat","CoefName","restrict"))
  
  el_mean$jack_zv  <- el_mean$Estimate/el_mean$Estimate.sd
  el_mean$jack_pv  <- round(2 * (1 - pt(abs(el_mean$jack_zv), df=el_mean$Estimate.length)),5)
  
  #---------------------------------------------------
  # Distribution bars- Scores                      ####
  ef_dist <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$ef_dist)}), fill = TRUE)) %>% 
    tidyr::gather(stat, Estimate, c("est_count","est_weight")) 
  
  ef_dist <- ef_dist[!ef_dist$Estimate %in% c(NA,Inf,-Inf,NaN),]

  ef_dist <- doBy::summaryBy(list(c("Estimate"),c("Survey","sample","Tech","type","estType","range","Frqlevel","stat","restrict","draw")),
                             data=ef_dist,FUN=mean,keep.names = T)
  
  ef_dist <- dplyr::inner_join(
    ef_dist[ef_dist$draw %in% 0,c("Survey","sample","Tech","type","estType","range","Frqlevel","stat","restrict","Estimate")],
    doBy::summaryBy(list(c("Estimate"),c("Surveyy","Survey","sample","Tech","type","estType","range","Frqlevel","stat","restrict")),
                    data=ef_dist[!ef_dist$draw %in% 0,],FUN=c(mean,sd,length)),
    by=c("Survey","sample","Tech","type","estType","range","Frqlevel","stat","restrict"))
  
  ef_dist$jack_zv  <- ef_dist$Estimate/ef_dist$Estimate.sd
  ef_dist$jack_pv  <- round(2 * (1 - pt(abs(ef_dist$jack_zv), df=ef_dist$Estimate.length)),5)
  ef_dist$stat <- gsub("est_","",ef_dist$stat)
  
  #---------------------------------------------------
  # Disaggregated score summary                    ####
  disagscors <- NULL
  if(!is.null(res[[1]]$disagscors)){
  disagscors <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$disagscors)}), fill = TRUE)) %>% 
    tidyr::gather(stat, Estimate, c("wmean","mean","median","mode")) 
  disagscors <- disagscors[!disagscors$Estimate %in% c(NA,Inf,-Inf,NaN),]

  disagscors <- doBy::summaryBy(list(c("Estimate"),c("sample","estType","Tech","input","Survey","disagscors_level","disagscors_var","CoefName","stat","restrict","draw")),
                             data=disagscors,FUN=mean,keep.names = T)
  
  disagscorsGAP <- disagscors[disagscors$Tech %in% TechKey$Tech,]
  disagscorsGAP <- dplyr::inner_join(
    disagscorsGAP[!disagscorsGAP$Tech %in% min(TechKey$Tech),c("sample","estType","Survey","CoefName","input","stat","restrict","disagscors_level","disagscors_var","Tech","draw","Estimate")],
    doBy::summaryBy(list("Estimate",c("sample","estType","Survey","CoefName","input","stat","restrict","disagscors_level","disagscors_var","draw")),
                    data=disagscorsGAP[disagscorsGAP$Tech %in% min(TechKey$Tech),],FUN=c(mean),na.rm=T),
    by=c("sample","estType","Survey","CoefName","input","stat","restrict","disagscors_level","disagscors_var","draw"))
  
  disagscorsGAP$disag_efficiencyGap_lvl <- disagscorsGAP$Estimate - disagscorsGAP$Estimate.mean
  disagscorsGAP$disag_efficiencyGap_pct <- ((disagscorsGAP$disag_efficiencyGap_lvl/abs(disagscorsGAP$Estimate.mean)))*100
  disagscorsGAP <- disagscorsGAP[c("sample","estType","Survey","CoefName","input","stat","restrict","disagscors_level","disagscors_var","Tech","draw",
                                   "disag_efficiencyGap_lvl","disag_efficiencyGap_pct")]
  disagscorsGAP <- disagscorsGAP %>%  tidyr::gather(CoefName, Estimate, c("disag_efficiencyGap_lvl","disag_efficiencyGap_pct"))
  disagscors <- as.data.frame(data.table::rbindlist(list(disagscors,disagscorsGAP), fill = TRUE))
  
  disagscors <- dplyr::inner_join(
    disagscors[disagscors$draw %in% 0,c("sample","estType","Tech","input","Survey","disagscors_level","disagscors_var","CoefName","stat","restrict","Estimate")],
    doBy::summaryBy(list(c("Estimate"),c("sample","estType","Tech","input","Survey","disagscors_level","disagscors_var","CoefName","stat","restrict")),
                    data=disagscors[!disagscors$draw %in% 0,],FUN=c(mean,sd,length)),
    by=c("sample","estType","Tech","input","Survey","disagscors_level","disagscors_var","CoefName","stat","restrict"))
  
  disagscors$jack_zv  <- disagscors$Estimate/disagscors$Estimate.sd
  disagscors$jack_pv  <- round(2 * (1 - pt(abs(disagscors$jack_zv), df=disagscors$Estimate.length)),5)
  }
  #---------------------------------------------------
  # Summary- Risk                                  ####
  rk_mean <- NULL
  if(!is.null(res[[1]]$rk_mean) & ! nrow(res[[1]]$rk_mean) %in% 0){
    rk_mean <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$rk_mean)}), fill = TRUE)) 
    rk_mean <- rk_mean[!rk_mean$Estimate %in% c(NA,Inf,-Inf,NaN),]

    rk_mean <- doBy::summaryBy(list(c("Estimate"),c("sample","Tech","Survey","stat","CoefName","restrict","draw")),
                                  data=rk_mean,FUN=mean,keep.names = T)
    
    rk_mean <- dplyr::inner_join(
      rk_mean[rk_mean$draw %in% 0,c("sample","Tech","Survey","stat","CoefName","restrict","Estimate")],
      doBy::summaryBy(list(c("Estimate"),c("sample","Tech","Survey","stat","CoefName","restrict")),
                      data=rk_mean[!rk_mean$draw %in% 0,],FUN=c(mean,sd,length)),
      by=c("sample","Tech","Survey","stat","CoefName","restrict"))
    rk_mean$jack_zv  <- rk_mean$Estimate/rk_mean$Estimate.sd
    rk_mean$jack_pv  <- round(2 * (1 - pt(abs(rk_mean$jack_zv), df=rk_mean$Estimate.length)),5)
    rk_mean$type <- "risk"
  }
  #---------------------------------------------------
  # Distribution bars- Risk                        ####
  rk_dist <- NULL
  if(!is.null(res[[1]]$rk_dist) & ! nrow(res[[1]]$rk_dist) %in% 0){
    rk_dist <- as.data.frame(data.table::rbindlist(lapply(1:length(res),function(draw){return(res[[draw]]$rk_dist)}), fill = TRUE)) %>% 
      tidyr::gather(stat, Estimate, c("est_count","est_weight")) 
    rk_dist <- rk_dist[!rk_dist$Estimate %in% c(NA,Inf,-Inf,NaN),]

    rk_dist <- doBy::summaryBy(list(c("Estimate"),c("Survey","sample","Tech","range","Frqlevel","stat","restrict","draw")),
                               data=rk_dist,FUN=mean,keep.names = T)
    
    rk_dist <- dplyr::inner_join(
      rk_dist[rk_dist$draw %in% 0,c("Survey","sample","Tech","range","Frqlevel","stat","restrict","Estimate")],
      doBy::summaryBy(list(c("Estimate"),c("Survey","sample","Tech","range","Frqlevel","stat","restrict")),
                      data=rk_dist[!rk_dist$draw %in% 0,],FUN=c(mean,sd,length)),
      by=c("Survey","sample","Tech","range","Frqlevel","stat","restrict"))
    
    rk_dist$jack_zv  <- rk_dist$Estimate/rk_dist$Estimate.sd
    rk_dist$jack_pv  <- round(2 * (1 - pt(abs(rk_dist$jack_zv), df=rk_dist$Estimate.length)),5)
    rk_dist$stat <- gsub("est_","",rk_dist$stat)
    rk_dist$type <- "risk"
  }
  #---------------------------------------------------
  # Summary and export                             ####
  
  res <- list(sf_estm = sf_estm,el_mean = el_mean,ef_mean = ef_mean,
              rk_mean = rk_mean,ef_dist = ef_dist,rk_dist = rk_dist, disagscors=disagscors,
              el_samp=res[[1]]$el_samp,ef_samp=res[[1]]$ef_samp,rk_samp=res[[1]]$rk_samp)

  return(res)
  #---------------------------------------------------
}
#---------------------------------------------
# TE estimations                           ####

Fxn_te_cals <- function(i){
  tryCatch({
    md <- readRDS(paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))$md[c("UID","weights")]
    md <- dplyr::inner_join(DATA,md,by="UID")
    
    md$HrvstKg <- md$HrvstKg/md$Area
    md$SeedKg <- md$SeedKg/md$Area
    md$HHLaborAE <- md$HHLaborAE/md$Area
    md$HirdHr <- md$HirdHr/md$Area
    md$FertKg <- md$FertKg/md$Area
    md$PestLt <- md$PestLt/md$Area
    
    treatment <- "Treat"
    
    atet_scalar <- as.data.frame(
      data.table::rbindlist(
        lapply(
          c("HrvstKg","Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
          function(outcome){
            tryCatch({
              # outcome <- "SeedKg"
              Emch.formula  <- paste0(paste0("factor(",Emch,")"),collapse = "+")
              Match.formula <- paste0("Treat~",paste0(c(Scle),collapse = "+"))
              for(var in c(Fixd)){ 
                Match.formula<-paste0(Match.formula,"+factor(",var,")")
              }
              
              data <- md[!(md[,treatment] %in% NA | md[,outcome] %in% NA | md[,"weights"] %in% NA),]
              
              data <- data[complete.cases(data[c(Emch,Scle,Fixd)]),]
              
              Fit.formula <- as.formula(paste0("log(",outcome,"+",min(data[,outcome][data[,outcome] > 0],na.rm=T)*(1/100),
                                               ")~",treatment,"*(",gsub("Treat~","",Match.formula),"+",Emch.formula,")"))
              
              fit_lm <- lm(as.formula(Fit.formula),weights = weights,data=data) # summary(fit_lm)
              TREATED <- data[names(data)[!names(data) %in% treatment]];TREATED[,treatment] <- 1
              TREATED <- predict(fit_lm,TREATED)
              
              UNTREATED <- data[names(data)[!names(data) %in% treatment]];UNTREATED[,treatment] <- 0
              UNTREATED <- predict(fit_lm,UNTREATED)
              
              data$TE_OLS <- (exp(TREATED - UNTREATED)-1)*100
              data$outcome <- outcome
              
              TE <- data[!(data$TE_OLS <= -100 | data$TE_OLS >= 100),]
               
              setDT(TE) 
              ATE  <- data.frame(TE[, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
              ATET <- data.frame(TE[Treat %in% 1][, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
              ATEU <- data.frame(TE[Treat %in% 0][, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
              
              aic <- as.numeric(AIC(fit_lm))
              ll  <- as.numeric(logLik(fit_lm))
              R2  <- summary(fit_lm)$r.squared
              R2a <- summary(fit_lm)$adj.r.squared
              Ft  <- summary(fit_lm)$fstatistic[1]
              N   <- nrow(fit_lm$model)
              
              fit_lm_res <- data.frame(outcome = outcome,treatment=treatment,
                                       level = c("ATE","ATET","ATEU","aic","ll","R2","N","Ft","R2a"),est=c(ATE,ATET,ATEU,aic,ll,R2,N,Ft,R2a))
              
              return(fit_lm_res)
            }, error = function(e){return(NULL)})
          }), fill = TRUE)) 
    
    atet_scalar <- data.frame(m.specs[i,],atet_scalar)
    
    saveRDS(atet_scalar,file=paste0("results/te/te",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))
    return(i)
  }, error = function(e){return(NULL)})
}


Fxn_te_summary <- function(){
estim <- as.data.frame(
  data.table::rbindlist(
    lapply(
      list.files(paste0("results/te/"),full.names = T),
      function(file){
        # i <- 1
        DONE <- NULL
        tryCatch({
          DONE <- readRDS(file)
        }, error=function(e){})
        return(DONE)
      }), fill = TRUE))

estim <- estim[!estim$est %in% c(NA,NaN,Inf,-Inf),]

estim <- dplyr::full_join(estim[estim$boot %in% 0, c("method","distance","link","outcome","level","est")],
                          doBy::summaryBy(list("est",c("method","distance","link","outcome","level")),
                                          data=estim,FUN=c(length,mean,sd)),
                          by=c("method","distance","link","outcome","level"))

names(estim)[names(estim) %in% "est.mean"]   <- "jack_mean"
names(estim)[names(estim) %in% "est.sd"]     <- "jack_se"
names(estim)[names(estim) %in% "est.length"] <- "jack_n"
estim$jack_tvl <- estim$jack_mean/estim$jack_se
estim$jack_pvl <- 2 * (1 - pt(abs(estim$jack_tvl), estim$jack_n-2))

saveRDS(estim,file=paste0("results/te_summary.rds"))
return("")
}

#---------------------------------------------