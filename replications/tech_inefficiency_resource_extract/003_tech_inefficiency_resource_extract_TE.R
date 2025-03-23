
rm(list=ls(all=TRUE));gc();library('magrittr');library(future.apply);library(dplyr);library(quantreg);library(data.table);library(systemfit);library(tidyr);library(nnet);library(stats)
library(MatchIt);library(randomForest);library(CBPS);library(dbarts);library(optmatch);library(Matching);library(rgenoud);library(ggplot2);library(haven);library(marginaleffects)
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_resource_extract"))
dir.create("results")
dir.create("results/te")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_resource_extract_data.dta")))
DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- DATA$extraction_any %in% 1

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

m.specs <- readRDS("results/mspecs.rds")

plan(multisession)
future_lapply(
  1:nrow(m.specs), #
  function(i){
    tryCatch({
      # i <- 1
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
                # outcome <- "Area"
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
                DISAB <- data[names(data)[!names(data) %in% treatment]];DISAB[,treatment] <- TRUE
                DISAB <- predict(fit_lm,DISAB)
                
                NODISAB <- data[names(data)[!names(data) %in% treatment]];NODISAB[,treatment] <- FALSE
                NODISAB <- predict(fit_lm,NODISAB)
                
                data$TE_OLS <- (exp(DISAB - NODISAB)-1)*100
                data$outcome <- outcome
                
                TE <- data[!(data$TE_OLS <= -100 | data$TE_OLS >= 100),]
                
                setDT(TE) 
                ATE  <- data.frame(TE[, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
                ATET <- data.frame(TE[extraction_any %in% 1][, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
                ATEU <- data.frame(TE[extraction_any %in% 0][, .(est = weighted.mean(x=TE_OLS, w=weights,na.rm= TRUE)), by = .(outcome)])$est
                
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
      #-----------------------------------------------------------
      return(i)
    }, error = function(e){return(NULL)})
  })

function(){

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
}

