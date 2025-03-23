rm(list=ls(all=TRUE));gc();library('magrittr');library(future.apply);library(dplyr);library(purrr)
library(MatchIt);library(randomForest);library(CBPS);library(dbarts);library(optmatch);library(Matching);library(rgenoud)

setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))

setwd(paste0(getwd(),"/replications/tech_inefficiency_resource_extract"))

dir.create("results")
dir.create("results/matching")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/Harmonized_Farm_resources_extraction_Data.dta")))

DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- DATA$extraction_any #%in% 1

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

Emch.formula  <- paste0(paste0("factor(",Emch,")"),collapse = "+")
Match.formula <- paste0("Treat~",paste0(c(Scle),collapse = "+"))
for(var in c(Fixd)){ Match.formula<-paste0(Match.formula,"+factor(",var,")")}

DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
summary(DATA[c(Emch,Scle,Fixd)])

function(){
  m.specs <- Fxn_draw_spec(drawN=100,DATA=DATA,myseed=03182025)
  saveRDS(m.specs$m.specs,file="results/mspecs.rds")
  saveRDS(m.specs$drawlist,file="results/drawlist.rds")
}

m.specs <- readRDS("results/mspecs.rds")

# m.specs <- m.specs[! paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY,4,pad="0"),".rds") %in%
#                      list.files(paste0("results/matching"),full.names = T),]

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  m.specs <- m.specs[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  1:nrow(m.specs), #
  function(i,DATA){
    tryCatch({
      # i <- 1;m.data <- DATA
      Sampels <- Fxn_Sampels(DATA=DATA,Emch=Emch,Scle=Scle,Fixd=Fixd,m.specs=m.specs,i=i, drawlist = readRDS("results/drawlist.rds"))
      if(! m.specs$boot[i] %in% 0){Sampels[["m.out"]] <- NULL}
      saveRDS(Sampels,file=paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))
    }, error=function(e){})
    return(i)
  },DATA=DATA)

function(){
  Fxn_Covariate_balance()
  
  mspecs_optimal <- readRDS(paste0("results/mspecs_optimal.rds"))[c("ARRAY","method","distance","link")]
  mspecs_fullset <- readRDS(paste0("results/mspecs.rds"))
  mspecs_fullset <- mspecs_fullset[!grepl("linear",mspecs_fullset$link),]
  mspecs_fullset <- mspecs_fullset[mspecs_fullset$boot %in% 0,c("ARRAY","method","distance","link")]
  
  m.specs <- readRDS(paste0("results/mspecs.rds"))
  m.specs <- m.specs[m.specs$boot %in% 0,]
  m.specs <- m.specs[m.specs$method %in% mspecs_fullset$method,]
  m.specs <- m.specs[m.specs$distance %in% mspecs_fullset$distance,]
  m.specs <- m.specs[m.specs$link %in% mspecs_fullset$link,]
  m.specs$name <- ifelse(m.specs$link %in% NA,m.specs$distance,m.specs$link)
  
  atet <- as.data.frame(
    data.table::rbindlist(
      lapply(
        1:nrow(m.specs),
        function(mm){
          # mm <- 1
          DONE <- NULL
          tryCatch({
            md <- dplyr::inner_join(unique(readRDS(paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[mm],4,pad="0"),".rds"))$md),
                                    DATA,by=c("Surveyx","EaId","HhId","Mid","UID"))
            
            md$HrvstKg <- md$HrvstKg/md$Area
            md$SeedKg <- md$SeedKg/md$Area
            md$HHLaborAE <- md$HHLaborAE/md$Area
            md$HirdHr <- md$HirdHr/md$Area
            md$FertKg <- md$FertKg/md$Area
            md$PestLt <- md$PestLt/md$Area
            
            atet_scalar <- as.data.frame(
              data.table::rbindlist(
                future_lapply(
                  c("HrvstKg","Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
                  function(outcome){
                    # outcome <- "Area"
                    DONE <- NULL
                    tryCatch({
                      atet_scalar <- marginaleffects::avg_comparisons(
                        lm(as.formula(paste0("log(",outcome,"+0.00001)","~Treat*(",paste0(c(paste0("factor(",c(Emch,Fixd),")"),Scle),collapse = "+"),")")),
                           data = md[!log(md[,outcome]+0.00001) %in% c(NA,NaN,Inf,-Inf),], weights = weights), variables = "Treat",
                        newdata = subset(md[!log(md[,outcome]+0.00001) %in% c(NA,NaN,Inf,-Inf),], Treat == 1),wts = "weights",vcov="HC0")
                      atet_scalar <- data.frame(outcome=outcome,as.data.frame(atet_scalar)[c("estimate","std.error","statistic","p.value","s.value","conf.low","conf.high")])
                      DONE <- atet_scalar
                    }, error=function(e){})
                    return(DONE)
                  }), fill = TRUE))
            
            atet_factor <- as.data.frame(
              data.table::rbindlist(
                future_lapply(
                  c("Credit","Eqip","Extension","OwnLnd"),
                  function(outcome){
                    # outcome <- "Credit"
                    DONE <- NULL
                    tryCatch({
                      atet_factor <- marginaleffects::avg_comparisons(
                        glm(as.formula(paste0(outcome,"~Treat*(",paste0(c(paste0("factor(",c(Emch,Fixd),")"),Scle),collapse = "+"),")")),
                            data = md, weights = weights,family = quasibinomial()), variables = "Treat",
                        newdata = subset(md, Treat == 1),wts = "weights",vcov="HC0",comparison = "lnratioavg",transform = "exp")
                      atet_factor <- data.frame(outcome=outcome,as.data.frame(atet_factor)[c("estimate","p.value","s.value","conf.low","conf.high")])
                      atet_factor$std.error <- atet_factor$statistic <- NA
                      DONE <- atet_factor
                    }, error=function(e){})
                    return(DONE)
                  }), fill = TRUE))
            
            DONE <- data.frame(m.specs[mm,c("method","distance","name")],rbind(atet_factor,atet_scalar))
          }, error=function(e){})
          return(DONE)
        }), fill = TRUE))
  
  saveRDS(atet,file=paste0("results/matching_treatment_effects.rds"))
}

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))
