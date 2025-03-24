rm(list=ls(all=TRUE));gc();library('magrittr');library(future.apply);library(dplyr);library(purrr)
library(MatchIt);library(randomForest);library(CBPS);library(dbarts);library(optmatch);library(Matching);library(rgenoud)

setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))

setwd(paste0(getwd(),"/replications/tech_inefficiency_education"))

dir.create("results")
dir.create("results/matching")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_education_data.dta")))

DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- DATA$educated

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female")
Scle <- c("AgeYr","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

Emch.formula  <- paste0(paste0("factor(",Emch,")"),collapse = "+")
Match.formula <- paste0("Treat~",paste0(c(Scle),collapse = "+"))
for(var in c(Fixd)){ Match.formula<-paste0(Match.formula,"+factor(",var,")")}

DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
summary(DATA[c(Emch,Scle,Fixd)])

function(){
  m.specs <- Fxn_draw_spec(drawN=100,DATA=DATA,myseed=03242025)
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
      # i <- 10;m.data <- DATA
      Sampels <- Fxn_Sampels(DATA=DATA,Emch=Emch,Scle=Scle,Fixd=Fixd,m.specs=m.specs,i=i)
      if(! m.specs$boot[i] %in% 0){Sampels[["m.out"]] <- NULL}
      saveRDS(Sampels,file=paste0("results/matching/Match",stringr::str_pad(m.specs$ARRAY[i],4,pad="0"),".rds"))
    }, error=function(e){})
    return(i)
  },DATA=DATA)

# 

function(){
  Fxn_Covariate_balance()
}

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))
