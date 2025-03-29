rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_conflict"))
dir.create("results")
dir.create("results/estimations")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_conflict_data.dta")))

function(){
  m.specs <- Fxn_draw_spec(drawN=100,DATA=DATA,myseed=myseed)
  saveRDS(m.specs$m.specs,file="results/mspecs.rds")
  saveRDS(m.specs$drawlist,file="results/drawlist.rds")
}

m.specs <- readRDS("results/mspecs.rds")

FXNFORMS  <- Fxn_SF_forms()$FXNFORMS
DISTFORMS <- Fxn_SF_forms()$DISTFORMS

function(){

  mainD <- 1
  mainF <- 2
  
  SPECS <- Fxn_SPECS(c("index0CAT","index1CAT", "index2CAT", "index3CAT", "index4CAT", "index5CAT", "index6CAT"),  
                     mainD = mainD, mainF=mainF)
  SPECS <- SPECS[!SPECS$disasg %in% c( "EduCat"),]

  SPECS <- SPECS[!(paste0(SPECS$disasg,"_",SPECS$level,"_",SPECS$TechVar,"_",names(FXNFORMS)[SPECS$f],"_",
                          names(DISTFORMS)[SPECS$d],"_",SPECS$nnm,".rds") %in% list.files("results/estimations/")),]
  
  row.names(SPECS) <- 1:nrow(SPECS)
  
  saveRDS(SPECS,file="results/SPECS.rds")
}

SPECS <- readRDS("results/SPECS.rds")

if(!is.na(as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")))){
  SPECS <- SPECS[as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),]
}

lapply(
  c(1:nrow(SPECS)),
  function(fit){
    # fit <- 1
    f <- SPECS$f[fit]
    d <- SPECS$d[fit]
    disasg <- SPECS$disasg[fit]
    level <- SPECS$level[fit]
    TechVar <- SPECS$TechVar[fit]
    
    if(!paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],".rds") %in% list.files("results/estimations/")){
      #tryCatch({ 
      
      # Data Preparation
      data <- DATA[DATA[,SPECS$disasg[fit]] %in% SPECS$level[fit],]
      data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,SPECS$TechVar[fit]]))))
      if(!SPECS$disasg[fit] %in% "CropID") data <- data[data[,"CropID"] %in% "Pooled",]
      TechKey   <- unique(data[c("Tech",SPECS$TechVar[fit])])
      TechKey   <- TechKey[order(TechKey$Tech),]
      
      for(crop in c(c("Beans","Cassava","Cocoa","Cocoyam","Other","Millet","Okra","Palm","Peanut",
                      "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam","Maize"))){
        data[,paste0("CROP_",crop)] <- ifelse(data[,paste0("Area_",crop)] > 0, crop,NA)
      }
      
      ArealistX <- names(data)[grepl("Area_",names(data))]
      ArealistX <- ArealistX[ArealistX %in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Other","Millet","Okra","Palm","Peanut",
                                                             "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam","Other"))]
      
      ArealistX <- apply(data[names(data)[names(data) %in% ArealistX]],2,mean) > 0.03
      ArealistX <- names(ArealistX)[ArealistX %in% TRUE]
      if(length(ArealistX)>0){ 
        data$Area_Other <- 1 - rowSums(data[c(ArealistX[!ArealistX %in% "Area_Other"],"Area_Maize")],na.rm=T)
        ArealistX <- unique(c(ArealistX,"Area_Other"))
      }
      
      
      # draw estimations
      drawlist = readRDS("results/drawlist.rds")
      
      res <- lapply(
        unique(drawlist$ID),Fxn_draw_estimations,
        data = data,
        surveyy  = TRUE,
        intercept_shifters  = list(Svarlist=ArealistX,Fvarlist=c("Ecozon")),
        intercept_shiftersM = list(Svarlist=ArealistX,Fvarlist=c("Ecozon")),
        drawlist = drawlist,
        wvar = "Weight",
        yvar = "HrvstKg",
        xlist = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
        ulist = list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
        UID   = c("UID", "Survey", "CropID", "HhId", "EaId", "Mid"),
        disagscors_list = c("Ecozon","Region","AgeCat","EduLevel","Female",names(data)[grepl("CROP_",names(data))]),
        f     = f,
        d     = d,
        tvar  = TechVar) 
      
      # draw summary [START FROM HERE]
      res <- Fxn.draw_summary(res=res,TechKey=TechKey)
      
      for(xx in 1:length(res)){
        tryCatch({
          res[[xx]][,"FXN"]     <- names(FXNFORMS)[f]
          res[[xx]][,"DIS"]     <- names(DISTFORMS)[d]
          res[[xx]][,"disasg"]  <- disasg
          res[[xx]][,"level"]   <- level
          res[[xx]][,"TCH"]     <- TechVar
          res[[xx]][,"TCHLvel"] <- factor(res[[xx]][,"Tech"],levels = c(-999,TechKey$Tech,999),labels = c("National",TechKey[,2],"Meta"))
        }, error=function(e){})
      }
      
      function(){
        Main <- res$ef_mean
        Main <- Main[Main$Survey %in% "GLSS0",]
        #Main <- Main[!Main$sample %in% "unmatched",]
        Main <- Main[Main$stat %in% "wmean",]
        Main <- Main[Main$CoefName %in% "efficiencyGap_pct",]
        Main <- Main[Main$restrict %in% "Restricted",]
        Main <- Main[Main$estType %in% "teBC",]
        Main[Main$type %in% "TGR",c("sample","type","TCHLvel","Estimate")]
        Main[Main$type %in% "TE",c("sample","type","TCHLvel","Estimate")]
        Main[Main$type %in% "MTE",c("sample","type","TCHLvel","Estimate")]
      }
      
      res[["names"]] <- paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d])
      
      saveRDS(res,file=paste0("results/estimations/",disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],".rds"))
      
      #}, error=function(e){})
    }
    return(fit)
  })

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))

