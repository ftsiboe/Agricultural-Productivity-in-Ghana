rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_education"))
dir.create("results")
dir.create("results/estimations")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_education_data.dta")))

DATA$YerEduCat <- as.integer(cut(DATA$YerEdu,c(-1,0,3,6,9,12,100),right = T))
DATA$YerEduCat <- ifelse(DATA$educated %in% 1 & DATA$YerEduCat %in% 1,NA,DATA$YerEduCat)
DATA$EduLevel   <- as.integer(DATA$EduLevel)

for( vv in c("EduWhyNo", "Survey", "Region", "Ecozon", "Locality", "Ethnic", "Season", "Head", "Religion", "Marital", "CropID")){
  DATA[,vv] <- haven::as_factor(DATA[,vv])
}
FXNFORMS  <- Fxn_SF_forms()$FXNFORMS
DISTFORMS <- Fxn_SF_forms()$DISTFORMS

function(){

  mainD <- 1
  mainF <- 2
  
  SPECS <- Fxn_SPECS(TechVarlist=c("educated","EduLevel","numeracy","any_formal","any_read","any_write","any_literacy",
                                   "local_literacy","fregn_literacy","any_train","apprentice","student","YerEduCat"),  
                     mainD = mainD, mainF=mainF)
  SPECS <- SPECS[!SPECS$disasg %in% c("EduLevel","EduCat","Region","Ecozon"),]
  SPECS <- SPECS[!(SPECS$disasg %in% c("CropID") & ! SPECS$level %in% "Pooled"),]
  
  SPECS <- rbind(
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "educated" & SPECS$level %in% "Pooled"),], nnm="fullset"),
    data.frame(SPECS[ (SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "educated" & SPECS$level %in% "Pooled"),], nnm="optimal"),
    data.frame(SPECS[!(SPECS$f %in% mainF & SPECS$d %in% mainD & SPECS$TechVar %in% "educated" & SPECS$level %in% "Pooled"),], nnm="optimal"))
  
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
    # fit <- 2
    f <- SPECS$f[fit]
    d <- SPECS$d[fit]
    disasg <- SPECS$disasg[fit]
    level <- SPECS$level[fit]
    TechVar <- SPECS$TechVar[fit]
    nnm <- SPECS$nnm[fit]
    # nnm <- "optimal"
    if(!paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds") %in% list.files("Results/Estimations/")){
      #tryCatch({ 
      
      # Data Preparation
      data <- DATA[DATA[,SPECS$disasg[fit]] %in% SPECS$level[fit],]
      data <- data[!data[,SPECS$TechVar[fit]] %in% NA,]
      data$Tech <- as.numeric(as.integer(as.factor(as.character(data[,SPECS$TechVar[fit]]))))
      if(!SPECS$disasg[fit] %in% "CropID") data <- data[data[,"CropID"] %in% "Pooled",]
      TechKey <- unique(data[c("Tech",SPECS$TechVar[fit])])
      TechKey <- TechKey[order(TechKey$Tech),]
      
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
      
      if(nnm %in% "fullset") drawlist <- drawlist[drawlist$ID<=50,]
      
      disagscors_list <- NULL
      if(paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds") %in% 
         "CropID_Pooled_educated_TL_hnormal_optimal.rds"){
        disagscors_list <- c("Ecozon","Region","AgeCat","Female",names(data)[grepl("CROP_",names(data))])
      }
      
      res <- lapply(
        unique(drawlist$ID),Fxn_draw_estimations,
        data = data,
        surveyy  = F,
        intercept_shifters  = list(Svarlist=ArealistX,Fvarlist=c("Survey","Ecozon")),
        intercept_shiftersM = list(Svarlist=NULL,Fvarlist=c("Survey","Ecozon")),
        drawlist = drawlist,
        wvar = "Weight",
        yvar = "HrvstKg",
        xlist = c("Area", "SeedKg", "HHLaborAE","HirdHr","FertKg","PestLt"),
        ulist = list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
        ulistM= list(Svarlist=c("lnAgeYr","lnYerEdu","CrpMix"),Fvarlist=c("Female","Survey","Ecozon","Extension","Credit","EqipMech","OwnLnd")),
        UID   = c("UID", "Survey", "CropID", "HhId", "EaId", "Mid"),
        disagscors_list = disagscors_list,
        f     = f,
        d     = d,
        tvar  = TechVar,
        nnm   = nnm) 
      
      # resX <- res
      # resX[["names"]] <- paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm)
      # saveRDS(resX,file=paste0("Results/boots/",disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds"))
      
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
        Main <- Main[!Main$sample %in% "unmatched",]
        Main <- Main[Main$stat %in% "wmean",]
        Main <- Main[Main$CoefName %in% "efficiencyGap_pct",]
        Main <- Main[Main$restrict %in% "Restricted",]
        Main <- Main[Main$estType %in% "teBC",]
        Main[Main$type %in% "TGR",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "TE",c("sample","type","Tech","Estimate")]
        Main[Main$type %in% "MTE",c("sample","type","Tech","Estimate")]
      }
      
      res[["names"]] <- paste0(disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm)
      
      if(nnm %in% "fullset"){
        res$rk_dist <- NULL
        res$rk_mean <- NULL
        res$rk_samp <- NULL
        res$el_samp <- NULL
        res$ef_samp <- NULL 
      }
      
      saveRDS(res,file=paste0("results/estimations/",disasg,"_",level,"_",TechVar,"_",names(FXNFORMS)[f],"_",names(DISTFORMS)[d],"_",nnm,".rds"))
      
      #}, error=function(e){})
    }
    return(fit)
  })

# unlink(list.files(getwd(),pattern =paste0(".out"),full.names = T))

