rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/helpers_tech_inefficiency.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_extension"))
dir.create("results")
dir.create("results/te")

DATA <- Fxn_DATA_Prep(as.data.frame(haven::read_dta("data/tech_inefficiency_extension_data.dta")))

DATA <- DATA[as.character(haven::as_factor(DATA$CropID)) %in% "Pooled",]
DATA$Treat <- as.numeric(DATA$ext_available %in% 1)

Arealist <- names(DATA)[grepl("Area_",names(DATA))]
Arealist <- Arealist[Arealist%in% paste0("Area_",c("Beans","Cassava","Cocoa","Cocoyam","Maize","Millet","Okra","Palm","Peanut",
                                                   "Pepper","Plantain","Rice","Sorghum","Tomatoe","Yam"))]

Emch <- c("Survey","Region","Ecozon","Locality","Female","AgeCat")
Scle <- c("AgeYr","YerEdu","HHSizeAE","FmleAERt","Depend","CrpMix",Arealist)
Fixd <- c("Credit","OwnLnd","Ethnic","Marital","Religion","Head")

Emch.formula  <- paste0(paste0("factor(",Emch,")"),collapse = "+")
Match.formula <- paste0("Treat~",paste0(c(Scle),collapse = "+"))
for(var in c(Fixd)){ Match.formula<-paste0(Match.formula,"+factor(",var,")")}

DATA <- DATA[complete.cases(DATA[c("Surveyx","EaId","HhId","Mid","UID","Weight","Treat",Emch,Scle,Fixd)]),]
summary(DATA[c(Emch,Scle,Fixd)])

m.specs <- readRDS("results/mspecs.rds")

plan(multisession)
future_lapply(1:nrow(m.specs),Fxn_te_cals)

function(){Fxn_te_summary()}

