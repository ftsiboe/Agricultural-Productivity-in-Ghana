#*******************************************************************************
# Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
# Citation requirement;
# 1. Asravor, J., Tsiboe, F., Asravor, R.K. et al. Technology and managerial performance of farm operators by age in 	Ghana. J Prod Anal (2023). https://doi.org/10.1007/s11123-023-00679-y
# 2. Tsiboe, F. (2020). Nationally Representative Farm/Household Level Dataset on Crop Production in Ghana from 1987-2017.
#*******************************************************************************
#------------------------------------
# Preliminaries                   ####
rm(list=ls(all=TRUE))
library('magrittr');library(ggplot2);library(gganimate);library(rasterVis);library(gridExtra)
library(magick);library(ggpubr);library(dplyr);library(gtable)
Keep.List<-c("Keep.List",ls())
#------------------------------------
# Hypothesis Tests                ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
List<-list.files("C:/Research/Articles/Ghana/Farmer Age Productivity/Results/Model Outputs",recursive = T,full.names = T)
List<-List[grepl(".dta",List)]
length(List[!grepl("Pooled",List)])
Fxn1 <- function(file){
  # file<-3
  SFAs <- haven::read_dta(List[file])
  SFAs$Schmidt2_p <- ifelse(SFAs$Schmidt2<0,SFAs$Schmidt2_p,1)
  SFAs$Coelli_p   <- ifelse(SFAs$Coelli<0,SFAs$Coelli_p,1)
  SFAs$DAgostino_p <- ifelse(SFAs$DAgostino<0,SFAs$DAgostino_p,1)
  SFAs<-doBy::summaryBy(list(c("Obs","LL","Par","CD_test_p","Schmidt2_p","Coelli_p","Gutierrez_p","DAgostino_p",
                               "sigma_u","sigma_v","sigma2","Gamma","lambda","U_test_p","Sig_p"),c("Zone","CropIDx","Surveyx")),
                        data=data.frame(SFAs),keep.names=T,FUN=min)
  LR0<-SFAs[SFAs$Zone %in% 990,]$LL
  LR1<-sum(SFAs[!SFAs$Zone %in% 990,]$LL)
  DF0<-SFAs[SFAs$Zone %in% 990,]$Par
  DF1<-sum(SFAs[!SFAs$Zone %in% 990,]$Par)
  LRT<- data.frame(value=pchisq(-2*(LR0-LR1), df=DF1-DF0, lower.tail=F),
                   Test="Tech",Zone=990,CropIDx=unique(SFAs$CropIDx),Surveyx=unique(SFAs$Surveyx))
  SFAs <- SFAs %>%  tidyr::gather(Test, value, 4:ncol(SFAs))
  SFAs <- rbind(LRT,SFAs)
  SFAs<-SFAs[!SFAs$Test %in% c("CropIDx","Surveyx","Obs","LL","Par"),]
  
  return(SFAs)
}
TestStat<- do.call(rbind.data.frame,lapply(1:length(List), Fxn1))
nrow(unique(TestStat[c("Zone","Surveyx","CropIDx")]))

Tech <- TestStat[TestStat$Test %in% "Tech",]
Tech <- Tech[!Tech$CropIDx %in% "Pooled",]
mean(as.numeric(Tech$value<=0.1),na.rm=T)

TestStat<-TestStat[complete.cases(TestStat),]
nrow(unique(TestStat[c("Zone","Surveyx","CropIDx")]))
TestStat$Zone<-factor(TestStat$Zone, levels = c(0,1,2,990,991),
                      labels = c("15-35 years","36-59 years","Over 59 years","National","Meta"))
		
TestStat <- data.frame(TestStat)
TestStat$value<-ifelse(grepl("_p",TestStat$Test),as.numeric(TestStat$value<=0.1), as.numeric(TestStat$value))

RatePool<-doBy::summaryBy(value~Test+Zone, FUN=c(length,mean,sd,min,max), data=TestStat[!TestStat$CropIDx %in% "Pooled",])
RateCrop<-doBy::summaryBy(value~Test+Zone+CropIDx, FUN=c(length,mean,sd,min,max), data=TestStat)

RatePool$CropIDx <- "Pooled_all"

Rate<-rbind(RateCrop,RatePool)
Rate$value.se<-Rate$value.sd/sqrt(Rate$value.length)
Rate$value.Up<-Rate$value.mean+Rate$value.se*1.96
Rate$value.Lw<-Rate$value.mean-Rate$value.se*1.96

wb <- openxlsx::loadWorkbook("Results/Farmer_Age_Productivity_Ghana_Results.xlsx")
openxlsx::writeData(wb, sheet = "TestRates",Rate , colNames = T, startCol = "B", startRow = 1)
openxlsx::saveWorkbook(wb,"Results/Farmer_Age_Productivity_Ghana_Results.xlsx",overwrite = T)
#------------------------------------
# Elasticities  by Age            ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
data <- foreign::read.dta("Results/Summaries_Age.dta", convert.f=TRUE)
data <- data[!data$Equ %in% c("TGR_Hete","TE_Hete","MTE_Hete"),]
data <- data[data$Matrix %in% "Zone",]
data <- data[data$CropIDx %in% "Pooled_all",]
data <- data[complete.cases(data$Stdr),]
data <- data[!data$AgeCat %in% NA,]

data$Type<-ifelse(data$Equ =="Els_1","(a) Land",NA)
data$Type<-ifelse(data$Equ =="Els_2","(b) Seed",data$Type)
data$Type<-ifelse(data$Equ =="Els_3","(c) Family labor",data$Type)
data$Type<-ifelse(data$Equ =="Els_4","(d) Hired labor",data$Type)
data$Type<-ifelse(data$Equ =="Els_5","(e) Fertilizer",data$Type)
data$Type<-ifelse(data$Equ =="Els_6","(f) Pesticide",data$Type)
data$Type<-ifelse(data$Equ =="RTS","(g) Returns to scale",data$Type)
data <- data[!data$Type %in% NA,]
data$Stdr <-ifelse(data$Pval>0.05,NA,data$Stdr)

lable <- unique(data[c("Age","AgeYr_min","AgeYr_max")])
lable <- lable[lable$AgeYr_min %in% seq(15,100,15),]

Fig <- ggplot(data=data,aes(x=Age, y=Beta,group=1,fill=AgeCat,color=AgeCat)) +
  geom_smooth(method = 'loess', formula= y ~ x,span = 0.3,se=T, size=1) +
  geom_point(colour="black",pch=21, size=1.5) + 
  #geom_errorbar(aes(ymax = Beta + Stdr*1.96,ymin = Beta - Stdr*1.96), width = 0.25,color="blue") + 
  facet_wrap(vars(Type),ncol=4,scales = "free_y") + 
  scale_x_continuous(breaks=lable$Age, labels=paste0(lable$AgeYr_min," - ",lable$AgeYr_max)) + 
  scale_fill_manual(values = c("thistle","violet","purple")) +
  scale_color_manual(values = c("thistle","violet","purple")) +
  labs(title= "", x = "\nAge range", y = "Elasticity (%)\n", caption = "") + 
  ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position=c(0.85,0.20),legend.key.size = unit(0.25,"cm")) +
  theme(legend.text=element_text(size=11),
        legend.title=element_blank(),
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        axis.text.x = element_text(size = 6,angle = 90, colour="black",hjust = 0 ,vjust = 0.5), #
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))


ggsave("Results/Figs/Elasticities_Age.png", Fig, dpi = 600,width = 11, height =7)
#------------------------------------
# Tech/Eff by Age                 ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
data<-foreign::read.dta("Results/Summaries_Age.dta", convert.f=TRUE)
data <- data[data$Equ %in% c("TGR_Hete","TE_Hete","MTE_Hete"),]
data <- data[data$Matrix %in% "Zone",]
data <- data[complete.cases(data$Stdr),]
data <- data[data$CropIDx %in% "Pooled_all",]
data <- data[complete.cases(data$Stdr),]
data <- data[!data$AgeCat %in% NA,]
data$Type<-ifelse(data$Equ =="TGR_Hete","(i) Technology gap ratio (TGR)",NA)
data$Type<-ifelse(data$Equ =="TE_Hete","(ii) Technical efficiency (TE)",data$Type)
data$Type<-ifelse(data$Equ =="MTE_Hete","(iii) Meta-technical-efficiency (MTE)",data$Type)
lable <- unique(data[c("Age","AgeYr_min","AgeYr_max")])
lable <- lable[lable$AgeYr_min %in% seq(15,100,15),]

FigA <- ggplot(data=data,aes(x=Age, y=Beta,group=1,fill=AgeCat,color=AgeCat)) +
  geom_smooth(method = 'loess', formula= y ~ x,span = 0.25,se=F) +
  geom_point(colour="black",pch=21, size=2) + 
  #geom_errorbar(aes(ymax = Beta + Stdr*1.96,ymin = Beta - Stdr*1.96), width = 0.25,color="blue") + 
  facet_wrap(vars(Type),ncol=3) + 
  scale_x_continuous(breaks=lable$Age, labels=paste0(lable$AgeYr_min," - ",lable$AgeYr_max)) + 
  scale_y_continuous(breaks=seq(0,1.5,0.1), labels=sprintf(seq(0,1.5,0.1),fmt="%#.1f")) + 
  scale_fill_manual(values = c("thistle","violet","purple")) +
  scale_color_manual(values = c("thistle","violet","purple")) +
  labs(title= "", x = "\nSeason", y = "Ratio\n", caption = "") + 
  ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=11),
        legend.title=element_blank(),
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        axis.text.x = element_text(size = 7,angle = 90, colour="black",hjust = 0 ,vjust = 0.5), #
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

ggsave("Results/Figs/Score_Age.png", FigA, dpi = 600,width = 11, height =6)
#------------------------------------
# Elasticities  by Season         ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
data <- foreign::read.dta("Results/Summaries_Season.dta", convert.f=TRUE)
data <- data[!data$Equ %in% c("TGR_Hete","TE_Hete","MTE_Hete"),]
data <- data[!data$Seas %in% NA,]
data <- data[data$Matrix %in% "Zone",]
data <- data[data$CropIDx %in% "Pooled_all",]
data <- data[complete.cases(data$Stdr),]
data <- data[!data$AgeCat %in% NA,]

data$Type<-ifelse(data$Equ =="Els_1","(a) Land",NA)
data$Type<-ifelse(data$Equ =="Els_2","(b) Seed",data$Type)
data$Type<-ifelse(data$Equ =="Els_3","(c) Family labor",data$Type)
data$Type<-ifelse(data$Equ =="Els_4","(d) Hired labor",data$Type)
data$Type<-ifelse(data$Equ =="Els_5","(e) Fertilizer",data$Type)
data$Type<-ifelse(data$Equ =="Els_6","(f) Pesticide",data$Type)
data$Type<-ifelse(data$Equ =="RTS","(g) Returns to scale",data$Type)
data <- data[!data$Type %in% NA,]
data$Stdr <-ifelse(data$Pval>0.05,NA,data$Stdr)

lable <- unique(data[c("Seas","Season")])

Fig <- ggplot(data=data,aes(x=Seas, y=Beta,group=AgeCat,fill=AgeCat,color=AgeCat)) +
  geom_smooth(method = 'lm', formula= y ~ x,span = 1,se=F, size=1) +
  geom_point(colour="black",pch=21, size=1.5) + 
  geom_errorbar(aes(ymax = Beta + Stdr*1.96,ymin = Beta - Stdr*1.96), width = 0.25,color="blue") + 
  facet_wrap(vars(Type),ncol=4,scales = "free_y") + 
  scale_x_continuous(breaks=lable$Seas, labels=lable$Season) + 
  scale_fill_manual(values = c("thistle","violet","purple")) +
  scale_color_manual(values = c("thistle","violet","purple")) +
  labs(title= "", x = "\nSeason", y = "Elasticity (%)\n", caption = "") + 
  ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position=c(0.85,0.20),legend.key.size = unit(0.4,"cm")) +
  theme(legend.text=element_text(size=11),
        legend.title=element_blank(),
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        axis.text.x = element_text(size = 6,angle = 90, colour="black",hjust = 0 ,vjust = 0.5), #
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))


ggsave("Results/Figs/Elasticities_Season.png", Fig, dpi = 600,width = 11, height =7)
#------------------------------------
# Tech/Eff by Season              ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
data<-foreign::read.dta("Results/Summaries_Season.dta", convert.f=TRUE)
data <- data[data$Equ %in% c("TGR_Hete","TE_Hete","MTE_Hete"),]
data <- data[!data$Seas %in% NA,]
data <- data[data$Matrix %in% "Zone",]
data <- data[complete.cases(data$Stdr),]
data <- data[data$CropIDx %in% "Pooled_all",]
data <- data[complete.cases(data$Stdr),]
data <- data[!data$AgeCat %in% NA,]
data$Type<-ifelse(data$Equ =="TGR_Hete","(i) Technology gap ratio (TGR)",NA)
data$Type<-ifelse(data$Equ =="TE_Hete","(ii) Technical efficiency (TE)",data$Type)
data$Type<-ifelse(data$Equ =="MTE_Hete","(iii) Meta-technical-efficiency (MTE)",data$Type)
lable <- unique(data[c("Seas","Season")])

FigA <- ggplot(data=data,aes(x=Seas, y=Beta,group=AgeCat,fill=AgeCat,color=AgeCat)) +
  geom_smooth(method = 'loess', formula= y ~ x,span = 1,se=F) +
  geom_point(colour="black",pch=21, size=2) + 
  geom_errorbar(aes(ymax = Beta + Stdr*1.96,ymin = Beta - Stdr*1.96), width = 0.25,color="blue") + 
  facet_wrap(vars(Type),ncol=3) + 
  scale_x_continuous(breaks=lable$Seas, labels=lable$Season) + 
  scale_y_continuous(breaks=seq(0,1.5,0.1), labels=sprintf(seq(0,1.5,0.1),fmt="%#.1f")) + 
  scale_fill_manual(values = c("thistle","violet","purple")) +
  scale_color_manual(values = c("thistle","violet","purple")) +
  labs(title= "", x = "\nSeason", y = "Ratio\n", caption = "") + 
  ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=11),
        legend.title=element_blank(),
        axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        axis.text.x = element_text(size = 7,angle = 90, colour="black",hjust = 0 ,vjust = 0.5), #
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

ggsave("Results/Figs/Score_Season.png", FigA, dpi = 600,width = 11, height =6)
#------------------------------------
# Score by Region                 ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
Shp.GH <- rgdal::readOGR(dsn = "Data", layer = "GHA_admbndp1_1m_GAUL")

Data.f <- fortify(Shp.GH, region = "ADM1_NAME")  # maptools package required for this functionality
Data.f <- Data.f[c("long","lat","order","hole","piece", "id","group")]
names(Data.f)<-c("long","lat","order","hole","piece", "Region","group")

data <- foreign::read.dta("Results/Summaries_Region.dta", convert.f=TRUE)
data <- data[data$Equ %in% c("TGR_Hete","TE_Hete","MTE_Hete"),]
data <- data[data$Matrix %in% "Zone",]
data <- data[data$CropIDx %in% c("Pooled_all"),]
data$Type<-ifelse(data$Equ =="TGR_Hete","(i) Technology gap ratio (TGR)",NA)
data$Type<-ifelse(data$Equ =="TE_Hete","(ii) Technical efficiency (TE)",data$Type)
data$Type<-ifelse(data$Equ =="MTE_Hete","(iii) Meta-technical-efficiency (MTE)",data$Type)

data$Beta<-as.numeric(as.character(data$Beta))

Shp.names <- as.data.frame(sp::coordinates(Shp.GH))
names(Shp.names) <- c("longc", "latc")  #more sensible column names
Shp.names <- cbind(Shp.names,Shp.GH@data)
Shp.names$Region<-Shp.names$ADM1_NAME
Shp.names <- dplyr::inner_join(Shp.names, data, by = c("Region"))
Shp.names<-doBy::summaryBy(longc+latc+Beta~Region+Type+AgeCat,data=Shp.names,keep.names = T,FUN=mean)
Shp.names<-Shp.names[! Shp.names$Type %in% NA,]
Shp.names$labl <- paste0(Shp.names$Region," [",as.character(round(Shp.names$Beta,3)),"]")

data<-dplyr::full_join(data,Data.f,by="Region")
data<-data[! data$Beta %in% NA,]
data<-data[! data$Type %in% NA,]

Fig<-ggplot() + 
  geom_polygon(data = data,aes(long, lat, group = group, fill = Beta)) +
  geom_polygon(data = Shp.GH, aes(x = long, y = lat, group = group), colour = "black", fill = NA,size = 0.2) + 
  geom_text(data=Shp.names,aes(label = stringr::str_wrap(labl, 6), x = longc, y = latc), size=2) + #add labels at centroids +
  labs(title= "", x = "", y = "",fill ="", caption = "") + 
  guides(color = guide_legend(override.aes = list(size = 0.5))) +  
  scale_fill_gradient(low="thistle", high="purple",na.value = NA) +
  facet_grid(Type~AgeCat) + 
  labs(title= "", x = "", y = "",fill ="Score [ratio]", caption = "") + 
  ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), #
        axis.text.y = element_blank(),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))+coord_fixed()
ggsave("Results/Figs/Gap_Region.png", Fig, dpi = 600,width = 8, height =11)
#------------------------------------
# Hypothesis Tests                ####
rm(list= ls()[!(ls() %in% c(Keep.List))])
List<-list.files("C:/Research/Articles/Ghana/Farmer Age Productivity/Results/Model Outputs",recursive = T,full.names = T)
List<-List[grepl(".dta",List)]
Fxn1 <- function(file){
  # file<-3
  SFAs <- data.frame(haven::read_dta(List[file]))
  SFAs$Coef_x <- as.character(haven::as_factor(SFAs$Coef_x, levels="label"))
  SFAs$Equ_x <- as.character(haven::as_factor(SFAs$Equ_x, levels="label"))
  SFAs <- SFAs[c("CropIDx","Fxn","Zone","Equ_x","Coef_x","Beta","Stdr","Tsts","Pval")]
  SFAs <- SFAs[complete.cases(SFAs),]
  SFAs <- SFAs[SFAs$Equ_x %in% "lnsig2u",]
  SFAs <- SFAs[!SFAs$Beta %in% 0,]
  return(SFAs)
}
TechFxn<- do.call(rbind.data.frame,lapply(1:length(List), Fxn1))
Pooled <- TechFxn[!TechFxn$CropIDx %in% "Pooled",]
Pooled$CropIDx <- "Pooled_all"
TechFxn <- rbind(TechFxn,Pooled)
TechFxn<-doBy::summaryBy(Beta~CropIDx+Fxn+Zone+Equ_x+Coef_x, FUN=c(mean,sd), data=TechFxn)
wb <- openxlsx::loadWorkbook("Results/Farmer_Age_Productivity_Ghana_Results.xlsx")
openxlsx::writeData(wb, sheet = "TechFxn",TechFxn , colNames = T, startCol = "B", startRow = 1)
openxlsx::saveWorkbook(wb,"Results/Farmer_Age_Productivity_Ghana_Results.xlsx",overwrite = T)
#------------------------------------
# Correlation of variables        ####
library(ggcorrplot);library(gtable);library(gridExtra);library(ggridges)
data <- as.data.frame(haven::read_dta("Data/Cereal_Farmer_Age_Productivity_Ghana_data.dta"))
data$CropID <- as.character(haven::as_factor(data$CropID))
data$AgeCat <- as.character(haven::as_factor(data$AgeCat))
data <- data[data$CropID %in% "Pooled",]

corr <- list(round(cor(data[c("Female", "AgeYr", "YerEdu", "Yield", "Area", "OwnLnd", "SeedKg", 
                              "HHLaborAE", "HirdHr", "FertKg", "PestLt",
                              "EqipMech", "EqipIrig", "Credit", "Extension", "HHSizeAE", "Depend")]), 3))

for(age in unique(data$AgeCat)){
  corr[[length(corr)+1]] <- round(cor(data[data$AgeCat %in% age,
                                           c("Female", "AgeYr", "YerEdu", "Yield", "Area", "OwnLnd", "SeedKg", 
                                             "HHLaborAE", "HirdHr", "FertKg", "PestLt",
                                             "EqipMech", "EqipIrig", "Credit", "Extension", "HHSizeAE", "Depend")]), 3)
}

for(age in 1:length(corr)){

  corr[[age]][upper.tri( corr[[age]])] <- NA
  diag(corr[[age]]) <- NA
  corr[[age]] <- reshape2::melt(corr[[age]])
  corr[[age]]$AgeCat <- age
}

corr  <- as.data.frame(data.table::rbindlist(corr,fill = TRUE))
corr$AgeCat <- factor(corr$AgeCat,levels = 1:4,labels = c("Full sample",
                                                          "15-35 years",
                                                          "36-59 years",
                                                          "60 years and above"))

NAMES <- c("Female", "AgeYr", "YerEdu", "Yield", "Area", "OwnLnd", "SeedKg", 
           "HHLaborAE", "HirdHr", "FertKg", "PestLt",
           "EqipMech", "EqipIrig", "Credit", "Extension", "HHSizeAE", "Depend")

LABS <- c("Female farmer (dummy)","Farmer age (years)","Farmer education (years)",
  "Pooled yield (Kg/ha)","Land (ha) ","Land owned (dummy)","Seed (GHC/ha)",
  "Household labor (AE)","Hired labor (man-days/ha)","Fertilizer (Kg/ha)",
  "Pesticide (Liter/ha)","Mechanization (dummy)","Irrigation (dummy)",
  "Credit (dummy)","Extension (dummy)","Household size (AE)","Household dependency (ratio)")

corr$Var1 <- as.numeric(as.character(factor(corr$Var1 ,levels = NAMES,labels = 1:length(NAMES))))
corr$Var1 <- factor(corr$Var1 ,levels = 1:length(NAMES),labels = LABS )

corr$Var2 <- as.numeric(as.character(factor(corr$Var2 ,levels = NAMES,labels = 1:length(NAMES))))
corr$Var2 <- factor(corr$Var2 ,levels = 1:length(NAMES),labels = LABS )

ggtheme = ggplot2::theme_minimal
colors = c("blue", "white", "red")
outline.color = "gray"
legend.title = "Corr"
tl.cex = 12
tl.srt = 45
fig <-  ggplot(data=corr,aes(Var1, Var2, fill = value)) + 
  geom_tile(color = outline.color) + 
  scale_fill_gradient2(low = colors[1], 
                       high = colors[3], 
                       mid = colors[2], 
                       midpoint = 0,
                       limit = c(-1, 1), 
                       space = "Lab", 
                       name = legend.title,
                       na.value = "#DCDCDC") + 
  ggtheme() + theme(axis.text.x = element_text(angle = tl.srt,
                                               vjust = 1, 
                                               size = tl.cex, hjust = 1), 
                    axis.text.y = ggplot2::element_text(size = tl.cex)) + 
  coord_fixed() +
  facet_wrap("AgeCat", ncol=2)

fig <-  fig + ERSTheme::ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=8),
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))

ggsave("Results/Figs/corrplot.png", fig, dpi = 600,width = 8, height =8)
#------------------------------------