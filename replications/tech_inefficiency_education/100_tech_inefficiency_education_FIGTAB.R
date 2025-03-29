
rm(list=ls(all=TRUE));gc()
setwd(ifelse(Sys.info()['sysname'] =="Windows",getwd(),"/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/"))
PROJECT <- getwd()
source(paste0(getwd(),"/codes/figures_and_tables.R"))
setwd(paste0(getwd(),"/replications/tech_inefficiency_education"))
dir.create("results")
dir.create("results/figures")
dir.create("results/figuresData")
mspecs_optimal <- readRDS("results/mspecs_optimal.rds")
Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- tab_main_specification()
wb <- openxlsx::loadWorkbook("results/tech_inefficiency_education_results_smf.xlsx")
openxlsx::writeData(wb, sheet = "msf",res , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,"results/tech_inefficiency_education_results_smf.xlsx",overwrite = T)

# Fig - Heterogeneity          
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig <- fig_heterogeneity00(res=readRDS("results/estimations/CropID_Pooled_educated_TL_hnormal_optimal.rds")$disagscors,
                    y_title="Percentage Difference (Educated less Uneducated)\n")
ggsave("results/figures/heterogeneity_crop_region.png", fig[["crop_region"]],dpi = 600,width = 8, height = 5)
ggsave("results/figures/heterogeneity_genderAge.png", fig[["genderAge"]],dpi = 600,width = 5, height = 5)

# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nDifference (%) [Educated less Uneducated]",
               res_list = c("results/estimations/CropID_Pooled_educated_CD_hnormal_optimal.rds",
                             list.files("results/estimations/",pattern = "CropID_Pooled_educated_TL_",full.names = T)))

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])

fig_input_te(y_title="\nEducation gap (%)",tech_lable=c("Full sample", "Educated sample", "Uneducated sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()


# Fig - Distribution 
dataFrq <- readRDS("results/estimations/CropID_Pooled_educated_TL_hnormal_fullset.rds")
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("Uneducated","Educated"))
fig_dsistribution(dataFrq)
