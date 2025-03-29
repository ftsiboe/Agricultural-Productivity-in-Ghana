
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
fig_heterogeneity00(res=readRDS("results/estimations/CropID_Pooled_educated_TL_hnormal_optimal.rds")$disagscors,
                    y_title="Percentage Difference (Educated less Uneducated)\n")

# Fig - Robustness              
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness("\nDifference (%) [Educated less Uneducated]")

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])

fig_input_te(y_title="\nEducation gap (%)",tech_lable=c("Full sample", "Educated sample", "Uneducated sample"))

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance()



