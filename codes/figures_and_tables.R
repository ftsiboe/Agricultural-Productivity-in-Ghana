#-----------------------------------------
# Preliminaries                        ####
library('magrittr');library(ggplot2);library(rasterVis);library(gridExtra)
library(dplyr);library(gtable)

#-----------------------------------------
# ERS Theme                            ####
#' ERS Theme
#'
#' @examples
#' ggplot2::ggplot() + ERSTheme::ers_theme()
#' @export

ers_theme = function() {
  ggplot2::theme(
    line                  = ggplot2::element_line(
      colour = "black",
      size = 0.5,
      linetype = 1,
      lineend = "butt",
      arrow = FALSE
    ),
    rect                  = ggplot2::element_rect(
      fill = "white",
      colour = NA,
      size = 0.5,
      linetype = 1
    ),
    text                  = ggplot2::element_text(
      family = "sans",
      face = "plain",
      colour = "black",
      size = 9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.x          = ggplot2::element_text(
      vjust = 1,
      margin = ggplot2::margin(
        t = 2.75,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.x.top      = ggplot2::element_text(
      vjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 2.75,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.y          = ggplot2::element_blank(),
    axis.text.x           = ggplot2::element_text(
      vjust = 1,
      margin = ggplot2::margin(
        t = 2.2,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.x.top       = ggplot2::element_text(
      vjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 2.2,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.y           = ggplot2::element_text(
      hjust = 1,
      margin = ggplot2::margin(
        t = 0,
        r = 2.2,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.y.right     = ggplot2::element_text(
      hjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 2.2,
        unit = "pt"
      )
    ),
    axis.ticks            = ggplot2::element_blank(),
    axis.line             = ggplot2::element_blank(),
    legend.margin         = ggplot2::margin(
      t = 5.5,
      r = 5.5,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    ),
    legend.spacing        = ggplot2::unit(11, units = "pt"),
    legend.key            = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.size       = ggplot2::unit(1.2, units = "lines"),
    legend.title          = ggplot2::element_text(hjust = 0, size = 9),
    legend.text           = ggplot2::element_text(size = 9),
    legend.position       = "bottom",
    legend.justification  = "center",
    legend.box.margin     = ggplot2::margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0,
      unit = "cm"
    ),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing    = ggplot2::unit(11, units = "pt"),
    panel.background      = NULL,
    panel.border          = ggplot2::element_blank(),
    panel.spacing         = ggplot2::unit(5.5, units = "pt"),
    panel.grid.major.x    = ggplot2::element_blank(),
    panel.grid.minor.x    = ggplot2::element_blank(),
    panel.grid.major.y    = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor.y    = ggplot2::element_line(size = ggplot2::rel(0.5)),
    plot.title            = ggplot2::element_text(
      face = "bold",
      size = 10.5,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 5.5,
        r = 0,
        b = 5.5,
        l = 0,
        unit = "pt"
      )
    ),
    plot.title.position   = "plot",
    plot.subtitle         = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 5,
        l = 0,
        unit = "pt"
      )
    ),
    plot.caption          = ggplot2::element_text(
      size = 8,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 5.5,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    plot.caption.position = "plot",
    plot.tag              = ggplot2::element_text(
      size = 8,
      hjust = 0,
      vjust = 0.5,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 10,
        l = 0,
        unit = "pt"
      )
    ),
    plot.tag.position     = c(0, 1),
    plot.margin           = ggplot2::margin(
      t = 10,
      r = 5.5,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    ),
    strip.background      = ggplot2::element_rect(fill = "grey85", colour = "grey20"),
    strip.placement       = "inside",
    strip.text            = ggplot2::element_text(
      colour = "grey10",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(
        t = 4.4,
        r = 4.4,
        b = 4.4,
        l = 4.4,
        unit = "pt"
      )
    ),
    strip.text.y          = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(2.75, units = "pt"),
    strip.switch.pad.wrap = ggplot2::unit(2.75, units = "pt"),
    strip.text.y.left     = ggplot2::element_text(angle = 90)
  )
}
#-----------------------------------------
# Main Specification                   ####
tab_main_specification <- function(){
  res <- as.data.frame(
    data.table::rbindlist(
      lapply(
        list.files("results/estimations/",pattern = "_optimal.rds",full.names = T),
        function(file) {
          DONE <- NULL
          tryCatch({
            # file <- list.files("results/estimations/",pattern = "TL_hnormal_optimal.rds",full.names = T)[5]
            res <- readRDS(file)
            
            sf_estm <- res$sf_estm
            sf_estm$jack_pv <- ifelse(sf_estm$CoefName %in% 
                                        c("Nobs","olsSkew","olsM3Okay","CoelliM3Test","AgostinoSkw","LRInef",
                                          "mlLoglik","LRT_LL0","LRT_LL1","LRT_DF0","LRT_DF1","LRT_Tv","LRT_DF"),
                                      sf_estm$Pvalue,sf_estm$jack_pv)
            
            unique(sf_estm$CoefName)
            
            sf_estm$estm_type  <- "sf_estm"
            sf_estm$level_type <- "level"
            sf_estm <- sf_estm[c("TCH","FXN","DIS","estm_type","level_type","sample","Survey","restrict","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
            
            el_mean <- res$el_mean
            el_mean <- el_mean[el_mean$stat %in% "wmean",]
            el_mean$estm_type  <- "el_mean"
            el_mean$level_type <- gsub("elasticity","",el_mean$CoefName)
            el_mean$level_type <- ifelse(el_mean$level_type %in% "","level",el_mean$level_type)
            el_mean$CoefName <- el_mean$input
            el_mean <- el_mean[c("TCH","FXN","DIS","estm_type","level_type","sample","Survey","restrict","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
            
            ef_mean <- res$ef_mean
            ef_mean <- ef_mean[ef_mean$stat %in% "wmean",]
            ef_mean <- ef_mean[ef_mean$estType %in% "teBC",]
            ef_mean$estm_type  <- "ef_mean"
            ef_mean$level_type <- gsub("efficiency","",ef_mean$CoefName)
            ef_mean$level_type <- ifelse(ef_mean$level_type %in% "","level",ef_mean$level_type)
            ef_mean$CoefName <- ef_mean$type
            ef_mean <- ef_mean[c("TCH","FXN","DIS","estm_type","level_type","sample","Survey","restrict","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
            
            # chek <- unique(el_mean[c("type","sample","Survey","restrict","Tech","CoefName")])
            
            res <- rbind(ef_mean,el_mean,sf_estm)
            DONE <- res[c("TCH","FXN","DIS","estm_type","Survey","CoefName","sample","restrict","level_type","Tech","Estimate","Estimate.sd","jack_pv")]
            
          }, error=function(e){})
          return(DONE)
        }), fill = TRUE))
  return(res)
}
#-----------------------------------------
# Fig - Heterogeneity                  ####
fig_heterogeneity00 <- function(res,y_title){
  res$disasg <- as.character(res$disagscors_var)
  res$level <- as.character(res$disagscors_level)
  res <- res[res$estType %in% "teBC",]
  res <- res[res$Survey %in% "GLSS0",]
  res <- res[res$restrict %in% "Restricted",]
  res <- res[res$stat %in% "mean",]
  res <- res[!res$sample %in% "unmatched",]
  res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
  
  res <- res[c("disasg","level","FXN","DIS","Survey","input","TCH","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
  
  res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "1","Farmer aged\n35 or less",res$level)
  res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "2","Farmer aged\n36 to 59",res$level)
  res$level <- ifelse(res$disasg %in% "AgeCat" & res$level == "3","Farmer aged\n60 or more",res$level)
  
  res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "0","Farmer with\nno formal\neducation",res$level)
  res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "1","Farmer with\nprimary education",res$level)
  res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "2","Farmer with\njunior secondary\nschool education",res$level)
  res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "3","Farmer with\nsenior secondary\nschool education",res$level)
  res$level <- ifelse(res$disasg %in% "EduLevel" & res$level == "4","Farmer with\npost senior\nsecondary school\neducation",res$level)
  
  res$level <- ifelse(res$disasg %in% "Female" & res$level == "1","Female\nfarmer",res$level)
  res$level <- ifelse(res$disasg %in% "Female" & res$level == "0","Male\nfarmer",res$level)
  
  eff_fig_fxn <- function(disasg,type=NULL,xsize=7,title=""){
    # disasg <- c("AgeCat","Female");type<-"farmer"
    data   <- unique(rbind(res[(res$disasg %in% "CropID" & res$level %in% "Pooled"),],res[res$disasg %in% disasg,]))
    myrank <- data[data$input %in% "MTE",]
    myrank <- myrank[myrank$Tech %in% min(data$Tech,na.rm=T),]
    
    if("farmer" %in% type){
      myrank <- myrank[order(myrank$level),]
      myrank <- rbind(myrank[myrank$level %in% "Pooled",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$disasg %in% "Female",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$disasg %in% "AgeCat",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$level %in% "Farmer with\nno formal\neducation",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$level %in% "Farmer with\nprimary education",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$level %in% "Farmer with\njunior secondary\nschool education",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$level %in% "Farmer with\nsenior secondary\nschool education",c("disasg","level","FXN","DIS","Survey","TCH")],
                      myrank[myrank$level %in% "Farmer with\npost senior\nsecondary school\neducation",c("disasg","level","FXN","DIS","Survey","TCH")])
    }
    if(is.null(type)){
      myrank <- myrank[order(myrank$Estimate),c("disasg","level","FXN","DIS","Survey","TCH")]
      myrank <- rbind(myrank[myrank$level %in% "Pooled",],myrank[!myrank$level %in% "Pooled",])
    }
    myrank$x1 <- 1:nrow(myrank)
    data <- dplyr::inner_join(myrank,data,by=names(myrank)[names(myrank) %in% names(data)])
    data$x2<-ifelse(data$input =="TGR",1,NA)
    data$x2<-ifelse(data$input =="TE",2,data$x2)
    data$x2<-ifelse(data$input =="MTE",3,data$x2)
    data$x <- as.integer(as.factor(paste0(stringr::str_pad(data$x1,pad="0",3),stringr::str_pad(data$x2,pad="0",3))))
    data <- data[order(data$x),]
    myrank <- unique(data[c("x2","x","x1","input","level","disasg")])
    myrank_lines <- data[data$input %in% "MTE",]
    myrank <- data[data$input %in% "TE",]
    
    data$input<- factor(data$x2,levels = 1:3,labels = c("Technology gap ratio","Technical efficiency","Meta-technical-efficiency"))
    
    fig <- ggplot(data=data,aes(x = x,y=Estimate ,group=input,shape=input,colour=input,fill=input)) +
      geom_vline(xintercept=myrank_lines$x[1:(nrow(myrank_lines)-1)]+0.5, lwd=0.5, lty=5,color = "#808080") +
      geom_errorbar(aes(ymax = Estimate + Estimate.sd, ymin = Estimate - Estimate.sd), width = 0.25) +
      geom_point(size=1.5) + 
      #facet_wrap(~ input,ncol=1,scales = "free_y") +
      scale_x_continuous(breaks = myrank$x,labels = myrank$level) +
      labs(title=title,x="", y ="",caption = "") +
      scale_fill_manual(name="",values = c("thistle","violet","purple")) +
      scale_color_manual(name="",values = c("thistle","violet","purple")) +
      scale_shape_manual(name="",values = c(21,22,23,24,25,8,4)) +
      ers_theme() +
      theme(axis.title= element_text(size=9,color="black"),
            plot.title  = element_text(size = 8),
            axis.text.y = element_text(size = 5),
            axis.text.x = element_text(size = xsize), #
            axis.title.y= element_text(size=6,color="black"),
            legend.position="none",
            legend.title=element_text(size=7),
            legend.text=element_text(size=7),
            plot.caption = element_text(size=8),
            strip.text = element_text(size = 10),
            strip.background = element_rect(fill = "white", colour = "black", size = 1))
    
    write.csv(data[order(data$input),c("input","disasg","level","Estimate","Estimate.sd","jack_pv")],
              file=paste0("results/figuresData/",paste0(disasg,collapse = "_"),".csv"))
    return(fig)
  }
  
  grobs <- ggplotGrob(eff_fig_fxn(disasg = "CROP",xsize=5.5,title="(A) Major crops")+theme(legend.position="bottom"))$grobs
  legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
  Ylab<-ggplot()+geom_text(aes(x=0,y=0),label=y_title,size=3,angle=90)+theme_void()
  
  marg <- c(0.05,0.5,-0.5,0.5)
  fig.CropID   <- eff_fig_fxn(disasg = "CROP",xsize=5.5,title="(A) Major crops")
  fig.Location <- eff_fig_fxn(disasg = "Region",title="(B) Administrative regions")
  fig <- cowplot::plot_grid(
    fig.CropID + theme(plot.margin = unit(marg,"cm"))  ,
    fig.Location + theme(plot.margin = unit(marg,"cm")) ,
    ncol=1, align="v",rel_heights=c(1,1),
    greedy=F)
  fig <- cowplot::plot_grid(fig,legend,ncol=1,rel_heights=c(1,0.1))
  fig <- cowplot::plot_grid(Ylab,fig,nrow=1,rel_widths =c(0.002,0.03))
  ggsave("results/figures/heterogeneity_crop_region.png", fig,dpi = 600,width = 8, height = 5)
  
  fig.Farmer   <- eff_fig_fxn(disasg = c("AgeCat","Female","EduLevel"),
                              type="farmer",xsize=7) +
    labs(title="",x="", y =y_title,caption = "") +
    theme(legend.position="bottom")
  ggsave("results/figures/heterogeneity_genderAge.png", fig.Farmer,dpi = 600,width = 5, height = 5)
  return("done-heterogeneity")
}
#-----------------------------------------
# Fig - Robustness                     ####

fig_robustness <- function(y_title){
data <- as.data.frame(
  data.table::rbindlist(
    lapply(
      c("results/estimations/CropID_Pooled_educated_CD_hnormal_optimal.rds",
        list.files("results/estimations/",pattern = "CropID_Pooled_educated_TL_",full.names = T)),
      function(file) {
        DONE <- NULL
        tryCatch({
          # file <- list.files("Results/Estimations/",pattern = "TL_hnormal_optimal.rds",full.names = T)[1]
          ef_mean <- readRDS(file)$ef_mean
          #ef_mean <- ef_mean[ef_mean$stat %in% "wmean",]
          #ef_mean <- ef_mean[ef_mean$estType %in% "teBC",]
          ef_mean <- ef_mean[ef_mean$type %in% c("TE","TGR","MTE"),]
          ef_mean <- ef_mean[ef_mean$CoefName %in% c("efficiencyGap_pct"),]
          #ef_mean <- ef_mean[ef_mean$restrict %in% c("Restricted"),]
          ef_mean <- ef_mean[ef_mean$Survey %in% c("GLSS0"),]
          DONE <- ef_mean
          
        }, error=function(e){})
        return(DONE)
      }), fill = TRUE))


mainest <- unique(data[(data$FXN %in% "TL" & data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                          data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link) & 
                          data$restrict %in% c("Restricted")),])
mainest <- mainest[c("type","Estimate","Estimate.sd")]
names(mainest) <- c("type","mainest","mainest.sd")

production <- unique(data[(data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                             data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link) & 
                             data$restrict %in% c("Restricted")),])
production$options <- ifelse(production$FXN %in% "CD","Cobb-Douglas production function",NA)
production$options <- ifelse(production$FXN %in% "TL","Translog production function",production$options)
production$options <- ifelse(production$FXN %in% "LN","Linear production function",production$options)
production$options <- ifelse(production$FXN %in% "QD","Quadratic production function",production$options)
production$options <- ifelse(production$FXN %in% "GP","Generalized production function",production$options)
production$options <- ifelse(production$FXN %in% "TP","Transcendental production function",production$options)
production <- production[c("options","type","Estimate","Estimate.sd")]
production$dimension <- "(A) production"
production

distribution <- unique(data[(data$FXN %in% "TL" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                               data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link) & 
                               data$restrict %in% c("Restricted")),])
distribution$options <- ifelse(distribution$DIS %in% "hnormal","Half normal distribution",NA)
distribution$options <- ifelse(distribution$DIS %in% "tnormal","Truncated normal distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "tnormal_scaled","Scaled truncated normal distribution with the",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "exponential","Exponential distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "rayleigh","Rayleigh distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "uniform","Uniform distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "gamma","Gamma distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "lognormal","Log normal distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "weibull","Weibull distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "tslaplace","Truncated skewed Laplace distribution",distribution$options)
distribution$options <- ifelse(distribution$DIS %in% "genexponential","Generalized exponential distribution",distribution$options)
distribution <- distribution[c("options","type","Estimate","Estimate.sd")]
distribution$Estimate.sd <- ifelse(distribution$options %in% c("Rayleigh distribution","Truncated normal distribution"),NA,distribution$Estimate.sd)
distribution$dimension <- "(B) distribution"
distribution

efficiency <- unique(data[(data$DIS %in% "hnormal" & data$stat %in% "wmean" & 
                             data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link) & 
                             data$restrict %in% c("Restricted")),])
efficiency$options <- ifelse(efficiency$estType %in% "teJLMS","Jondrow et al. (1982) efficiency",NA)
efficiency$options <- ifelse(efficiency$estType %in% "teBC","Battese and Coelli (1988) efficiency",efficiency$options)
efficiency$options <- ifelse(efficiency$estType %in% "teMO","Conditional model efficiency",efficiency$options)
efficiency <- efficiency[c("options","type","Estimate","Estimate.sd")]
efficiency$dimension <- "(C) efficiency"
efficiency

tendency <- unique(data[(data$DIS %in% "hnormal" & data$estType %in% "teBC" & 
                           data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link) & 
                           data$restrict %in% c("Restricted")),])
tendency <- tendency[!tendency$stat %in% "mode",]
tendency$options <- ifelse(tendency$stat %in% "wmean","Weighted mean efficiency aggregation",NA)
tendency$options <- ifelse(tendency$stat %in% "mean","Simple mean efficiency aggregation",tendency$options)
tendency$options <- ifelse(tendency$stat %in% "median","Median efficiency aggregation",tendency$options)
#tendency$options <- ifelse(tendency$stat %in% "mode","modal efficiency aggregation",tendency$options)
tendency <- tendency[c("options","type","Estimate","Estimate.sd")]
tendency$dimension <- "(D) tendency"

sample <-  unique(data[(data$FXN %in% "TL" & data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                          data$restrict %in% c("Restricted")),])
sample$options <- ifelse(sample$sample %in% "unmatched","Unmatched sample",NA)
sample$options <- ifelse(sample$sample %in% "logit","Logit [PS] matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "cauchit","Cauchit [PS] matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "probit","Probit [PS] matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "cloglog","Complementary Log-Log [PS] matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "euclidean","Euclidean matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "robust_mahalanobis","Robust Mahalanobis matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "scaled_euclidean","Scaled Euclidean matched sample",sample$options)
sample$options <- ifelse(sample$sample %in% "mahalanobis","Mahalanobis matched sample",sample$options)
sample <- sample[c("options","type","Estimate","Estimate.sd")]
sample$dimension <- "(E) sample"

Restricted <- unique(data[(data$FXN %in% "TL" & data$DIS %in% "hnormal" & data$stat %in% "wmean" & data$estType %in% "teBC" & 
                             data$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link)),])
Restricted$options <- paste0(Restricted$restrict," production function")
Restricted <- Restricted[c("options","type","Estimate","Estimate.sd")]
Restricted$dimension <- "(F) Production function properties"
Restricted

dataF <- rbind(efficiency,production,distribution,tendency,sample,Restricted)

dataF <- dplyr::inner_join(dataF,mainest,by=c("type"))

xlab <- doBy::summaryBy(Estimate~dimension+options,data=dataF[dataF$type %in% "MTE",],FUN=mean)

xlab <- xlab[order(xlab$Estimate.mean),]
xlab <- xlab[order(xlab$options),]
xlab <- xlab[order(-as.integer(as.factor(xlab$dimension))),]
xlab$x <- 1:nrow(xlab)

xlab <- unique(xlab[c("dimension","options","x")])

dataF <- dplyr::inner_join(dataF,xlab,by=c("dimension","options"))

dataF$type <- as.numeric(as.character(factor(dataF$type,levels = c("MTE","TGR","TE","risk"),labels = 1:4)))
dataF$type <- factor(dataF$type,levels = 1:4,
                     labels = c("Meta-frontier\ntechnical\nefficiency (MTE)",
                                "Technology gap\nratio (TGR)",
                                "Technical\nefficiency (TE)",
                                "Production\ncoefficient\nof variation"))

xline <- xlab[xlab$x %in% doBy::summaryBy(x~dimension,FUN=min,data=xlab)$x.min,"x"]
xline <- xline[2:length(xline)]-0.5

dataF$mainestN <- "Preferred specification estimate and 95% confidence interval"

fig <- ggplot(data=dataF,aes(x=factor(x), y=Estimate, group=1)) +
  geom_vline(xintercept =xline,size = 0.2,color = "gray",lty=1) +
  geom_ribbon(aes(ymin = mainest - mainest.sd*1.96, ymax = mainest + mainest.sd*1.96, fill=mainestN), 
              alpha = 0.5,color="thistle") +
  geom_line(aes(y=mainest,linetype=mainestN,color=mainestN),size=0.5) + 
  geom_errorbar(aes(ymax = Estimate + Estimate.sd*1.96,ymin = Estimate - Estimate.sd*1.96),
                width = 0.25,color="blue") + 
  geom_point(size=1.5,shape=21,fill="purple",color="blue") + 
  scale_fill_manual(values ="thistle") +
  scale_color_manual(values ="black") +
  scale_linetype_manual(values =2) +
  scale_x_discrete(breaks = xlab$x,labels = xlab$options) +
  facet_grid(~type , scales = "free_x") +
  guides(fill = guide_legend(nrow=2,override.aes = list(size=2.5))) + 
  labs(title= "", x = "", y = y_title, caption = "") +
  ers_theme() +
  theme(axis.title= element_text(size=9,color="black"),
        plot.title  = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8), #
        axis.title.y= element_text(size=6,color="black"),
        legend.position="none",
        legend.title=element_text(size=7),
        legend.text=element_text(size=7),
        plot.caption = element_text(size=8),
        strip.text = element_text(size = 10),
        strip.background = element_rect(fill = "white", colour = "black", size = 1)) + coord_flip()

ggsave(paste0("results/figures/robustness.png"),fig,dpi = 600,width = 6.8, height = 7.8)
return("done-robustness")
}
#-----------------------------------------
# Matching TE                          ####

fig_input_te <- function(y_title,tech_lable){

data <- readRDS(paste0("results/te_summary.rds"))
data <- data[data$method %in% mspecs_optimal$method,]
data <- data[data$distance %in% mspecs_optimal$distance,]
data <- data[data$link %in% mspecs_optimal$link,]
data <- data[order(data$est),]
data <- data[data$level %in% c("ATE","ATET","ATEU"),]


data$level <- ifelse(data$level %in% "ATE" ,1,data$level)
data$level <- ifelse(data$level %in% "ATET" ,2,data$level)
data$level <- ifelse(data$level %in% "ATEU" ,3,data$level)
data$level <- factor(as.numeric(data$level),levels = 1:3,
                     labels = tech_lable)

data$outC <- ifelse(data$outcome %in% "HrvstKg" ,1,NA)
data$outC <- ifelse(data$outcome %in% "SeedKg" ,2,data$outC)
data$outC <- ifelse(data$outcome %in% "FertKg" ,3,data$outC)
data$outC <- ifelse(data$outcome %in% "PestLt" ,4,data$outC)
data$outC <- ifelse(data$outcome %in% "Area" ,5,data$outC)
data$outC <- ifelse(data$outcome %in% "HHLaborAE" ,6,data$outC)
data$outC <- ifelse(data$outcome %in% "HirdHr" ,7,data$outC)
data$outC <- ifelse(data$outcome %in% "Extension" ,8,data$outC)
data$outC <- ifelse(data$outcome %in% "OwnLnd" ,9,data$outC)
data$outC <- ifelse(data$outcome %in% "Credit" ,10,data$outC)

data$outC <- factor(data$outC,levels = 1:10,
                    labels = c(
                      "Output (real value)",
                      "Planting materials (real value)", 
                      "Fertilizer (Kg/ha)",
                      "Pesticide (Liter/ha)",
                      "Land (ha)", 
                      "Household labor (AE)",
                      "Hired labor (man-days/ha)",
                      "Extension" ,
                      "OwnLnd" ,
                      "Credit"))


dodge <- position_dodge(width = 0.75)
figTe <- ggplot(data=data,aes(x=reorder(outC,est), y=est,group=level,color=level,fill=level,shape=level)) +
  geom_hline(yintercept =0,size = 0.5,color = "black") +
  geom_errorbar(aes(ymax = est+jack_se*1.96 , ymin = est-jack_se*1.96 ), width = 0.25,colour="blue", position = dodge) +
  geom_point(position = dodge,size=2.5) +
  labs(title="", x="",
       y = y_title,caption ="") +
  scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
  ers_theme() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=8),
        legend.title=element_blank(),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x = element_text(size = 8, colour="black"), #
        axis.text.y = element_text(size = 8, colour="black"),
        plot.caption = element_text(size=10, hjust = 0 ,vjust =0, face = "italic"),
        strip.text = element_text(size = 11),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))+ coord_flip();figTe

write.csv(data,file=paste0("results/figuresData/input_TE_data.csv"))

ggsave(paste0("results/figures/input_TE.png"), figTe,dpi = 600,width = 6, height =5)
return("done-input_TE")
}
#-----------------------------------------
# Figure - Covariate balance           ####
fig_covariate_balance <- function(){
  bal_tab <- readRDS(paste0("results/balance_table.rds"))
  ranking <- readRDS(paste0("results/mspecs_ranking.rds"))
  mspecs  <- readRDS(paste0("results/mspecs.rds"))
  CovBalDATA <- rbind(bal_tab[(bal_tab$sample %in% "Un" & bal_tab$ARRAY %in% 5),],
                      bal_tab[bal_tab$sample %in% "Adj",])
  
  CovBalDATA$sample <- ifelse(CovBalDATA$sample %in% "Un",CovBalDATA$sample,
                              ifelse(CovBalDATA$link %in% NA,CovBalDATA$distance ,CovBalDATA$link))
  
  CovBalDATA <- dplyr::inner_join(CovBalDATA,ranking[c("ARRAY","ID","name")],by="ARRAY")
  CovBalDATA$ID <- ifelse(CovBalDATA$sample %in% "Un",0,CovBalDATA$ID)
  CovBalDATA$sample <- factor(CovBalDATA$ID,levels = c(0,ranking$ID),labels = c("Unmatched",ranking$name))
  #unique(CovBalDATA$Coef)
  
  mval <- c("HHSizeAE","FmleAERt","Depend",
            "factor(Female)","AgeYr","YerEdu","factor(Head)_Head","factor(Head)_Spouse of Head","factor(Head)_Member",
            "CrpMix","Area_Maize","Area_Rice","Area_Millet","Area_Sorghum","Area_Beans","Area_Peanut","Area_Cassava",
            "Area_Yam","Area_Cocoyam","Area_Plantain","Area_Pepper","Area_Okra","Area_Tomatoe","Area_Cocoa","Area_Palm",
            "factor(Credit)","factor(OwnLnd)","factor(Ethnic)_Akan","factor(Ethnic)_Ewe","factor(Ethnic)_Ga-Dangme","factor(Ethnic)_Guan",
            "factor(Ethnic)_Gurma","factor(Ethnic)_Gursi","factor(Ethnic)_Mande","factor(Ethnic)_Mole-Dagbani",
            "factor(Ethnic)_Non-Ghana","factor(Ethnic)_Other","factor(Marital)_None","factor(Marital)_Union",
            "factor(Marital)_Married","factor(Religion)_None","factor(Religion)_Catholic","factor(Religion)_Protestant",
            "factor(Religion)_Christian","factor(Religion)_Islam","factor(Region)_Ashanti","factor(Region)_Brong Ahafo",
            "factor(Region)_Central","factor(Region)_Eastern","factor(Region)_Greater Accra","factor(Region)_Northern",
            "factor(Region)_Upper East","factor(Region)_Upper West","factor(Region)_Volta","factor(Region)_Western",
            "factor(Ecozon)_Coastal Savanna","factor(Ecozon)_Forest Zone","factor(Ecozon)_Guinea Savanah","factor(Ecozon)_Sudan Savanah",
            "factor(Ecozon)_Transitional Zone","factor(Locality)_Urban","factor(Survey)_GLSS7")
  
  mlab <- c("Household:size","Household:females","Household:dependency",
            "Farmer:Female ","Farmer:age","Farmer:education","Farmer:household head","Farmer:spouse of head","Farmer:household member",
            "Crop diversification","Share of land allocated toMaize","Land share:Rice","Land share:Millet",
            "Land share:Sorghum","Land share:Beans","Land share:Peanut","Land share:Cassava",
            "Land share:Yam","Land share:Cocoyam","Land share:Plantain","Land share:Pepper",
            "Land share:Okra","Land share:Tomatoe","Land share:Cocoa","Land share:Palm",
            "Credit","Land owned","Ethnicity:Akan","Ethnicity:Ewe","Ethnicity:Ga-Dangme","Ethnicity:Guan",
            "Ethnicity:Gurma","Ethnicity:Gursi","Ethnicity:Mande","Ethnicity:Mole-Dagbani",
            "Ethnicity:Non-Ghana","Ethnicity:Other","Marital status:None","Marital status:Union",
            "Marital status:Married","Religion:None","Religion:Catholic","Religion:Protestant",
            "Religion:Christian","Religion:Islam","Region:Ashanti","Region:Brong Ahafo",
            "Region:Central","Region:Eastern","Region:Greater Accra","Region:Northern",
            "Region:Upper East","Region:Upper West","Region:Volta","Region:Western",
            "Ecology:Coastal Savanna","Ecology:Forest Zone","Ecology:Guinea Savanah","Ecology:Sudan Savanah",
            "Ecology:Transitional Zone","Urban Locality","GLSS7 Survey")
  
  # commodities <- readRDS("N:/ARMS/ARMS Work Share/FTsiboe/Data/apmFarms/commodities.rds")
  
  for(i in 1:length(mval)){
    CovBalDATA$Coef <- ifelse(CovBalDATA$Coef %in% mval[i]  ,i,CovBalDATA$Coef)
  }
  
  CovBalDATA$Coef <- as.numeric(as.character(CovBalDATA$Coef))
  CovBalDATA <- CovBalDATA[!CovBalDATA$Coef %in% NA,]
  CovBalDATA$Coef <- factor(CovBalDATA$Coef,levels = 1:length(mlab),labels = mlab)
  
  CovBalDATA$stat <- ifelse(CovBalDATA$stat %in% "Diff"  ,1,CovBalDATA$stat)
  CovBalDATA$stat <- ifelse(CovBalDATA$stat %in% "V_Ratio" ,2,CovBalDATA$stat)
  CovBalDATA$stat <- ifelse(CovBalDATA$stat %in% "KS" ,3,CovBalDATA$stat)
  
  CovBalDATA$stat <- as.numeric(as.character(CovBalDATA$stat))
  
  CovBalDATA$stat <- factor(
    CovBalDATA$stat,levels = 1:3,
    labels =c("Absolute Standardized Mean Differences",
              "Variance Ratios",
              "Kolmogorov-Smirnov (KS) Statistics"))
  
  CovBalDATA <- CovBalDATA[!CovBalDATA$value %in% NA,]
  CovBalDATA <- CovBalDATA[!CovBalDATA$Coef %in% NA,]
  balance <- ggplot(
    data=CovBalDATA,
    aes(x=value,y=reorder(Coef,1/as.integer(Coef),na.rm = TRUE),group=sample,fill=sample,color=sample,shape=sample)) +
    geom_point() +
    geom_point(data=CovBalDATA[CovBalDATA$ID %in% nrow(ranking),],
               aes(x=value,y=reorder(Coef,1/as.integer(Coef),na.rm = TRUE),group=sample),
               color="purple",shape=11,fill="purple") +
    geom_vline(data=data.frame(
      x=c(0,1,0),
      stat=factor(
        c(1:3),levels = 1:3,
        labels =c("Absolute Standardized Mean Differences",
                  "Variance Ratios",
                  "Kolmogorov-Smirnov (KS) Statistics"))),
      aes(xintercept =x),size = 0.5,color = "black") +
    scale_fill_manual("Sample:",values =c("violet","thistle","thistle","thistle","thistle",
                                          "thistle","thistle","thistle","purple")) +
    scale_color_manual("Sample:",values =c("violet","thistle","thistle","thistle","thistle",
                                           "thistle","thistle","thistle","purple")) +
    scale_shape_manual("Sample:",values = c(21,25,24,22,23,3,4,8,11)) +
    labs(title="", x="", y = "",caption ="") +
    facet_wrap( ~ stat  ,scales = "free_x",ncol=3) +
    #ers_theme() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    theme(legend.position="bottom")+
    theme(legend.text=element_text(size=8),
          legend.title=element_text(size=8),
          plot.title=element_text(size=10),
          axis.title.y=element_text(size=7),
          axis.title.x=element_text(size=10),
          axis.text.x = element_text(size = 8, colour="black"), #
          axis.text.y = element_text(size = 6, colour="black"),
          plot.caption = element_text(size=11,hjust = 0 ,vjust = 0, face = "italic"),
          strip.text = element_text(size = 8),
          strip.background = element_rect(fill = "white", colour = "black", size = 1));balance
  
  ggsave(paste0("results/figures/Covariate_balance_variance.png"), balance,dpi = 600,width = 11, height = 7)
  return("done-Covariate_balance_variance")
}
#-----------------------------------------