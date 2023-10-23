####ForDashboard folder####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("99_path.R")
source(paste0(projectFolder,"/p_steps/for_dashboard.R"))
