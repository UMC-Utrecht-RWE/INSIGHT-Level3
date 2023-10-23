rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

#####Study_source_population#####
if(run_study_source_population=="Yes"){
source(paste0(projectFolder,"/p_steps/Main_01_studysourcepopulation.R"))
}

#####Medicine exposure#####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

if(run_medicines=="Yes"){
  source(paste0(projectFolder,"/p_steps/Main_02_medicines.R"))
}

####Vaccine exposure####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

if(run_vaccines=="Yes"){
  source(paste0(projectFolder,"/p_steps/Main_03_vaccines.R"))
}
#####Diagnoses#####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

if(run_diagnoses=="Yes"){
  source(paste0(projectFolder,"/p_steps/Main_04_diagnoses.R"))
}

#####Pregnancy#####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

if(run_pregnancy=="Yes"){
  source(paste0(projectFolder,"/p_steps/Main_05_pregnancy.R"))
}
#####Populations of interest####
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source(paste0(projectFolder,"/p_steps/running_scripts_info.R"))

if(run_poi=="Yes"){
  source(paste0(projectFolder,"/p_steps/Main_06_poi.R"))
}
####EUROCAT Indicators####
# rm(list=ls())
# if(!require(rstudioapi)){install.packages("rstudioapi")}
# library(rstudioapi)
# 
# projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(projectFolder)
# source("packages.R")
# source("99_path.R")
# load(paste0(g_intermediate,"environment.RData"))
# Rmd_EUROCAT<-paste0(pre_dir,"/EUROCAT_DQI_L3.Rmd")
# system.time(source(paste0(pre_dir,"eurocat_dqi.R")))
# 
# if(subpopulations_present=="No"){
#   system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = "EUROCAT_DQI_L3.html")) 
# } else {
#   for (a in 1: length(subpopulations_names)){
#     system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = paste0(subpopulations_names[a],"_EUROCAT_DQI_L3.html")))  
#   }
# }
# source(paste0(pre_dir,"save_environment.R"))
# 


