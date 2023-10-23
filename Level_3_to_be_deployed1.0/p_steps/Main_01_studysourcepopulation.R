#####Study_source_population#####
source("packages.R")
source("99_path.R")
source(paste0(pre_dir, "parameters/info.R"))
source(paste0(pre_dir,"parameters/study_parameters.R"))
source(paste0(pre_dir,"/lifestyle_list_creation.R"))
setwd(projectFolder)

system.time(source(paste0(pre_dir,"study_source_population_script.R")))
#Create report
for(i in readRDS(paste0(std_pop_tmp,"SCHEME_06.rds"))[["subpopulations"]]){
  
  if(SUBP) {
    report_dir1 <- paste0(std_source_pop_dir,i)
    report_dir2 <- paste0(std_source_pop_dir,i,"/Masked/")
    
  }else{
    report_dir1 <- paste0(std_source_pop_dir,"Reports/")
    report_dir2 <- paste0(std_source_pop_dir,"Masked/")
  }
  
  rmarkdown::render(paste0(pre_dir,"Report_01_StudyPopulation.Rmd"),
                    output_file = paste0(report_dir1,"Report_01_Study_population_",i,".html"),
                    output_dir = report_dir1
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_01_StudyPopulation_GDPR.Rmd"),
                    output_file = paste0(std_source_pop_dir,"GDPR/","Report_01_Study_population_",i,".html"),
                    output_dir = paste0(std_source_pop_dir,"GDPR/")
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_02_Dates.Rmd"),
                    output_file = paste0(report_dir1,"/","Report_02_Dates_",i,".html"),
                    output_dir = report_dir1
  )
  rmarkdown::render(paste0(pre_dir,"Report_02_Dates_GDPR.Rmd"),
                    output_file = paste0(std_source_pop_dir,"GDPR/", "Report_02_Dates_",i,".html"),
                    output_dir = paste0(std_source_pop_dir,"GDPR/")
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_03_VisitsLifestyle.Rmd"),
                    output_file = paste0(report_dir1,"/","Report_03_VisitsLifestyle_",i,".html"),
                    output_dir = report_dir1
  )
  
  rmarkdown::render(paste0(pre_dir,"Report_03_VisitsLifestyle_GDPR.Rmd"),
                    output_file = paste0(std_source_pop_dir,"GDPR/","Report_03_VisitsLifestyle_",i,".html"),
                    output_dir = paste0(std_source_pop_dir,"GDPR/")
  )
  
  rm(report_dir1,report_dir2)
}

#rename reports
date_DAP_name_part<-paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_")
files <- list.files(path = paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/Reports/"))

# Rename the files
for (file in files) {
  new_name <- paste0(date_DAP_name_part,file)
  file.rename(from = paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/Reports/", file), 
              to = paste0(projectFolder,"/g_output/STUDY_SOURCE_POPULATION/Reports/", new_name))
}

source(paste0(pre_dir,"save_environment.R"))