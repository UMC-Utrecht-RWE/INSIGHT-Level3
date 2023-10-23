source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS), length(actual_tables$SURVEY_ID))>0){
  system.time(source(paste0(pre_dir,"Step_11_00_PREGNANCY_L3.R")))
  
  if(subpopulations_present=="No"){
    # system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Overview_Completeness_L3.html"))) 
    # system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Counts_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Counts_L3.html"))) 
    # system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Rates_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Rates_L3.html"))) 
    
    system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Overview_Completeness_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Overview_Completeness_L3.html"))) 
    system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Counts_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Counts_L3.html"))) 
    system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Rates_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","PREGNANCY_Rates_L3.html"))) 
    
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[a],"_PREGNANCY_Overview_Completeness_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Counts_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[a],"_PREGNANCY_Counts_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_11_PREGNANCY_Rates_L3.Rmd"), output_dir = paste0(output_dir,"PREGNANCY/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[a],"_PREGNANCY_Rates_L3.html")))  
    }
  }
}
source(paste0(pre_dir,"save_environment.R"))
