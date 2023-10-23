source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))


if(length(actual_tables$MEDICINES)>0){
  system.time(source(paste0(pre_dir,"Step_08_00_MEDICINES_L3.R")))
  
  if(subpopulations_present=="No"){
    #system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "MEDICINES_Overview_Completeness_L3.html"))) 
    #system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","MEDICINES_Counts_L3.html"))) 
    #system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","MEDICINES_Rates_L3.html"))) 
    
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Overview_Completeness_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", "MEDICINES_Overview_Completeness_L3.html"))) 
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Counts_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","MEDICINES_Counts_L3.html"))) 
    system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Rates_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"MEDICINES/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","MEDICINES_Rates_L3.html"))) 
    
  } else {
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Overview_Completeness_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_",subpopulations_names[a],"_MEDICINES_Overview_Completeness_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Counts_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_",subpopulations_names[a],"_MEDICINES_Counts_L3.html")))  
    }
    for (a in 1: length(subpopulations_names)){
      system.time(render(paste0(pre_dir,"/Report_08_MEDICINES_Rates_L3.Rmd"), output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_",subpopulations_names[a],"_MEDICINES_Rates_L3.html")))  
    }
  }
}
source(paste0(pre_dir,"save_environment.R"))