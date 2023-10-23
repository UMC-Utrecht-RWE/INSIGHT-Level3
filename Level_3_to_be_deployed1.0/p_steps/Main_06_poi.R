source("packages.R")
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))

if(sum(length(list.files(paste0(populations_dir,"DIAGNOSES/"))),length(list.files(paste0(populations_dir,"PREGNANCY/"))))>0){
  if(sum(length(list.files(paste0(populations_dir,"MEDICINES/"))),length(list.files(paste0(populations_dir,"VACCINES/"))))>0){
    
    system.time(source(paste0(pre_dir,"Step_12_00_POI_L3.R")))
    
    if(subpopulations_present=="No"){
      #system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_medicines_L3.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_medicines_L3.html"))) 
      #system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_pregnancy_L3.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_pregnancy_L3.html"))) 
      #system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_vaccines_L3.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_vaccines_L3.html"))) 
      system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_medicines_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_medicines_L3.html"))) 
      system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_pregnancy_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_pregnancy_L3.html"))) 
      system.time(render(paste0(pre_dir,"Report_12_POI_Diagnoses_primary_vaccines_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Diagnoses_primary_vaccines_L3.html"))) 
      
      #system.time(render(paste0(pre_dir,"Report_12_POI_Pregnancy_primary_medicines_L3.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Pregnancy_primary_medicines_L3.html"))) 
      #system.time(render(paste0(pre_dir,"Report_12_POI_Pregnancy_primary_vaccines_L3.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Pregnancy_primary_vaccines_L3.html"))) 
      system.time(render(paste0(pre_dir,"Report_12_POI_Pregnancy_primary_medicines_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Pregnancy_primary_medicines_L3.html"))) 
      system.time(render(paste0(pre_dir,"Report_12_POI_Pregnancy_primary_vaccines_L3_GDPR.Rmd"), output_dir = paste0(output_dir,"POI/Reports/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_","POI_Pregnancy_primary_vaccines_L3.html"))) 
      
    } else {
      for (a in 1: length(subpopulations_names)){
        system.time(render(paste0(pre_dir,"/Report_12_POI_Diagnoses_primary_L3.Rmd"), output_dir = paste0(output_dir,"POI/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[a],"_POI_Diagnoses_primary_L3.html")))  
      }
      for (a in 1: length(subpopulations_names)){
        system.time(render(paste0(pre_dir,"/Report_12_POI_Pregnancy_primary_L3.Rmd"), output_dir = paste0(output_dir,"POI/"), output_file = paste0(format(Sys.Date(), "%Y"),format(Sys.Date(), "%m"),format(Sys.Date(), "%d"),"_",data_access_provider_name,"_", subpopulations_names[a],"_POI_Pregnancy_primary_L3.html")))  
      }
    }
    
  }
}
source(paste0(pre_dir,"save_environment.R"))
