#Creation of output folders

####Study source populations####
if (subpopulations_present=="No"){
  #output folder for STUDY_SOURCE_POPULATION report in g_output
  if ("STUDY_SOURCE_POPULATION" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    dir.create(paste(std_source_pop_dir,"Masked", sep=""))
    
    #Create the time log folder
    dir.create(paste0(output_dir, "STUDY_SOURCE_POPULATION/Time_log"))
    
  } else {
    #Create the  folder in the output dir
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    dir.create(paste(std_source_pop_dir,"Masked", sep=""))
    #Create the time log folder
    dir.create(paste0(output_dir, "STUDY_SOURCE_POPULATION/Time_log"))
  }
  
  
  #STUDY_SOURCE_POPULATION_tmp/STUDY_SOURCE_POPULATION folder where all intermediary files are saved
  if ("STUDY_SOURCE_POPULATION" %in% list.files(tmp)){
    unlink(paste0(tmp,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }else{
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }
  
  #output folder for STUDY_SOURCE_POPULATION report in g_intermediate/populations
  if (length(list.files(populations_dir))>0){
    unlink(paste0(g_intermediate, "populations"), recursive = T)#delete folder
    dir.create(paste(g_intermediate, "populations", sep=""))
  } 
  
  #delete environment.R file
  if(length(list.files(g_intermediate,"environment"))>0){
    unlink(paste0(g_intermediate,list.files(g_intermediate,"environment")))
  }
  
  
} else {
  #output folder for MEDICINES report in g_output
  if ("STUDY_SOURCE_POPULATION" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    
    
    do.call(file.remove, list(list.files(std_source_pop_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      #Create the time log folder
      dir.create(paste0(output_dir, "STUDY_SOURCE_POPULATION/", subpopulations_names[i], "/Time_log"))
    }
    
  } else {
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(output_dir, "STUDY_SOURCE_POPULATION", sep=""))
    std_source_pop_dir<-paste(output_dir, "STUDY_SOURCE_POPULATION/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(std_source_pop_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      #Create the time log folder
      dir.create(paste0(output_dir, "STUDY_SOURCE_POPULATION/", subpopulations_names[i], "/Time_log"))
    }
  }
  
  
  #STUDY_SOURCE_POPULATION_tmp/STUDY_SOURCE_POPULATION folder where all intermediary files are saved
  if ("STUDY_SOURCE_POPULATION" %in% list.files(tmp)){
    unlink(paste0(tmp,"STUDY_SOURCE_POPULATION"), recursive = T)#delete folder
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }else{
    #Create the STUDY_SOURCE_POPULATION folder in the output dir
    dir.create(paste(tmp, "STUDY_SOURCE_POPULATION", sep=""))
    std_pop_tmp<-paste(tmp, "STUDY_SOURCE_POPULATION/", sep="")
  }
  
  #output folder for STUDY_SOURCE_POPULATION report in g_intermediate/populations
  if (length(list.files(populations_dir))>0){
    unlink(paste0(g_intermediate, "populations"), recursive = T)#delete folder
    dir.create(paste(g_intermediate, "populations", sep=""))
  } 
  
  #delete environment.R file
  if(length(list.files(g_intermediate,"environment"))>0){
    unlink(paste0(g_intermediate,list.files(g_intermediate,"environment")))
  }
  
  
}

####MEDICINES####
if (subpopulations_present=="No"){
  #output folder for MEDICINES report in g_output
  if ("MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    dir.create(paste(med_dir,"Masked", sep=""))
    med_less<-paste(med_dir, "Masked/", sep="")
    dir.create(paste(med_dir,"Time_log", sep=""))
    
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    dir.create(paste(med_dir,"Masked", sep=""))
    med_less<-paste(med_dir, "Masked/", sep="")
    dir.create(paste(med_dir,"Time_log", sep=""))
  }
  
  #output medicines_study_population in g_intermediate/populations/medicines
  if ("MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
  }
  
  #MEDICINES_tmp/MEDICINES folder where all intermediary files are saved
  if ("MEDICINES" %in% list.files(tmp)){
    unlink(paste0(tmp,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }else{
    #Create the MEDICINES folder in the output dir
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }
  
} else {
  #output folder for MEDICINES report in g_output
  if ("MEDICINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    
    do.call(file.remove, list(list.files(med_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Time_log"))
    }
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(output_dir, "MEDICINES", sep=""))
    med_dir<-paste(output_dir, "MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_dir, subpopulations_names[i],"/Time_log"))
    }
  }
  
  #output medicines_study_population in g_intermediate/populations/diagnoses
  if ("MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/",sep="")
    do.call(file.remove, list(list.files(medicines_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(medicines_pop, subpopulations_names[i]))
    }
  } else {
    #Create the MEDICINES folder in the output dir
    dir.create(paste(populations_dir, "MEDICINES", sep=""))
    medicines_pop<-paste(populations_dir, "MEDICINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(medicines_pop, subpopulations_names[i]))
    }
  }
  
  #MEDICINES_tmp/MEDICINES folder where all intermediary files are saved
  if ("MEDICINES" %in% list.files(tmp)){
    unlink(paste0(tmp,"MEDICINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }else{
    #Create the MEDICINES folder in the output dir
    dir.create(paste(tmp, "MEDICINES", sep=""))
    medicines_tmp<-paste(tmp, "MEDICINES/", sep="")
  }
  
}



####VACCINES####
if (subpopulations_present=="No"){
  #output folder for VACCINES report in g_output
  if ("VACCINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "VACCINES", sep=""))
    vacc_dir<-paste(output_dir, "VACCINES/", sep="")
    dir.create(paste(vacc_dir,"Masked", sep=""))
    vacc_less<-paste(vacc_dir, "Masked/", sep="")
    dir.create(paste(vacc_dir,"Time_log", sep=""))
  } else {
    #Create the VACCINES folder in the output dir
    dir.create(paste(output_dir, "VACCINES", sep=""))
    vacc_dir<-paste(output_dir, "VACCINES/", sep="")
    dir.create(paste(vacc_dir,"Masked", sep=""))
    vacc_less<-paste(vacc_dir, "Masked/", sep="")
    dir.create(paste(vacc_dir,"Time_log", sep=""))
  }
  
  #output VACCINES_study_population in g_intermediate/populations/VACCINES
  if ("VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "VACCINES", sep=""))
    vaccines_pop<-paste(populations_dir, "VACCINES/", sep="")
  } else {
    #Create the VACCINES folder in the output dir
    dir.create(paste(populations_dir, "VACCINES", sep=""))
    vaccines_pop<-paste(populations_dir, "VACCINES/", sep="")
  }
  
  #vaccines_tmp/VACCINES folder where all intermediary files are saved
  if ("VACCINES" %in% list.files(tmp)){
    unlink(paste0(tmp,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "VACCINES", sep=""))
    vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
  }else{
    #Create the VACCINES folder in the output dir
    dir.create(paste(tmp, "VACCINES", sep=""))
    vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
  }
  
} else {
  #output folder for VACCINES report in g_output
  if ("VACCINES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "VACCINES", sep=""))
    vacc_dir<-paste(output_dir, "VACCINES/", sep="")
    
    do.call(file.remove, list(list.files(vacc_dir, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i],"/Time_log"))
    }
  } else {
    #Create the VACCINES folder in the output dir
    dir.create(paste(output_dir, "VACCINES", sep=""))
    vacc_dir<-paste(output_dir, "VACCINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_dir, subpopulations_names[i],"/Time_log"))
    }
  }
  
  #output VACCINES_study_population in g_intermediate/populations/diagnoses
  if ("VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "VACCINES", sep=""))
    vaccines_pop<-paste(populations_dir, "VACCINES/",sep="")
    do.call(file.remove, list(list.files(vaccines_pop, full.names = T)))
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vaccines_pop, subpopulations_names[i]))
    }
  } else {
    #Create the VACCINES folder in the output dir
    dir.create(paste(populations_dir, "VACCINES", sep=""))
    vaccines_pop<-paste(populations_dir, "VACCINES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vaccines_pop, subpopulations_names[i]))
    }
  }
  
  #vaccines_tmp/VACCINES folder where all intermediary files are saved
  if ("VACCINES" %in% list.files(tmp)){
    unlink(paste0(tmp,"VACCINES"), recursive = T)#delete folder
    dir.create(paste(tmp, "VACCINES", sep=""))
    vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
  }else{
    #Create the VACCINES folder in the output dir
    dir.create(paste(tmp, "VACCINES", sep=""))
    vaccines_tmp<-paste(tmp, "VACCINES/", sep="")
  }
  
}

####DIAGNOSES####
if (subpopulations_present=="No"){
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
    dir.create(paste(diag_dir,"Time_log", sep=""))
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
    dir.create(paste(diag_dir,"Time_log", sep=""))
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
  #DIAGNOSES_tmp/DIAGNOSES folder where all intermediary files are saved
  if ("DIAGNOSES" %in% list.files(tmp)){
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
    do.call(file.remove, list(list.files(diag_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(tmp, "DIAGNOSES", sep=""))
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
  }
  
} else {
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Time_log"))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Time_log"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/",sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
  #DIAGNOSES_tmp/DIAGNOSES folder where all intermediary files are saved
  if ("DIAGNOSES" %in% list.files(tmp)){
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
    do.call(file.remove, list(list.files(diag_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(tmp, "DIAGNOSES", sep=""))
    diag_tmp<-paste(tmp, "DIAGNOSES/", sep="")
  }
  
}

####PREGNANCY####
if (subpopulations_present=="No"){
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
    dir.create(paste(preg_dir,"Time_log", sep=""))
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
    dir.create(paste(preg_dir,"Time_log", sep=""))
  }
  
  #output diagnoses_study_population in g_intermediate/populations/pregnancy
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  }
  
  #PREGNANCY_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EVENTS folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
  #SI_tmp/SI folder where all intermediary files are saved
  if ("PREGNANCY_SI" %in% list.files(tmp)){
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_SI", sep=""))
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
  }
  
  #PREG_tmp/PREG folder where all intermediary files are saved
  if ("PREG" %in% list.files(tmp)){
    preg_tmp<-paste(tmp, "PREG/", sep="")
    do.call(file.remove, list(list.files(preg_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREG", sep=""))
    preg_tmp<-paste(tmp, "PREG/", sep="")
  }
  
} else {
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Time_log"))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Time_log"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/",sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  }
  
  #PREGNANCY_EV_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EV folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_M folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #PREGNANCY_S_tmp/PREGNANCY_S folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_S folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
  
  #SI_tmp/SI folder where all intermediary files are saved
  if ("PREGNANCY_SI" %in% list.files(tmp)){
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_SI", sep=""))
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
  }
  
  #PREG_tmp/PREG folder where all intermediary files are saved
  if ("PREG" %in% list.files(tmp)){
    preg_tmp<-paste(tmp, "PREG/", sep="")
    do.call(file.remove, list(list.files(preg_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREG", sep=""))
    preg_tmp<-paste(tmp, "PREG/", sep="")
  }
}
####POI####
if (subpopulations_present=="No"){
  
  if ("POI" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"POI"), recursive = T)#delete folder
    dir.create(paste0(output_dir, "POI"))
    poi_dir<-paste0(output_dir, "POI/")
    
    #EVENTS_MEDICINES
    dir.create(paste0(poi_dir, "EVENTS_MEDICINES"))
    ev_med_dir<-paste0(poi_dir, "EVENTS_MEDICINES/")
    dir.create(paste0(ev_med_dir,"Masked"))
    
    #EVENTS_VACCINES
    dir.create(paste0(poi_dir, "EVENTS_VACCINES"))
    ev_vacc_dir<-paste0(poi_dir, "EVENTS_VACCINES/")
    dir.create(paste0(ev_vacc_dir,"Masked"))
    
    #EVENTS_PREGNANCY
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY"))
    ev_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY/")
    dir.create(paste0(ev_preg_dir,"Masked"))
    
    #PREGNANCY_MEDICINES
    dir.create(paste0(poi_dir, "PREGNANCY_MEDICINES"))
    med_preg_dir<-paste0(poi_dir, "PREGNANCY_MEDICINES/")
    dir.create(paste0(med_preg_dir,"Masked"))
    
    #PREGNANCY_VACCINES
    dir.create(paste0(poi_dir, "PREGNANCY_VACCINES"))
    vacc_preg_dir<-paste0(poi_dir, "PREGNANCY_VACCINES/")
    dir.create(paste0(vacc_preg_dir,"Masked"))
    
    
    #Time_log
    dir.create(paste0(poi_dir, "Time_log"))
    #reports
    dir.create(paste0(poi_dir, "Reports"))
    #GDPR
    dir.create(paste0(poi_dir, "GDPR"))
    
    #to be added later
    # #EVENTS_PREGNANCY_MEDICINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES" ))
    # ev_med_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES/" )
    # dir.create(paste0(ev_med_preg_dir,"Masked" ))
    # 
    # #EVENTS_PREGNANCY_VACCINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES" ))
    # ev_vacc_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES/" )
    # dir.create(paste0(ev_vacc_preg_dir,"Masked" ))
    
  } else {
    #Create the  folder in the output dir
    dir.create(paste0(output_dir, "POI"))
    poi_dir<-paste0(output_dir, "POI/")
    
    #EVENTS_MEDICINES
    dir.create(paste0(poi_dir, "EVENTS_MEDICINES"))
    ev_med_dir<-paste0(poi_dir, "EVENTS_MEDICINES/")
    dir.create(paste0(ev_med_dir,"Masked"))
    
    #EVENTS_VACCINES
    dir.create(paste0(poi_dir, "EVENTS_VACCINES"))
    ev_vacc_dir<-paste0(poi_dir, "EVENTS_VACCINES/")
    dir.create(paste0(ev_vacc_dir,"Masked"))
    
    #EVENTS_PREGNANCY
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY"))
    ev_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY/")
    dir.create(paste0(ev_preg_dir,"Masked"))
    
    #PREGNANCY_MEDICINES
    dir.create(paste0(poi_dir, "PREGNANCY_MEDICINES"))
    med_preg_dir<-paste0(poi_dir, "PREGNANCY_MEDICINES/")
    dir.create(paste0(med_preg_dir,"Masked"))
    
    #PREGNANCY_VACCINES
    dir.create(paste0(poi_dir, "PREGNANCY_VACCINES"))
    vacc_preg_dir<-paste0(poi_dir, "PREGNANCY_VACCINES/")
    dir.create(paste0(vacc_preg_dir,"Masked"))
    
    #Time_log
    dir.create(paste0(poi_dir, "Time_log"))
    #Reports
    dir.create(paste0(poi_dir, "Reports"))
    #GDPR
    dir.create(paste0(poi_dir, "GDPR"))
    
    # #EVENTS_PREGNANCY_MEDICINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES" ))
    # ev_med_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES/" )
    # dir.create(paste0(ev_med_preg_dir,"Masked" ))
    # 
    # #EVENTS_PREGNANCY_VACCINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES" ))
    # ev_vacc_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES/" )
    # dir.create(paste0(ev_vacc_preg_dir,"Masked" ))
  }
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    poi_tmp<-paste0(tmp, "POI/")
    do.call(file.remove, list(list.files(poi_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste0(tmp, "POI"))
    poi_tmp<-paste0(tmp, "POI/")
  }
  
  ####To be added later####
  #output events_pregnancy_study_population in g_intermediate/populations/EVENTS_PREGNANCY
  if ("EVENTS_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "EVENTS_PREGNANCY"))
    ev_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY/")
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste0(populations_dir, "EVENTS_PREGNANCY" ))
    ev_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY/" )
  }
  
  #output pregnancy_medicines_study_population in g_intermediate/populations/PREGNANCY_MEDICINES
  if ("PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY_MEDICINES"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
    preg_med_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
  } else {
    #Create the PREGNANCY_MEDICINES folder in the output dir
    dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
    preg_med_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
  }
  
  #output pregnancy_vaccines_study_population in g_intermediate/populations/PREGNANCY_MEDICINES
  if ("PREGNANCY_VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY_VACCINES"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
    preg_vacc_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
  } else {
    #Create the PREGNANCY_VACCINES folder in the output dir
    dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
    preg_vacc_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
  }
  
  
  #####Not needed####
  # #output med_preg_dir_study_population in g_intermediate/populations/med_preg_dir
  # if ("PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"PREGNANCY_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
  #   med_preg_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
  # } else {
  #   #Create the EVENTS_PREGNANCY folder in the output dir
  #   dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
  #   med_preg_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
  # }
  # 
  # #output vacc_preg_pop_study_population in g_intermediate/populations/vacc_preg_pop
  # if ("PREGNANCY_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"PREGNANCY_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
  #   vacc_preg_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
  # } else {
  #   #Create the EVENTS_PREGNANCY folder in the output dir
  #   dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
  #   vacc_preg_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
  # }
  # 
  # if ("EVENTS_PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_PREGNANCY_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES" ))
  #   ev_med_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES/" )
  # } else {
  #   #Create the  folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES" ))
  #   ev_med_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES/" )
  # }
  # 
  # if ("EVENTS_PREGNANCY_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_PREGNANCY_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES" ))
  #   ev_vacc_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES/" )
  # } else {
  #   #Create the  folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES" ))
  #   ev_vacc_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES/" )
  # }
  # 
  # if ("EVENTS_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_MEDICINES" ))
  #   ev_med_pop<-paste0(populations_dir, "EVENTS_MEDICINES/" )
  # } else {
  #   #Create the  folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_MEDICINES" ))
  #   ev_med_pop<-paste0(populations_dir, "EVENTS_MEDICINES/" )
  # }
  # 
  # if ("EVENTS_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_VACCINES" ))
  #   ev_vacc_pop<-paste0(populations_dir, "EVENTS_VACCINES/" )
  # } else {
  #   #Create the  folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_VACCINES" ))
  #   ev_vacc_pop<-paste0(populations_dir, "EVENTS_VACCINES/" )
  # }
  # 
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    unlink(paste0(tmp,"POI"), recursive = T)#delete folder
    dir.create(paste0(tmp, "POI" ))
    poi_tmp<-paste0(tmp, "POI/" )
  }else{
    #Create the POI folder in the output dir
    dir.create(paste0(tmp, "POI" ))
    poi_tmp<-paste0(tmp, "POI/" )
  }
} else {
  
  if ("POI" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"POI"), recursive = T)#delete folder
    dir.create(paste0(output_dir, "POI" ))
    poi_dir<-paste0(output_dir, "POI/" )
    
    #EVENTS_MEDICINES
    dir.create(paste0(poi_dir, "EVENTS_MEDICINES" ))
    ev_med_dir<-paste0(poi_dir, "EVENTS_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_VACCINES
    dir.create(paste0(poi_dir, "EVENTS_VACCINES" ))
    ev_vacc_dir<-paste0(poi_dir, "EVENTS_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_PREGNANCY
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY" ))
    ev_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #PREGNANCY_MEDICINES
    dir.create(paste0(poi_dir, "PREGNANCY_MEDICINES" ))
    med_preg_dir<-paste0(poi_dir, "PREGNANCY_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #PREGNANCY_VACCINES
    dir.create(paste0(poi_dir, "PREGNANCY_VACCINES" ))
    vacc_preg_dir<-paste0(poi_dir, "PREGNANCY_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #Time_log
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "Time_log", subpopulations_names[i]))
    }
    #Reports
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "Reports", subpopulations_names[i]))
    }
    #GDPR
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "GDPR", subpopulations_names[i]))
    }
    ####To be added later####
    # #EVENTS_PREGNANCY_MEDICINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES" ))
    # ev_med_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES/" )
    # for (i in 1:length(subpopulations_names)){
    #   dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    # }
    # for (i in 1:length(subpopulations_names)){
    #   dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    # }
    # 
    # 
    # #EVENTS_PREGNANCY_VACCINES
    # dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES" ))
    # ev_vacc_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES/" )
    # for (i in 1:length(subpopulations_names)){
    #   dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    # }
    # for (i in 1:length(subpopulations_names)){
    #   dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    # }
    
  } else {
    #Create the  folder in the output dir
    dir.create(paste0(output_dir, "POI" ))
    poi_dir<-paste0(output_dir, "POI/" )
    
    #EVENTS_MEDICINES
    dir.create(paste0(poi_dir, "EVENTS_MEDICINES" ))
    ev_med_dir<-paste0(poi_dir, "EVENTS_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_VACCINES
    dir.create(paste0(poi_dir, "EVENTS_VACCINES" ))
    ev_vacc_dir<-paste0(poi_dir, "EVENTS_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_dir, subpopulations_names[i],"/Masked"))
    }
    
    #EVENTS_PREGNANCY
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY" ))
    ev_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #PREGNANCY_MEDICINES
    dir.create(paste0(poi_dir, "PREGNANCY_MEDICINES" ))
    med_preg_dir<-paste0(poi_dir, "PREGNANCY_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #PREGNANCY_VACCINES
    dir.create(paste0(poi_dir, "PREGNANCY_VACCINES" ))
    vacc_preg_dir<-paste0(poi_dir, "PREGNANCY_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    #Time_log
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "Time_log", subpopulations_names[i]))
    }
    #Reports
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "Reports", subpopulations_names[i]))
    }
    #GDPR
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(poi_dir, "GDPR", subpopulations_names[i]))
    }
    ####To be added later####
    #EVENTS_PREGNANCY_MEDICINES
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES" ))
    ev_med_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_med_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
    
    #EVENTS_PREGNANCY_VACCINES
    dir.create(paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES" ))
    ev_vacc_preg_dir<-paste0(poi_dir, "EVENTS_PREGNANCY_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_vacc_preg_dir, subpopulations_names[i],"/Masked"))
    }
    
  }
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    poi_tmp<-paste0(tmp, "POI/" )
    do.call(file.remove, list(list.files(poi_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste0(tmp, "POI" ))
    poi_tmp<-paste0(tmp, "POI/" )
  }
  
  ####To be added later####
  #output events_pregnancy_study_population in g_intermediate/populations/EVENTS_PREGNANCY
  if ("EVENTS_PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"EVENTS_PREGNANCY"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "EVENTS_PREGNANCY" ))
    ev_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the EVENTS_PREGNANCY folder in the output dir
    dir.create(paste0(populations_dir, "EVENTS_PREGNANCY" ))
    ev_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(ev_preg_pop, subpopulations_names[i]))
    }
    
  }
  
  #output pregnancy_medicines_study_population in g_intermediate/populations/PREGNANCY_MEDICINES
  if ("PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY_MEDICINES"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
    preg_med_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_med_pop, subpopulations_names[i]))
    }
  } else {
    #Create the PREGNANCY_MEDICINES folder in the output dir
    dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
    preg_med_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_med_pop, subpopulations_names[i]))
    }
    
  }
  
  #output pregnancy_vaccines_study_population in g_intermediate/populations/PREGNANCY_MEDICINES
  if ("PREGNANCY_VACCINES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY_VACCINES"), recursive = T)#delete folder
    dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
    preg_vacc_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_vacc_pop, subpopulations_names[i]))
    }
  } else {
    #Create the PREGNANCY_VACCINES folder in the output dir
    dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
    preg_vacc_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_vacc_pop, subpopulations_names[i]))
    }
  }
  
  # if ("PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"PREGNANCY_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
  #   med_preg_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/"  )
  #   do.call(file.remove, list(list.files(med_preg_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(med_preg_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the PREGNANCY_MEDICINES folder in the output dir
  #   dir.create(paste0(populations_dir, "PREGNANCY_MEDICINES" ))
  #   med_preg_pop<-paste0(populations_dir, "PREGNANCY_MEDICINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(med_preg_pop, subpopulations_names[i]))
  #   }
  # }
  #
  # if ("PREGNANCY_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"PREGNANCY_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
  #   vacc_preg_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/"  )
  #   do.call(file.remove, list(list.files(vacc_preg_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(vacc_preg_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the PREGNANCY_VACCINES folder in the output dir
  #   dir.create(paste0(populations_dir, "PREGNANCY_VACCINES" ))
  #   vacc_preg_pop<-paste0(populations_dir, "PREGNANCY_VACCINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(vacc_preg_pop, subpopulations_names[i]))
  #   }
  # }
  #
  # if ("EVENTS_PREGNANCY_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_PREGNANCY_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES" ))
  #   ev_med_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES/"  )
  #   do.call(file.remove, list(list.files(ev_med_preg_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_med_preg_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the EVENTS_PREGNANCY_MEDICINES folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES" ))
  #   ev_med_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_MEDICINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_med_preg_pop, subpopulations_names[i]))
  #   }
  # }
  #
  # if ("EVENTS_PREGNANCY_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_PREGNANCY_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES" ))
  #   ev_vacc_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES/"  )
  #   do.call(file.remove, list(list.files(ev_vacc_preg_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_vacc_preg_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the EVENTS_PREGNANCY_VACCINES folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES" ))
  #   ev_vacc_preg_pop<-paste0(populations_dir, "EVENTS_PREGNANCY_VACCINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_vacc_preg_pop, subpopulations_names[i]))
  #   }
  # }
  #
  # if ("EVENTS_MEDICINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_MEDICINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_MEDICINES" ))
  #   ev_med_pop<-paste0(populations_dir, "EVENTS_MEDICINES/"  )
  #   do.call(file.remove, list(list.files(ev_med_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_med_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the EVENTS_MEDICINES folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_MEDICINES" ))
  #   ev_med_pop<-paste0(populations_dir, "EVENTS_MEDICINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_med_pop, subpopulations_names[i]))
  #   }
  # }
  #
  # if ("EVENTS_VACCINES" %in% list.files(populations_dir)){
  #   unlink(paste0(populations_dir,"EVENTS_VACCINES"), recursive = T)#delete folder
  #   dir.create(paste0(populations_dir, "EVENTS_VACCINES" ))
  #   ev_vacc_pop<-paste0(populations_dir, "EVENTS_VACCINES/"  )
  #   do.call(file.remove, list(list.files(ev_vacc_pop, full.names = T)))
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_vacc_pop, subpopulations_names[i]))
  #   }
  # } else {
  #   #Create the EVENTS_VACCINES folder in the output dir
  #   dir.create(paste0(populations_dir, "EVENTS_VACCINES" ))
  #   ev_vacc_pop<-paste0(populations_dir, "EVENTS_VACCINES/" )
  #   for (i in 1:length(subpopulations_names)){
  #     dir.create(paste0(ev_vacc_pop, subpopulations_names[i]))
  #   }
  # }
  
  
  #POI_tmp/POI folder where all intermediary files are saved
  if ("POI" %in% list.files(tmp)){
    unlink(paste0(tmp,"POI"), recursive = T)#delete folder
    dir.create(paste0(tmp, "POI" ))
    poi_tmp<-paste0(tmp, "POI/" )
  }else{
    #Create the POI folder in the output dir
    dir.create(paste0(tmp, "POI" ))
    poi_tmp<-paste0(tmp, "POI/" )
  }
}