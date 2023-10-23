#Create folders function

create_folder<-function(path_fl, location, name_folder){
  if(name_folder %in% list.files(paste0(path_fl,"/",location))){
  #check if folder is present, delete and then create it
  unlink(paste0(path_fl,"/",location, name_folder,"/"), recursive = T)#delete folder
    #create folder
    dir.create(paste0(path_fl, "/",location, name_folder))
    
  }else{
    #create folder
  dir.create(paste0(path_fl, "/",location, name_folder))
  }
}
