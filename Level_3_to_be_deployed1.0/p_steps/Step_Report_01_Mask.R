
#i="PC"

for(i in readRDS(paste0(std_pop_tmp,"SCHEME_06.rds"))[["subpopulations"]]){

if(SUBP) {
  report_dir1 <- paste0(std_source_pop_dir,i)
  report_dir2 <- paste0(std_source_pop_dir,i,"/Masked")
  
  }else{
  report_dir1 <- substr(std_source_pop_dir,1,nchar(std_source_pop_dir)-1)
  report_dir2 <- paste0(std_source_pop_dir,"Masked")
}


list1 <- c("No. source","PY source","No. study","PY study","Total No","No Female","No Male","PY Male","PY Female","Total","No","No. of visits","No. of persons with at least one visit","Women with at least 1 record in study period","Woman in category",as.character(c(1:12)),as.character(c(1900:2200)))
list2 <- c("% Male","% Female","%","Visit rate, No. of visits/1000 PY","PY")

vlist <- rep(5,length(list1)) 
names(vlist) <- list1

do.call(file.remove, list(list.files(report_dir2, full.names = TRUE)))
DRE_Treshold(
  Inputfolder = report_dir1,
  Outputfolder = report_dir2,
  Delimiter = ";",
  Varlist = vlist,
  NAlist = list2
)

c("No. source","PY source","No. study","PY study")

}

rm(report_dir1,report_dir2,list1,list2)

#Copy all files from the masked folder into GDPR folder
files_in_masked<-list.files(paste0(std_source_pop_dir,"Masked/"))
#rename
for(i in 1:length(files_in_masked)){
  file.rename(paste0(std_source_pop_dir,"Masked/",files_in_masked[i]), paste0(std_source_pop_dir,
                                                                              "Masked/",
                                                                format(Sys.Date(), "%Y"),
                                                                format(Sys.Date(), "%m"),
                                                                format(Sys.Date(), "%d"),"_",
                                                                data_access_provider_name,"_",
                                                                files_in_masked[i]))
}

#Copy to gdpr
files_in_masked<-list.files(paste0(std_source_pop_dir,"Masked/"))
for(i in 1:length(files_in_masked)){
file.copy(paste0(std_source_pop_dir,"Masked/", files_in_masked[i]), paste0(std_source_pop_dir,"GDPR/"))
}

#Rename all files by adding the date and name of the data source in the beginning

#main output folder
output_file<-list.files(std_source_pop_dir, ".csv")
for(i in 1:length(output_file)){
  file.rename(paste0(std_source_pop_dir,output_file[i]), paste0(std_source_pop_dir,
                                                                format(Sys.Date(), "%Y"),
                                                                format(Sys.Date(), "%m"),
                                                                format(Sys.Date(), "%d"),"_",
                                                                data_access_provider_name,"_",
                                                                output_file[i]))
}


