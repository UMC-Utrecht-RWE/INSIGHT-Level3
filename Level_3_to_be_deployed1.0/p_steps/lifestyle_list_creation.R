#Lifestyle factors
####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "lifestyle_factors"))

#Create lists of list
Lifestyle<-list()

if(parameter_file[,.N]>0){
#create as many sublists as rows in the file
for(i in 1:parameter_file[,.N]){
     list<-list(CDM_table=parameter_file[i,CDM_table],
             CDM_column=parameter_file[i,CDM_column],
             value=c(unlist(str_split(parameter_file[i,value],","))),
             c.voc=parameter_file[i,c.voc],
             v.voc=parameter_file[i,v.voc],
             v.date=parameter_file[i,v.date]
             )
  #names(list)<-parameter_file[i,Variable_name]
  Lifestyle[[i]]<-list 
  names(Lifestyle)[i]<-parameter_file[i,Variable_name]
}
}
