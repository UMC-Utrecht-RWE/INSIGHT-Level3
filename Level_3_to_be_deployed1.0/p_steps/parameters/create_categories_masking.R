
####Create categories####


####Read excel file####
parameter_file_fl<-list.files(paste0(projectFolder,"/p_parameters/"),"study_parameters")
parameter_file<-as.data.table(read_excel(paste0(projectFolder,"/p_parameters/",parameter_file_fl),col_types = "text", sheet = "create_categories"))
#Set parameters basic parameters
records_categories_vector<-as.numeric(c(parameter_file[Indicator=="records_categories",min_value],
                                 parameter_file[Indicator=="records_categories",second_category],
                                 parameter_file[Indicator=="records_categories",max_value],
                                 parameter_file[Indicator=="records_categories",by]))

users_categories_vector<-as.numeric(c(parameter_file[Indicator=="users_categories",min_value],
                               parameter_file[Indicator=="users_categories",second_category],
                               parameter_file[Indicator=="users_categories",max_value],
                               parameter_file[Indicator=="users_categories",by]))

followup_categories_vector<-as.numeric(c(parameter_file[Indicator=="followup_categories",min_value],
                                  parameter_file[Indicator=="followup_categories",second_category],
                                  parameter_file[Indicator=="followup_categories",max_value],
                                  parameter_file[Indicator=="followup_categories",by]))

####records categories####
records_categories<-create_categories(records_categories_vector)
records_categories<-data.table(range=records_categories)
records_categories[, c("min", "max") := tstrsplit(range, ":")]
#remove the + sign from the last row
records_categories[.N,min:=records_categories_vector[3]+1]
records_categories[.N,max:=records_categories_vector[3]*1000000]
records_categories[,min:=as.numeric(min)][,max:=as.numeric(max)]
#Add the 0
records_categories<-rbind(data.table(range=0,min=0,max=0), records_categories)

####users categories####
users_categories<-create_categories(users_categories_vector)
users_categories<-data.table(range=users_categories)
users_categories[, c("min", "max") := tstrsplit(range, ":")]
#remove the + sign from the last row
users_categories[.N,min:=users_categories_vector[3]+1]
users_categories[.N,max:=users_categories_vector[3]*1000000]
users_categories[,min:=as.numeric(min)][,max:=as.numeric(max)]
#Add the 0
users_categories<-rbind(data.table(range=0,min=0,max=0), users_categories)

####followup categories####
followup_categories<-create_categories(followup_categories_vector)
followup_categories<-data.table(range=followup_categories)
followup_categories[, c("min", "max") := tstrsplit(range, ":")]
#remove the + sign from the last row
followup_categories[.N,min:=followup_categories_vector[3]+1]
followup_categories[.N,max:=followup_categories_vector[3]*1000000]
followup_categories[,min:=as.numeric(min)][,max:=as.numeric(max)]
#Add the 0
followup_categories<-rbind(data.table(range=0,min=0,max=0), followup_categories)
