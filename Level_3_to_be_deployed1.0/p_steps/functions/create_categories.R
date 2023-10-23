

#A function to create needed categories to mask counts

#first value: the minimum category value
#second value the second category value, used to accomodate small first interval
#third value: the maximum category value after which all other value will be classified together with the + sign
#fourth value: by option, the increase category element
create_categories<-function(value_vector){
  x<-value_vector
  size<-x[4]
  groups<-round(x[3]/size)
  index<-1
  min<-x[2]
  max<-x[3]
  chunks<-rep(NA, groups)
  for (size_ind in 1: groups){
    if(index<= groups){
      if(index==1){
        chunks[index]<-paste0(min+1,":", format(size_ind*size, scientific = F))
      }else{
        chunks[index]<-paste0(min,":", format(size_ind*size, scientific = F))
      }
      min<-size_ind*size+1
      index<-index+1
    }
  }
  max_cat<-paste0(max+1,"+")
  chunks<-c(paste0(x[1],":", x[2]), chunks, max_cat)
  return(chunks)
}
#categories_available<-create_categories(value_vector = values)
