combine_numeric_sequences<-function(nums){
  ranges<-vector()
  start<-NULL
  end<-NULL
  
  sorted_nums<-sort(nums)
  
  for(num in sorted_nums){
    if(is.null(start)){
      start<- num
      end<- num
    } else if(num==end+1) {
      end<- num 
    } else {
      if (start != end) {
        ranges<-append(ranges, paste0(start, "-", end))
      } else {
        ranges <- append(ranges, as.character(start))
      }
      start<-num
      end<-num
    }
  }
  
  if(!is.null(start)){
    if(start!=end){
      ranges<-append(ranges, paste0(start,"-",end))
    } else {
      ranges<-append(ranges, as.character(start))
    }
  }
  
  return(ranges)
  }