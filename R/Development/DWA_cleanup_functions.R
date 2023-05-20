### Clean up and Check functions

### Handle UniCOde ####
hndl_UCnew2 <- function(data,cmin=1,cmax=ncol(data)){
  
  # set container
  count_ncol <-0
  count_non  <-0
  count_ncolu<-0
  count_nonu <-0
  #loop
  
  for(i in cmin:cmax){
    # sum up
    if(length(which(stri_trans_isnfc(data[,i])==F))>0){
      count_ncol <- count_ncol+1}
    count_non <- count_non+length(which(stri_trans_isnfc(data[,i])==F))
    if(length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))>0){
      count_ncolu <- count_ncolu+1}
    count_nonu <- count_nonu+length(unique(data[,i]))-length(unique(stri_trans_nfc(data[,i])))
    
    # transform all columns
    data[,i] <-stri_trans_nfc(data[,i])
    
  }# end loop
  cat(paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings, leading to a total of ",
             count_nonu, " lesser uniques in ",count_ncolu, " columns."))
  res_uc <-paste0(count_ncol," columns contain a total of ",count_non, " non UFC strings. Transformation to NFC leads to a total of ",
                  count_nonu, " lesser uniques in ",count_ncolu, " columns.")
  
  
  # return
  return(list(data=data,res_uc=res_uc))
}# end function

### Check for Whitespaces ####
anyWS <- function(df,skip_message=F){
  
  # check for tailling
  tail <- lapply(1:ncol(df), function(x){
    length(which((grepl(" $", df[,x])==T)))
  })
  
  # sum up
  cat(paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces"),sep = "\n")
  res_tail <-paste0(length(which(tail>0))," columns contain a total of ",sum(unlist(tail)), " tailling whitespaces")
  
  # check for leading
  lead <- lapply(1:ncol(df), function(x){
    length(which((grepl("^ ", df[,x])==T)))
  })
  
  # sum up
  cat(paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces"),sep = "\n")
  res_lead <-paste0(length(which(lead>0))," columns contain a total of ",sum(unlist(lead)), " leading whitespaces")
  
  # trim both leading amnd tailing whitespaces
  for (i in 1:ncol(df)) {
    df[,i] <-str_trim(df[,i], "both") 
  }
  
  
  
  # return
  return(list(df=df,res_lead=res_lead,res_tail=res_tail))
} # end of function