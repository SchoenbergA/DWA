### DWA function ####
# function to check and handle DWA transcripts

# load packages
require(stringi)
require(stringr)
require(openxlsx) # excel handling

### DWA stats ####
DWA_stats <- function(write_data=F){
  ### merge
  # get all raws with places found
  sub_al <- al[which(!is.na(al$`Bearbeiten/in`)),]
  sub_jh <- jh[which(!is.na(jh$`Bearbeiten/in`)),]
  sub_fs <- fs[which(!is.na(fs$`Bearbeiten/in`)),]
  sub_jr <- jr[which(!is.na(jr$`Bearbeiten/in`)),]
  
  cat(paste0("AL ",round(nrow(sub_al)/nrow(al),digits = 4)*100,"% - ",nrow(sub_al)," in total"),sep = "\n")
  cat(paste0("JH ",round(nrow(sub_jh)/nrow(al),digits = 4)*100,"% - ",nrow(sub_jh)," in total"),sep = "\n")
  cat(paste0("FS ",round(nrow(sub_fs)/nrow(al),digits = 4)*100,"% - ",nrow(sub_fs)," in total"),sep = "\n")
  cat(paste0("JR ",round(nrow(sub_jr)/nrow(al),digits = 4)*100,"% - ",nrow(sub_jr)," in total"),sep = "\n")
  cat(" ",sep = "\n")
  
  # rbind
  #d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
  data_full <- do.call("rbind", list(sub_al,sub_jh,sub_fs,sub_jr))
  
  cat(paste0("DWA transliteration ",round(nrow(data_full)/nrow(al),digits = 4)*100,"% - ",nrow(data_full)," in total",sep = "\n"))
  if(write_data==T){
    cat(" Write all Data",sep = "\n")
    write.xlsx(data_full,"C:/Envimaster/DWA/DWA_Aleman_full.xlsx")
  }
  # write out
  # write.xlsx(data_full,file.path(wd,"DWA_places_14_12_22.xlsx"))
}

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

### Check for Whitespaces HIWI ####
WS_stats <- function(){
  # list hiwi data
  ls <- list(al,jh,fs,jr)
  hiwi <- c("AL","JH","FS","JR")
  
  # loop for each hiwi
  for(h in 1:4){
  # check for tailling
  tail <- lapply(1:ncol(ls[[h]]), function(x){
    length(which((grepl(" $", ls[[h]][,x])==T)))
  })
  
  # sum up
  nc_tail <- length(which(tail>0))
  n_tail  <- sum(unlist(tail))

  # check for leading
  lead <- lapply(1:ncol(ls[[h]]), function(x){
    length(which((grepl("^ ", ls[[h]][,x])==T)))
  })
  
  # sum up
  nc_lead <- length(which(lead>0))
  n_lead  <- sum(unlist(lead))

  # save resluts in df
  rows <- c(nc_tail,n_tail,nc_lead,n_lead)
  
  if(h==1){
    df <- rows
  } else {
    df <- rbind(df,rows)
  }
  
  
  } # end outer loop
  
  # get df for output
  row.names(df) <- hiwi
  colnames(df) <- c("col_tail","nt_ws","col_lead","nl_ws")
  df <- as.data.frame(df)
  df$sum <- (df$nt_ws+df$nl_ws)
  print(df)
} # end of function


### Check unsure stats ####
DWA_unsure <- function(){
  
  # get all raws with places found
  sub_al <- al[which(!is.na(al$`Bearbeiten/in`)),]
  sub_jh <- jh[which(!is.na(jh$`Bearbeiten/in`)),]
  sub_fs <- fs[which(!is.na(fs$`Bearbeiten/in`)),]
  sub_jr <- jr[which(!is.na(jr$`Bearbeiten/in`)),]
  
  # list hiwi data
  ls <- list(sub_al,sub_jh,sub_fs,jr)
  hiwi <- c("AL","JH","FS","JR")
  
  # count unsure enrties
  all_unsure <- lapply(1:4, function(i)  
    # n entries with []
    unsure <- lapply(1:ncol(ls[[i]]), function(x){
      length(which((grepl("\\[", ls[[i]][,x])==T)))
    })
  )
  unsure <-c(sum(unlist(all_unsure[[1]])),
             sum(unlist(all_unsure[[2]])),
             sum(unlist(all_unsure[[3]])),
             sum(unlist(all_unsure[[4]])))
  
  # count "?" enrties
  qm <- lapply(1:4, function(i)  
    # n entries with []
    qm1 <- lapply(1:ncol(ls[[i]]), function(x){
      length(which((grepl("\\?", ls[[i]][,x])==T)))
    })
  )
  questionmark <-c(sum(unlist(qm[[1]])),
             sum(unlist(qm[[2]])),
             sum(unlist(qm[[3]])),
             sum(unlist(qm[[4]])))
  
  # get total entries
  n_entries <-c(length(which(!is.na(sub_al[,c(19:56)]))),
                length(which(!is.na(sub_jh[,c(19:56)]))),
                length(which(!is.na(sub_fs[,c(19:56)]))),
                length(which(!is.na(sub_jr[,c(19:56)]))) )
  
  # get df
  df <- as.data.frame(cbind(unsure,n_entries,questionmark))
  
  # calc unsure rate
  df$unsure_rate <- round(df$unsure/df$n_entries,digits = 4)*100
  rownames(df) <- hiwi
  df
}
