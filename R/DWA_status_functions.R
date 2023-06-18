### DWA Status function ####
# function to get progress and check data

# load packages
require(stringi)
require(stringr)
require(openxlsx) # excel handling

### DWA stats ####
DWA_stats <- function(path,dup_check=T){
  # load data
  al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_AL.xlsx"))
  jh <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_JH.xlsx"))
  fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_FS.xlsx"))
  jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_JR.xlsx"))
  
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
  
  if(dup_check==T){
  # check for duplicates
    any(duplicated(data_full$Digi_Index)==T)
    
  if(nrow(data_full[which(duplicated(data_full$Digi_Index)==T),])>0){
    cat(paste0(nrow(data_full[which(duplicated(data_full$Digi_Index)==T),])," duplicates detected!"),sep="\n")
  } else {
    cat("No duplicates detected",sep="\n")
  }
  data_full <- data_full[which(duplicated(data_full$Digi_Index)==F),]
}
  cat(paste0("DWA transliteration ",round(nrow(data_full)/nrow(al),digits = 4)*100,"% - ",nrow(data_full)," / ", nrow(al),sep = "\n"))
  cat(paste0(nrow(al)-nrow(data_full)," rows missing"),sep= "\n")
  return(data_full)

}

### Check for Whitespaces HIWI ####
WS_stats <- function(path){
  # load data
  al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_AL.xlsx"))
  jh <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_JH.xlsx"))
  fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_fs.xlsx"))
  jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_jr.xlsx"))
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
DWA_unsure <- function(path){
  # load data
  al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_AL.xlsx"))
  jh <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_JH.xlsx"))
  fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_fs.xlsx"))
  jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"DWA_full_jr.xlsx"))
  
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

  cat(paste0(sum(df$questionmark)," total '?' entries"),sep="\n")
  cat(paste0(sum(df$unsure)," total '[unsure]' entries"),sep="\n")
  cat(" ",sep="\n")
  print(df)
}

### check NA in table ####
check_NA <- function(df,cols){
  # display amount of NAs per column
  for(i in cols){
    res <- length(which(is.na(df[,i])))
    cat(paste0(res," NA in", colnames(df)[i]),sep="\n")
    if(i==cols[1]){
      sum_res <- res
    } else {
      sum_res <- sum_res + res
    }
    
  }
  cat(paste0("Total NA ",sum_res),sep="\n")
}# end of function

### check unsure[] in table ####
check_unsure <- function(df){
  
  unsure <- lapply(1:ncol(df), function(x){
    length(which((grepl("\\[", df[,x])==T)))
  })
  cat(paste0("Total unsure[] ", sum(unlist(unsure))),sep="\n")
}
