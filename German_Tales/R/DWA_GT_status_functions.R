### DWA Status function ####
# function to get progress and check data

# load packages
require(stringi)
require(stringr)
require(openxlsx) # excel handling

### DWA stats ####
DWA_GT_stats <- function(path,bnd,dup_check=T){
  # load data
  al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_AL.xlsx"))
  jh <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_JH.xlsx"))
  fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_FS.xlsx"))
  jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_JR.xlsx"))
  
  ### merge
  # get all raws with places found
  sub_al <- al[which(!is.na(al$Bearbeiter_Lvl2)&al$Bd==bnd),]
  sub_jh <- jh[which(!is.na(jh$Bearbeiter_Lvl2)&jh$Bd==bnd),]
  sub_fs <- fs[which(!is.na(fs$Bearbeiter_Lvl2)&fs$Bd==bnd),]
  sub_jr <- jr[which(!is.na(jr$Bearbeiter_Lvl2)&jr$Bd==bnd),]
  
  # get target rows
  trg <-length(which(al$Bd==bnd))
  
  cat(paste0("AL ",round(nrow(sub_al)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(paste0("JH ",round(nrow(sub_jh)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(paste0("FS ",round(nrow(sub_fs)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(paste0("JR ",round(nrow(sub_jr)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(" ",sep = "\n")
  
  # rbind
  #d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
  data_full <- do.call("rbind", list(sub_al,sub_jh,sub_fs,sub_jr))
  
  if(dup_check==T){
    # check for duplicates
    any(duplicated(data_full$ID)==T)
    
    if(nrow(data_full[which(duplicated(data_full$ID)==T),])>0){
      cat(paste0(nrow(data_full[which(duplicated(data_full$ID)==T),])," duplicates in 'ID' detected!"),sep="\n")
    } else {
      cat("No duplicates detected",sep="\n")
    }
  }
  cat(paste0("DWA transliteration ",round(nrow(data_full)/trg,digits = 4)*100,"% - ",nrow(data_full)," / ", trg,sep = "\n"))
  cat(paste0(trg-nrow(data_full)," rows missing"),sep= "\n")
  return(data_full)
  
}
