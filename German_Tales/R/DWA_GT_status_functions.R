### DWA Status function ####
# function to get progress and check data

# load packages
require(stringi)
require(stringr)
require(openxlsx) # excel handling

### DWA stats ####
Check_progress <- function(path,bnd,dup_check=T){
  
  # fork specific band or full data
  if(bnd=="all"){
    cat("Status German Tales ",sep = "\n")
    cat(" ",sep = "\n")
    # load data
    al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_AL.xlsx"))
    fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_FS.xlsx"))
    jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_JR.xlsx"))
    
    ### merge
    # get all raws with places found
    sub_al <- al[which(!is.na(al$Bearbeiter_Lvl2)),]
    sub_fs <- fs[which(!is.na(fs$Bearbeiter_Lvl2)),]
    sub_jr <- jr[which(!is.na(jr$Bearbeiter_Lvl2)),]
    
    # get target rows
    trg <-nrow(al)
    
    cat(paste0("AL ",round(nrow(sub_al)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
    cat(paste0("FS ",round(nrow(sub_fs)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
    cat(paste0("JR ",round(nrow(sub_jr)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
    cat(" ",sep = "\n")
    
    # rbind
    #d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
    data_full <- do.call("rbind", list(sub_al,sub_fs,sub_jr))
    
    if(dup_check==T){
      # check for duplicates
      any(duplicated(data_full$ID)==T)
      
      if(nrow(data_full[which(duplicated(data_full$ID)==T),])>0){
        cat(paste0(nrow(data_full[which(duplicated(data_full$ID)==T),])," duplicates in 'ID' detected!"),sep="\n")
      } else {
        cat("No duplicates detected in 'ID'",sep="\n")
      }
    }
    cat(paste0("German Tales progress ",round(nrow(data_full)/trg,digits = 4)*100,"% - ",nrow(data_full)," / ", trg,sep = "\n"))
    cat(paste0(trg-nrow(data_full)," rows missing"),sep= "\n")
    
  } else {
  
  cat(paste0("Status for Band ",bnd),sep = "\n")
  cat(" ",sep = "\n")
  # load data
  al <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_AL.xlsx"))
  fs <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_FS.xlsx"))
  jr <- openxlsx::read.xlsx(xlsxFile =file.path(path,"German_tales_JR.xlsx"))
  
  ### merge
  # get all raws with places found
  sub_al <- al[which(!is.na(al$Bearbeiter_Lvl2)&al$Bd==bnd),]
  sub_fs <- fs[which(!is.na(fs$Bearbeiter_Lvl2)&fs$Bd==bnd),]
  sub_jr <- jr[which(!is.na(jr$Bearbeiter_Lvl2)&jr$Bd==bnd),]
  
  # get target rows
  trg <-length(which(al$Bd==bnd))
  
  cat(paste0("AL ",round(nrow(sub_al)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(paste0("FS ",round(nrow(sub_fs)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(paste0("JR ",round(nrow(sub_jr)/trg,digits = 4)*100,"% - ",trg," in total"),sep = "\n")
  cat(" ",sep = "\n")
  
  # rbind
  #d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
  data_full <- do.call("rbind", list(sub_al,sub_fs,sub_jr))
  
  if(dup_check==T){
    # check for duplicates
    any(duplicated(data_full$ID)==T)
    
    if(nrow(data_full[which(duplicated(data_full$ID)==T),])>0){
      cat(paste0(nrow(data_full[which(duplicated(data_full$ID)==T),])," duplicates in 'ID' detected!"),sep="\n")
    } else {
      cat("No duplicates detected in 'ID'",sep="\n")
    }
  }
  cat(paste0("German Tales Band ",bnd," progress ",round(nrow(data_full)/trg,digits = 4)*100,"% - ",nrow(data_full)," / ", trg,sep = "\n"))
  cat(paste0(trg-nrow(data_full)," rows missing"),sep= "\n")
  
  }
  return(data_full)
  
}


### Check Ortsname and Coordinates ####

Check_df <- function(gt_full, classes){
  
  # clean all brackets
  cat(paste0("cleaning all '{[ ]}' brackets"),sep="\n")
  cat(" ",sep="\n")
  for (i in 1:nrow(gt_full)){
    gt_full$Ort_Klasse[i]<-gsub("\\[|\\]", "", gt_full$Ort_Klasse[i])
    gt_full$Ort_Klasse[i]<-str_trim(gsub("\\{[^}]*}", "", gt_full$Ort_Klasse[i]),"both")
  }
  
  if(any(is.na(gt_full$Ort_Klasse))){
    cat(paste0("NA in 'Ortsklasse' detected in IDs:"),sep="\n")
    print(gt_full$ID[which(is.na(gt_full$Ort_Klasse))])
    cat(" ",sep="\n")
  } 
  
  
  # check for invalid "Ortsklasse"
  which(unique(gt_full$Ort_Klasse)%in%classes)
  class_issue <-unique(gt_full$Ort_Klasse)[-which(unique(gt_full$Ort_Klasse)%in%c("Region/Gebiet","Ort","ungenau","spezifisch","unklar","Insel"))]
  # filter NAs
  if(any(is.na(class_issue))){
    class_issue <- class_issue[-which(is.na(class_issue))]
  }
  
  if(length(class_issue!=0)){
    for(i in 1:length(class_issue)){
      cat(paste0("Invalid 'Ortsname' '",class_issue[i],"' detected in IDs:"),sep="\n")
      print(gt_full$ID[which(gt_full$Ort_Klasse==class_issue[i])])
      cat(" ",sep="\n")
    }
    cat(paste0("Valid classes for 'Ortsname' are '",paste(classes,collapse = "', '"),"'"),sep="\n")
    cat(" ",sep="\n")
  }
  
  
  
  #  check for NA in Coordinates
  gt_full$x <- as.numeric(gt_full$x )
  gt_full$y <- as.numeric(gt_full$y )
  
  if(any(is.na(gt_full$x))){
    cat(paste0("NA in 'x' Coordinates detected in IDs:"),sep="\n")
    cat(" ",sep="\n")
    print(gt_full$ID[which(is.na(gt_full$x))])
  } else {
    cat(paste0(" 'x' Coordinates are valid"),sep="\n")
    cat(" ",sep="\n")
  }
  
  if(any(is.na(gt_full$y))){
    cat(paste0("NA in 'y' Coordinates detected in IDs:"),sep="\n")
    cat(" ",sep="\n")
    print(gt_full$ID[which(is.na(gt_full$y))])
  } else {
    cat(paste0(" 'y' Coordinates are valid"),sep="\n")
    cat(" ",sep="\n")
  }
  
}