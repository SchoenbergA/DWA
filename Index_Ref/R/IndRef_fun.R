### IndexRef Functions

### IndexRef status ####
IndRef_stats <- function(){

  cat(paste0("AL ",round(nrow(al)/783,digits = 4)*100," % - ",nrow(al)," in total"),sep = "\n")
  cat(paste0("JH ",round(nrow(jh)/783,digits = 4)*100," % - ",nrow(jh)," in total"),sep = "\n")
  cat(paste0("FS ",round(nrow(fs)/783,digits = 4)*100," % - ",nrow(fs)," in total"),sep = "\n")
  cat(paste0("JR ",round(nrow(jr)/783,digits = 4)*100," % - ",nrow(jr)," in total"),sep = "\n")
  cat(" ",sep = "\n")
  
  # rbind
  #d1ata_full <- rbind(sub_al,sub_jh,sub_fs)
  rows <- nrow(al)+
  nrow(jh)+
  nrow(fs)+
  nrow(jr)
    
  
  cat(paste0("IndexRef ",round(rows/783,digits = 4)*100," % - ",rows," in total",sep = "\n"))
  
}



