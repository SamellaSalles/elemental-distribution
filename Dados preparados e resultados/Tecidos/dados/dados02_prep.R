pasta <- paste(getwd(),"/dats02_0-5",sep="")
filenames <- list.files(pasta, pattern="*.dat", full.names=TRUE)
dados_2 <- lapply(filenames, read.csv, sep="")
names(dados_2) <- sapply(filenames,function(f) substr(f, nchar(pasta)+2,nchar(f)-4))

prep_dats02 <- function(dados_2){
  require(tidyverse)
  #browser()
  for(pos in 1:length(dados_2)){
    
    name_arq <- names(dados_2)[pos]

    d <- dados_2[[pos]]

    if(any(str_detect(names(d), ".mass.fraction")))
      d <- as.data.frame(cbind(row=d[,1],sapply(d[,which(str_detect(names(d), ".mass.fraction"))],function(v) v*10^6)))
    
    columns <- c("P.K", "S.K", "K.K", "Ca.K", "Mn.K", "Fe.K", "Cu.K", "Zn.K")
    columns.mf <- paste(columns,"mass.fraction",sep=".")
    res <- tryCatch(d %>% 
                      select(c("row",columns.mf)),
                    error=function(c) NULL)
    if(is.null(res)){
      res <- tryCatch(d %>% 
                        select(c("row",columns)),
                      error=function(c) NULL)
    }
    d <- res
    
    d <- d  %>%
      mutate(grupo = substr(name_arq,2,2), 
             tecido = substr(name_arq,3,3), 
             semana = substr(name_arq,1,1),
             amostra = substr(name_arq,4,4),
             pontos = substr(name_arq,5,nchar(name_arq))) %>%
      select(grupo,tecido,semana,amostra,pontos,row,
             which(str_detect(names(d), ".K"))) %>% 
      rename_all(stringr::str_replace, ".K", ".K.ppm") %>%
      rename_all(stringr::str_replace, ".mass.fraction", "")
    
    dados_2[[pos]] <- d
  }
  return(do.call(rbind,dados_2))
}

dados_2 <- prep_dats02(dados_2)

write.csv(dados_2,"dados02.csv",row.names = FALSE)