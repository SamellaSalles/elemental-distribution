pasta <- paste(getwd(),"/dats01_5",sep="")
filenames <- list.files(pasta, pattern="*.dat", full.names=TRUE)
dados_1 <- lapply(filenames, read.csv, sep="")
names(dados_1) <- sapply(filenames,function(f) substr(f, nchar(pasta)+2,nchar(f)-4))

prep_dats01 <- function(dados_1){
  
  require(tidyverse)
  
  for(pos in 1:length(dados_1)){
    name_arq <- names(dados_1)[pos]
    
    d <- dados_1[[pos]]
    
    d <- d %>%
    mutate(grupo = substr(name_arq,1,1), 
           tecido = substr(name_arq,4,4), 
           semana = 5,
           amostra = substr(name_arq,2,3),
           pontos = substr(name_arq,5,nchar(name_arq))) %>%
    select(grupo,tecido,semana,amostra,pontos,row,
           which(substr(names(d),1,2)!="s.")) %>% 
    select(-c(column, Cl.K, Ar.K, chisq)) %>%
    rename_all(stringr::str_replace, ".K", ".K.ppm")
  
    dados_1[[pos]] <- d
  }
  return(do.call(rbind,dados_1))
}

dados_1 <- prep_dats01(dados_1)

write.csv(dados_1,"dados01.csv",row.names = FALSE)