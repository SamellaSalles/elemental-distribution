plot_cada_elemento <- function(agrupamento,classe=NULL,descricao="",p.adjust.method="none",conf=0.95,...){
  stats <- list()
  n_colunas <- ncol(agrupamento)
  nomes_elementos <- names(agrupamento)[(n_colunas-7):n_colunas]
  #browser()
  for(elemento in nomes_elementos){
    
    if(!is.null(classe)){
      dados_elemento <- agrupamento[c(classe,elemento)]
      names(dados_elemento) <- c("variable","value")
    }
    else {
      dados_elemento <- agrupamento[elemento]
      names(dados_elemento) <- "value"
    }
    #browser()
    descricao_grafico <- paste(descricao,elemento,sep="_")
    
    stat <- tryCatch( tapply(dados_elemento$value, dados_elemento$variable, pastecs::stat.desc)
                      ,error = function(c) NULL )
    
    if(length(stat)==0) stat <- NULL
    #else {
    #  stat[stat<=(1-conf)] <- TRUE
    #  stat[stat!=TRUE] <- FALSE
    #}
    
    stats[[descricao_grafico]] <- stat
  }
  return(stats)
}

plot_elementos <- function(agrupamento,descricao="",p.adjust.method="none",conf=0.95,...){
  #browser()
  stats <- list()
  n_colunas <- ncol(agrupamento)
  nomes_elementos <- names(agrupamento)[(n_colunas-7):n_colunas]
  id.vars <- which(!names(agrupamento) %in% nomes_elementos)
  
  require(reshape)
  agrupamento <- reshape::melt(as.data.frame(agrupamento), id.vars = id.vars)
  
  dados_elementos <- agrupamento[c("variable","value")]
  
  descricao_grafico <- paste(descricao,sep="_")
  
  stat <- tryCatch( tapply(dados_elementos$value, dados_elementos$variable, pastecs::stat.desc)
                    ,error = function(c) NULL )
  
  if(length(stat)==0) stat <- NULL
  #else {
  #  stat[stat<=(1-conf)] <- TRUE
  #  stat[stat!=TRUE] <- FALSE
  #}
  
  stats[[descricao_grafico]] <- stat
  
  return(stats)
}


plots_elem <- function(dados,classe=NULL,cada_elem=TRUE,gb=NULL,desc="stats",p.adjust.method="none",conf=0.95,...){
  #browser()
  pars <- c(...)
  if(cada_elem){
    if(!is.null(gb))
      dados %>%
      group_by_(.dots = gb) %>%
      group_map(~plot_cada_elemento(agrupamento=.x,classe=classe,descricao=paste(desc,"_",paste(.y,collapse=""),sep=""),p.adjust.method=p.adjust.method,conf=conf,pars))
    else
      dados %>%
      group_map(~plot_cada_elemento(agrupamento=.x,classe=classe,descricao=desc,p.adjust.method=p.adjust.method,conf=conf,pars))
  } 
  else{
    if(!is.null(gb))
      dados %>%
      group_by_(.dots = gb) %>%
      group_map(~plot_elementos(agrupamento=.x,descricao=paste(desc,"_",paste(.y,collapse=""),sep=""),p.adjust.method=p.adjust.method,conf=conf,pars))
    else
      dados %>%
      group_map(~plot_elementos(agrupamento=.x,descricao=desc,p.adjust.method=p.adjust.method,conf=conf,pars))
  }
}


#overall statistics across all series for each metric: transf X transf (all series must be positive (negative))
#p.adjust.method=c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")


#=========== Dados ===========
dados <- read.csv(paste(getwd(),"/dados/dados0102.csv",sep=""),stringsAsFactors = FALSE)

library("tidyverse")
#Selecionando variáveis:
dados <- dados %>% select(2:4, 8:15)
dados$semana <- as.character(dados$semana)
dados_log <- data.frame(dados[1:3], lapply(dados[4:11], log) )


#=========== Stats ===========
#Dados reais - P.values por GRUPO para cada tecido e elemento:
stats_grupo <- plots_elem(dados,classe="grupo",gb=c("tecido"))
#Dados reais - P.values por SEMANA para cada grupo, tecido e elemento :
stats_semana <- plots_elem(dados,classe="semana",gb=c("grupo","tecido"))
#Dados reais - P.values por TECIDO para cada grupo e elemento :
stats_tecido_G <- plots_elem(dados,classe="tecido",gb=c("grupo"))
#Dados reais - P.values por TECIDO para cada grupo, semana e elemento :
stats_tecido_GS <- plots_elem(dados,classe="tecido",gb=c("grupo","semana"))

#Dados reais - P.values por ELEMENTOS para cada grupo:
stats_elementos_G <- plots_elem(dados,cada_elem=FALSE,gb=c("grupo"))
#Dados reais - P.values por ELEMENTOS para cada grupo e tecido:
stats_elementos_GT <- plots_elem(dados,cada_elem=FALSE,gb=c("grupo","tecido"))
#Dados reais - P.values por ELEMENTOS para cada grupo, tecido e semana:
stats_elementos_GTS <- plots_elem(dados,cada_elem=FALSE,gb=c("grupo","tecido","semana"))



#P.values por GRUPO para cada tecido e elemento:
stats_grupo_log <- plots_elem(dados_log,classe="grupo",gb=c("tecido"))
#P.values por SEMANA para cada grupo, tecido e elemento :
stats_semana_log <- plots_elem(dados_log,classe="semana",gb=c("grupo","tecido"))
#P.values por TECIDO para cada grupo e elemento :
stats_tecido_G_log <- plots_elem(dados_log,classe="tecido",gb=c("grupo"))
#P.values por TECIDO para cada grupo, semana e elemento :
stats_tecido_GS_log <- plots_elem(dados_log,classe="tecido",gb=c("grupo","semana"))

#P.values por ELEMENTOS para cada grupo:
stats_elementos_G_log <- plots_elem(dados_log,cada_elem=FALSE,gb=c("grupo"))
#P.values por ELEMENTOS para cada grupo e tecido:
stats_elementos_GT_log <- plots_elem(dados_log,cada_elem=FALSE,gb=c("grupo","tecido"))
#P.values por ELEMENTOS para cada grupo, tecido e semana:
stats_elementos_GTS_log <- plots_elem(dados_log,cada_elem=FALSE,gb=c("grupo","tecido","semana"))



#======= Tabela =============
tab_stats_elementos <- function(stats,conf=0.95){
  tab <- data.frame()
  #browser()
  for(item in stats){
    agrupamento <- stringr::str_remove(names(item),"stats_")
    
    grupo <- ifelse(nchar(agrupamento)>=1,"fixo","-")
    tecido <- ifelse(nchar(agrupamento)>=2,"fixo","-")
    semana <- ifelse(nchar(agrupamento)>=3,"fixo","-")
    elemento <- "mutavel"
    
    info <- data.frame(grupo=grupo,tecido=tecido,semana=semana,elemento=elemento,agrupamento=agrupamento)
    
    tab_elementos <- item[[1]]
    for(elemento in names(tab_elementos)){
      stats_summ <- data.frame(t(c(tab_elementos[[elemento]])))
      
      tab <- rbind(tab, cbind(info, item=elemento, stats_summ))
    }
  }
  
  return(tab)
}

tab_elementos <- rbind(cbind(dado="stats_elementos_G",tab_stats_elementos(stats_elementos_G)),
                       cbind(dado="stats_elementos_GT",tab_stats_elementos(stats_elementos_GT)),
                       cbind(dado="stats_elementos_GTS",tab_stats_elementos(stats_elementos_GTS)))

tab_stats <- function(stats,info,conf=0.95){
  tab <- data.frame()
  #browser()
  for(item in stats){
    if(length(item)==0) next
    
    for(agrup in 1:length(item)){
      
      agrupamento <- stringr::str_remove(names(item),"stats_")[agrup]
      
      grupo <- info$grupo
      tecido <- info$tecido
      semana <- info$semana
      elemento <- info$elemento
      
      tab_info <- cbind(grupo=grupo,tecido=tecido,semana=semana,elemento=elemento,agrupamento=agrupamento)
      
      tab_elementos <- item[[agrup]]
      
      for(elemento in names(tab_elementos)){
        stats_summ <- data.frame(t(c(tab_elementos[[elemento]])))
        
        tab <- rbind(tab, cbind(tab_info, item=elemento, stats_summ))
      }
      
    }
  }
  
  return(tab)
}


tab_grupo <- rbind(cbind(dado="stats_grupo",
                          tab_stats(stats_grupo,
                                    list(grupo="mutavel",tecido="fixo",semana="-",elemento="fixo"))))

tab_semana <- rbind(cbind(dado="stats_semana",
                          tab_stats(stats_semana,
                                    list(grupo="fixo",tecido="fixo",semana="mutavel",elemento="fixo"))))

tab_tecido_G <- rbind(cbind(dado="stats_tecido_G",
                          tab_stats(stats_tecido_G,
                                    list(grupo="fixo",tecido="mutavel",semana="-",elemento="fixo"))))

tab_tecido_GS <- rbind(cbind(dado="stats_tecido_GS",
                            tab_stats(stats_tecido_GS,
                                      list(grupo="fixo",tecido="mutavel",semana="fixo",elemento="fixo"))))

tab_geral <- rbind(tab_elementos,tab_grupo,tab_semana,tab_tecido_G,tab_tecido_GS)
write.csv2(tab_geral,"sumario_estatistico.csv", row.names = FALSE)
