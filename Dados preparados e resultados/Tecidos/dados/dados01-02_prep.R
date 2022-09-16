#Junção dados 01 e dados 02:
dados01 <- read.csv("dados01.csv",stringsAsFactors = FALSE)
dados02 <- read.csv("dados02.csv",stringsAsFactors = FALSE)

#Adicionar 1 coluna indicando de que dataset os dados são:
dados01 <- cbind(dataset="01",dados01)
dados02 <- cbind(dataset="02",dados02)

#Trocar o valor "l" da coluna GRUPO da tabela dados 01 
#para poder padronizar tudo como tumor ou controle nos dados integrados:
dados01 <- dados01 %>% 
  mutate(grupo = replace(grupo, grupo == "l", "t"))

#Mudar nomes de pontos e row para FAIXA E PONTO:
dados01 <- dados01 %>% 
  rename(faixa = pontos) %>%
  rename(ponto = row) %>%
  rename_all(stringr::str_replace, ".K.ppm", "")
dados02 <- dados02 %>% 
  rename(faixa = pontos) %>%
  rename(ponto = row) %>%
  rename_all(stringr::str_replace, ".K.ppm", "")

#Juntar os 2 dataset em 1 só:
dados0102 <- rbind(dados02, dados01)

write.csv(dados0102,"dados0102.csv",row.names = FALSE)
