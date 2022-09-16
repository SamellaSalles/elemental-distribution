# library
install.packages("ggridges")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("patchwork")
install.packages("GGally")
install.packages("ggrepel")
install.packages("lemon")

library(ggridges)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(patchwork)
library(GGally)
library(ggrepel)
library(lemon)

dados_log <- data.frame(dados[1:3], lapply(dados[4:11], log10) )

data <- dados_log
n_colunas <- ncol(data)
nomes_elementos <- names(data)[(n_colunas-7):n_colunas]
id.vars <- which(!names(data) %in% nomes_elementos)

require(reshape)
data <- reshape::melt(as.data.frame(data), id.vars = id.vars)
names(data) <- c("grupo","tecido","semana","elemento","concentracao")
#dados_elementos <- agrupamento[c("variable","value")]


#Dados de relevância
char_agrupamento <- strsplit(tab_geral$agrupamento, "")
grupo <- tecido <- semana <- c()
for(char in char_agrupamento){
  grupo <- c(grupo,char[1])
  tecido <- c(tecido,tryCatch(char[2],error = function(e) NA))
  semana <- c(semana,tryCatch(char[3],error = function(e) NA))
}
stats <- tab_geral

stats$grupo <- grupo
stats$tecido <- tecido
stats$semana <- semana
stats$agrupamento <- NULL



#==================== Nicho (distr.) =================

#------- 1-Distribuições diferentes C e T? ------------

# POR TECIDO [ALTERAR]
# separando o tecido
dado_tecido <- data[data$tecido=="f",]


#.............. Violin plot + Boxplot ............
# sample size
sample_size = dado_tecido %>% group_by(elemento,grupo) %>% summarize(num=n())

# Plot
dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=elemento, y=concentracao, fill=grupo)) +
  geom_violin(width=1, position = position_dodge(width=0.95)) +
  geom_boxplot(width=0.25, color="black", alpha=0.2) + #, position = position_dodge(width=0.95)
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18),
    legend.key.size = unit(2,"line")) +
  xlab("Elemento")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("dodgerblue", "red1"), 
                    name="Grupo",
                    labels=c("Controle", "Tumor")) +
  scale_y_continuous(limit = c(-4, 6))


#.............. Medianas + quartis ............
# Calculates mean, sd, median, etc
my_sum <- dado_tecido %>%
  group_by(elemento,grupo) %>%
  summarise( 
    n=n(),
    median=median(concentracao),
    qr3=quantile(concentracao,3/4),
    qr1=quantile(concentracao,1/4)
  )
  
ggplot(my_sum) +
  geom_errorbar( aes(x=elemento, ymin=qr1, ymax=qr3, group=grupo), position = position_dodge(width=0.9), width=0.4, colour="black", alpha=0.9, size=0.5) +
  geom_point( aes(x=elemento, y=median, color=grupo), size=5, stat="identity", position=position_dodge(width=0.9), alpha=1) +
  xlab("Elemento")+
  ylab("Concentração (log10(ppm))")+
  scale_color_manual(values = c("dodgerblue", "red1"), 
                     name="Grupo",
                     labels=c("Controle", "Tumor"))+
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) 


#------- 2-Distribuições ao longo das semanas por tecido ------------

# separando o figado e removendo a semana 0
dado_tecido <- data[data$tecido=="f" & !data$semana==0,]


#.............. Violin plot + Boxplot ............
# sample size
sample_size = dado_tecido %>% group_by(elemento,grupo) %>% summarize(num=n())

# Plot
dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=grupo)) +
  geom_violin(width=1, position = position_dodge(width=1)) +
  geom_boxplot(width=0.4, color="grey40", alpha=0.2, position = position_dodge(width=1)) +
  facet_wrap(~ elemento, ncol=4) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18),
    strip.text.x = element_text(colour="black", size=18)) +
  xlab("Semana")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("dodgerblue", "red1"), 
                    name="Grupo",
                    labels=c("C", "T"))+
  scale_color_manual(values = c("blue4", "red4"))


#.............. Median lines + IQR ............

dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=grupo)) +
  geom_boxplot(width=0.4, color="grey40", alpha=0.1, position = position_dodge(width=0)) +
  facet_wrap(~ elemento, ncol=4) +
  stat_summary(aes(group = grupo, color=grupo, linetype=grupo), geom = 'line', fun = median, position = position_dodge(width = 0), size=1, alpha=1) +
  stat_summary(aes(group = grupo, color=grupo), geom = 'point', fun = median, position = position_dodge(width = 0), size=1.5) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18),
    strip.text.x = element_text(colour="black", size=18)) +
  xlab("Semana")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("dodgerblue", "red1"))+
  scale_color_manual(values = c("dodgerblue", "red1"))

#.............. Parallel coordinates ............

# Calculates mean, sd, median, etc
my_sum <- dado_tecido %>%
  group_by(elemento,grupo,semana) %>%
  summarise( 
    n=n(),
    median=median(concentracao),
    qr3=quantile(concentracao,3/4),
    qr1=quantile(concentracao,1/4)
  )

my_sum <- my_sum[,c(1,2,3,5)]

ggplot(my_sum, aes(x=semana, y=median, color=grupo, group=interaction(elemento, grupo))) +
  geom_line( size=0.9, stat="identity", alpha=1) +
  geom_point( size=2, stat="identity", alpha=1) +
  xlab("Semana") +
  ylab("Concentração (log10(ppm))") +
  scale_color_manual(values = c("dodgerblue", "red1"), 
                     name="Grupo",
                     labels=c("C", "T"))+
  theme_bw() +
  theme(
  legend.position="right",
  plot.title = element_text(size=14), 
  axis.title.x = element_text(colour="black", size=18),
  axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
  axis.title.y = element_text(colour="black", size=18),
  axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
  legend.title = element_text(colour="black", size=18), 
  legend.text = element_text(colour="black", size=18),
  strip.text.x = element_text(colour="black", size=18))


#------- 3-Distribuições pelos tecidos (por elemento) ------------

# separando o figado e removendo a semana 0
dado_tecido <- data[data$semana==5,]
dado_tecido$tecido <- factor(dado_tecido$tecido, levels=c("t", "f", "p"))
dado_tecido$gt <- factor(paste(dado_tecido$grupo,dado_tecido$tecido,sep="."), ordered=TRUE, levels=c("c.p", "t.p","c.f","t.f","t.t"))
#dado_tecido$gt <- interaction(dado_tecido$grupo,dado_tecido$tecido)

#.............. Ridgeline (por tecido) ............
dado_tecido %>%
  arrange(grupo) %>%
  ggplot( aes(y=reorder(gt,desc(gt)), x=concentracao, fill=interaction(tecido,grupo))) +
  geom_density_ridges(alpha=0.6, bandwidth=0.05,scale=0.8) +
  geom_boxplot(alpha=0.6,width=0.15) +
  facet_wrap( ~ elemento, ncol=4) +
  theme_bw() +
  theme(
    legend.position="none", #Para tirar a legenda
    panel.spacing = unit(0.4, "lines"),
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18, vjust=-1),
    axis.text.x  = element_text(angle=0, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18, vjust=1.5),
    axis.text.y  = element_text(angle=0, colour="black", vjust=0.5, hjust=0.8, size=18), 
    legend.title = element_text(colour="black", size=14),
    legend.text = element_text(colour="black", size=14), 
    strip.text.x = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) +
  xlab("Concentration (log10(ppm))") +
  ylab("Tissue") +
  #Para adicionar cor por grupo:
  #scale_fill_manual(values = c("dodgerblue", "red1"), 
  #                  name="Group",
  #                  labels=c("Control", "Tumor"))+
  #Para adicionar cor por tecido na legenda: 
  scale_fill_manual(values = c("lightskyblue2","royalblue", "red4","lightsalmon", "orangered2"),
                    name="Tissue",
                    labels=c("Liver (Control)", "Lung (Control)", "Primary Tumor", "Liver (Tumor)", "Lung (Tumor)")) +
  scale_y_discrete(labels=c("c.f"="Liver (Control)","t.f"="Liver (Tumor)","c.p"="Lung (Control)","t.p"="Lung (Tumor)","t.t"="Primary Tumor ")) +
  scale_x_continuous(limit = c(-4, 6))



#------- 4-Distribuição do tumor vs. fígado (por semana) ------------

# separando o figado e removendo a semana 0
dado_tecido <- data[!data$semana==0 & (data$tecido=="t" | data$tecido=="f"),]


#.............. Violin plot + Boxplot ............
# sample size
sample_size = dado_tecido %>% group_by(elemento,grupo) %>% summarize(num=n())

# Plot
dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=interaction(tecido, grupo))) +
  geom_violin(width=0.95, position = position_dodge(width=0.92)) +
  geom_boxplot(width=0.35, color="black", alpha=0.2, position = position_dodge(width=0.92)) +
  geom_vline(aes(xintercept=1.5), colour="grey30", linetype="solid") +
  geom_vline(aes(xintercept=2.5), colour="grey30", linetype="solid") +
  facet_rep_wrap(~ elemento, ncol=2, strip.position = "top", repeat.tick.labels = TRUE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=0, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=0, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=16), 
    legend.text = element_text(colour="black", size=16), 
    strip.text.x = element_text(colour="black", size=16),
    legend.key.size = unit(2, "lines")) +
  xlab("Week")+
  ylab("Concentration (log10(ppm))")+
  scale_fill_manual(values = c("lightskyblue2", "lightsalmon", "red4"), 
                    name="Tissue",
                    labels=c("Liver (Control)", "Liver (Tumor)", "Primary Tumor")) +
  scale_y_continuous(limit = c(-4, 6)) +
  scale_x_discrete(position = "bottom")


#.............. Median lines + IQR ............

dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=interaction(tecido, grupo))) +
  #geom_boxplot(width=0.4, color="grey40", alpha=0.1, position = position_dodge(width=0)) +
  facet_wrap(~ elemento, ncol=4) +
  stat_summary(aes(group = interaction(tecido, grupo), color=interaction(tecido, grupo)), geom = 'line', fun = median, position = position_dodge(width = 0), size=1.5, alpha=1) +
  stat_summary(aes(group = interaction(tecido, grupo), color=interaction(tecido, grupo)), geom = 'point', fun = median, position = position_dodge(width = 0), size=2) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18), 
    strip.text.x = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) +
  xlab("Semana")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("lightskyblue2", "lightsalmon", "red4"), 
                    name="Tecido",
                    labels=c("Fígado (Controle)", "Fígado (Tumor)", "Tumor Primário"))+
  scale_color_manual(values = c("lightskyblue2", "lightsalmon", "red4"), 
                     name="Tecido",
                     labels=c("Fígado (Controle)", "Fígado (Tumor)", "Tumor Primário"))


#.............. Parallel coordinates ............

# Calculates mean, sd, median, etc
my_sum <- dado_tecido %>%
  group_by(elemento,tecido,semana) %>%
  summarise( 
    n=n(),
    median=median(concentracao),
    qr3=quantile(concentracao,3/4),
    qr1=quantile(concentracao,1/4)
  )

my_sum <- my_sum[,c(1,2,3,5)]

ggplot(my_sum, aes(x=semana, y=median, color=tecido, group=interaction(elemento, tecido))) +
  geom_line( size=0.9, stat="identity", alpha=1) +
  geom_point( size=2, stat="identity", alpha=1) +
  xlab("Semana") +
  ylab("Concentração (log10(ppm))") +
  scale_color_manual(values = c("lightsalmon", "red4"), 
                     name="Tecido",
                     labels=c("F", "T")) +
  theme_bw() +
  theme(
    legend.position="right",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18))


# Continuando no pulmão para a semana 5..........

# separando o figado e removendo a semana 0
dado_tecido <- data[data$semana==5 & (data$tecido=="t" | data$tecido=="p"),]


#.............. Violin plot + Boxplot ............
# sample size
sample_size = dado_tecido %>% group_by(elemento,grupo) %>% summarize(num=n())

# Plot
dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=interaction(tecido, grupo))) +
  geom_violin(width=1, position = position_dodge(width=1)) +
  geom_boxplot(width=0.4, color="black", alpha=0.2, position = position_dodge(width=1)) +
  facet_wrap(~ elemento, ncol=4) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18), 
    strip.text.x = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) +
  xlab("Semana")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("royalblue", "orangered2", "red4"), 
                    name="Tecido",
                    labels=c("Pulmão (Controle)", "Pulmão (Tumor)", "Tumor Primário"))+
  scale_y_continuous(limit = c(-4, 6))

#==================== Biomarcador (distr.) =================

#------- 1-Distribuições tumor vs. outros tecidos (TODOS) ------------

#.............. Ridgeline (por tecido) ............
dado_tecido <- data
dado_tecido$tecido <- factor(dado_tecido$tecido, levels=c("t", "f", "p"))

dado_tecido %>%
  ggplot( aes(y=interaction(tecido, grupo), x=concentracao,  fill=grupo)) +
  geom_density_ridges(alpha=0.6, bandwidth=0.05) +
  facet_wrap(~ elemento, ncol=4) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.spacing = unit(0.1, "lines"),
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18), 
    strip.text.x = element_text(colour="black", size=18)
  ) +
  xlab("Concentração (log10(ppm))") +
  ylab("Tecido") +
  scale_fill_manual(values = c("dodgerblue", "red1"), 
                    name="Grupo",
                    labels=c("C", "T"))+
  scale_y_discrete(labels=c("CF", "CP", "TT", "TF", "TP"))


#------- 2-Distribuição do tumor (por semana) ------------

# separando o figado e removendo a semana 0
dado_tecido <- data[data$grupo=="t" & data$tecido=="t",]

#.............. Violin plot + Boxplot ............
# sample size
sample_size = dado_tecido %>% group_by(elemento,grupo) %>% summarize(num=n())

# Plot
dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=tecido)) +
  geom_violin(width=1, position = position_dodge(width=1)) +
  geom_boxplot(width=0.4, color="black", alpha=0.2, position = position_dodge(width=1)) +
  #stat_summary(aes(group = tecido, color=tecido, linetype=tecido), geom = 'line', fun = median, position = position_dodge(width = 0), size=1, alpha=1) +
  #stat_summary(aes(group = tecido, color=tecido), geom = 'point', fun = median, position = position_dodge(width = 0), size=1.5) +
  facet_wrap(~ elemento, ncol=4) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18),
    axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18), 
    strip.text.x = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) +
  xlab("Semana")+
  ylab("Concentração (log10(ppm))")+
  scale_fill_manual(values = c("red4"), 
                    name="Tecido",
                    labels=c("Tumor Primário")) +
  scale_y_continuous(limit = c(-6, 7), breaks = scales::pretty_breaks(n = 6)) 
  #scale_color_manual(values = c("red4"))


#.............. Median lines + IQR ............

dado_tecido %>%
  left_join(sample_size) %>%
  ggplot( aes(x=semana, y=concentracao, fill=tecido)) +
  geom_boxplot(width=0.4, color="grey40", alpha=0.1, position = position_dodge(width=0)) +
  facet_wrap(~ elemento, ncol=4) +
  stat_summary(aes(group = tecido, color=tecido), geom = 'line', fun = median, position = position_dodge(width = 0), size=1.5, alpha=1) +
  stat_summary(aes(group = tecido, color=tecido), geom = 'point', fun = median, position = position_dodge(width = 0), size=2) +
  theme_bw() +
  theme(
    legend.position="bottom",
    plot.title = element_text(size=14), 
    axis.title.x = element_text(colour="black", size=18),
    axis.text.x  = element_text(angle=0, colour="black", vjust=0.5, size=18), 
    axis.title.y = element_text(colour="black", size=18, vjust = 1.5),
    axis.text.y  = element_text(angle=0, colour="black", vjust=0.5, size=18), 
    legend.title = element_text(colour="black", size=18), 
    legend.text = element_text(colour="black", size=18),
    strip.text.x = element_text(colour="black", size=18),
    legend.key.size = unit(2, "lines")) +
  xlab("Week")+
  ylab("Concentration (log10(ppm))")+
  scale_color_manual(values = c("red4"), 
                     name="Tissue",
                     labels=c("Primary Tumor")) + 
  scale_fill_manual(values = c("red4"), 
                    name="Tissue",
                    labels=c("Primary Tumor")) +
  scale_y_continuous(limit = c(-6, 7), breaks = scales::pretty_breaks(n = 6))

 


#.............. Parallel coordinates ............

# Calculates mean, sd, median, etc
my_sum <- dado_tecido %>%
  group_by(elemento,tecido,semana) %>%
  summarise( 
    n=n(),
    median=median(concentracao),
    qr3=quantile(concentracao,3/4),
    qr1=quantile(concentracao,1/4)
  )

my_sum <- my_sum[,c(1,2,3,5)]

ggplot(my_sum, aes(x=semana, y=median, color=tecido, group=interaction(elemento, tecido))) +
  geom_line( size=0.9, stat="identity", alpha=1) +
  geom_point( size=2, stat="identity", alpha=1) +
  xlab("Semana") +
  ylab("Concentração (log10(ppm))") +
  scale_color_manual(values = c("red4"), 
                     name="Tecido",
                     labels=c("T")) +
  theme_bw()+
  theme(
  legend.position="bottom",
  plot.title = element_text(size=14), 
  axis.title.x = element_text(colour="black", size=18),
  axis.text.x  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
  axis.title.y = element_text(colour="black", size=18),
  axis.text.y  = element_text(angle=90, colour="black", vjust=0.5, size=18), 
  legend.title = element_text(colour="black", size=18), 
  legend.text = element_text(colour="black", size=18))
  #geom_label_repel(aes(label = elemento), nudge_x = 1, na.rm = TRUE)
