base_tcc <- base_tcc %>%mutate(base_tcc, 
       genero = replace(genero, genero=="Feminino", "F"),
       genero = replace(genero, genero=="Masculino", "M")
       )

base_tcc <- base_tcc %>%mutate(base_tcc, 
                               fonte_informacao = replace(fonte_informacao, fonte_informacao=="Mídias sociais (Facebook, Instagram, Twitter, grupos de Whatsapp, Telegram, etc)", 
                                                          "Midia_sociais")
)


df2 <- base_tcc[complete.cases(base_tcc),]

dados2 <- base_tcc[!is.na(base_tcc$significado_direita),]

base_tcc <- base_tcc[,-18]


base_tcc<-dplyr::select(base_tcc, 
                        auto_ideologia, 
                        apoia_regime_militar
)



#gerando a Anacor multipla
ACM <- dudi.acm(base_tcc, scannf = FALSE)


# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(base_tcc,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


#pacote para usar o sql no R
install.packages("sqldf")
library(sqldf)
install.packages("plotly")
require(plotly)


base_teste <- sqldf("Select * from baseSeg5 where familia='Sim'")
summary(base_teste)

baseAbortoReligiao <- select(base_tcc, ideologia, aborto, religiao)



#Transformando os tipos das variaveis em fatores
baseAbortoReligiao <- as.data.frame(unclass(baseAbortoReligiao), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegRelAbo <- dudi.acm(baseAbortoReligiao, scannf = FALSE,nf = 3)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegRelAbo$eig / sum(ACMsegRelAbo$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseAbortoReligiao,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegRelAbo$c1, Variável = rep(names(quant_categorias),
                                                     quant_categorias))

# Plotando o mapa perceptual
ggplotly(
  df_ACM %>%
    rownames_to_column() %>%
    rename(Categoria = 1) %>%
    ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
    geom_point() +
    ggtitle("Ideologia x Aborto") +
    geom_label_repel() +
    geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
    geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
    labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
         y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
    theme_bw()
)

ACM_3D <- plot_ly()
ACM_3D <- add_trace(p = ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D

##########################################################
#salvando em html o grafico 3D

library("htmlwidgets")

htmlwidgets::saveWidget(
  widget = ACM_3D, #the plotly object
  file = "ideologiaAbortoReligiao.html", #the path & file name
  selfcontained = TRUE #creates a single html file
)
############################################################


base_teste <- sqldf("Select ideologia, aborto, idade from base_tcc where aborto='Sim'")
summary(base_teste)
base_testeNao <- sqldf("Select ideologia, aborto, idade from base_tcc where aborto='Não'")
summary(base_testeNao)


base_idadeAborto <- sqldf("Select ideologia, aborto, idade from base_tcc")
summary(base_idadeAborto)


install.packages("animation")
require(animation)



