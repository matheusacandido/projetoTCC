pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", 
             "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap",
             "readxl",
             "gganimate",
             "gifski",
             "FactoMineR", 
             "dplyr", 
             "ade4",
             "stringr")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Importando a base de dados
base_tcc <- read.csv("Questionario.csv")

#excluindo a data e o aceite do termo
base_tcc <- base_tcc[,3:25]


#alterando nomes das colunas
colnames(base_tcc)[2] = "faixa_idade" 
colnames(base_tcc)[3] = "genero" 
colnames(base_tcc)[4] = "escolaridade"
colnames(base_tcc)[5] = "praticante_religiao"
colnames(base_tcc)[6] = "fonte_informacao"
colnames(base_tcc)[7] = "interesse_politica"
colnames(base_tcc)[8] = "auto_ideologia"

base_tcc <- base_tcc[,2:8]

#Alterando o conteudo para diminuir o tamanho da string
base_tcc$fonte_informacao <- str_replace("Mídias/Redes sociais", "Mídias sociais (Facebook, Instagram, Twitter, grupos de Whatsapp, Telegram, etc)", base_tcc$fonte_informacao)

#removendo a linha que a pessoa coloca o genero como "translado"
base_tcc <- slice(base_tcc, -123)

#removendo a linha que a pessoa coloca o genero como "nao existe outro"
base_tcc <- slice(base_tcc, -93)


#transformando os tipos das variáveis em fatores
base_tcc <- as.data.frame(unclass(base_tcc), stringsAsFactors=TRUE)

#Aqui faremos o teste do qui²
sjt.xtab(var.row = base_tcc$faixa_idade,
         var.col = base_tcc$escolaridade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = base_tcc$faixa_idade,
         var.col = base_tcc$praticante_religiao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


sjt.xtab(var.row = base_tcc$fonte_informacao,
         var.col = base_tcc$escolaridade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


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
