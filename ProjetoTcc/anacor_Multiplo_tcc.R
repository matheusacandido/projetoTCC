pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap",
             "readxl",
             "gganimate",
             "amap", 
             "ade4",
             "gifski",
             "plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl")

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

##Inicio do tratamento de dados##
#excluindo a data e o aceite do termo
base_tcc <- base_tcc[,3:25]

  
#alterando nomes das colunas
colnames(base_tcc)[1] = "cidade_residencia" 
colnames(base_tcc)[2] = "faixa_idade" 
colnames(base_tcc)[3] = "genero" 
colnames(base_tcc)[4] = "escolaridade"
colnames(base_tcc)[5] = "praticante_religiao"
colnames(base_tcc)[6] = "fonte_informacao"
colnames(base_tcc)[7] = "interesse_politica"
colnames(base_tcc)[8] = "auto_ideologia"
colnames(base_tcc)[9] = "significado_esquerda"
colnames(base_tcc)[10] = "significado_direita"
colnames(base_tcc)[11] = "apoia_sindicato"
colnames(base_tcc)[12] = "apoia_greve"
colnames(base_tcc)[13] = "governo_deve_reprimir_manifestacao"
colnames(base_tcc)[14] = "apoia_regime_militar"
colnames(base_tcc)[15] = "respeito_leis"
colnames(base_tcc)[16] = "governo_proibir_partidos"
colnames(base_tcc)[17] = "meritocracia_capitalista"
colnames(base_tcc)[18] = "resolucao_problemas_sociais_socialismo"
colnames(base_tcc)[19] = "contra_aborto"
colnames(base_tcc)[20] = "apoia_desarmamento_civil"
colnames(base_tcc)[21] = "apoia_taxar_grandes_fortunas"
colnames(base_tcc)[22] = "governo_responsavel_saude_gratis"
colnames(base_tcc)[23] = "familia_uniao_duas_pessoas"

#Alterando o conteudo para diminuir o tamanho da string
#base_tcc$fonte_informacao <- str_replace("Mídias/Redes sociais", "Mídias sociais (Facebook, Instagram, Twitter, grupos de Whatsapp, Telegram, etc)", base_tcc$fonte_informacao)

#removendo a linha que a pessoa coloca o genero como "translado"
base_tcc <- slice(base_tcc, -123)

#removendo a linha que a pessoa coloca o genero como "nao existe outro"
base_tcc <- slice(base_tcc, -93)


##Comeco do processo para conseguir extrair informacoes para o modelo
#transformando os tipos das variáveis em fatores
base_tcc <- as.data.frame(unclass(base_tcc), stringsAsFactors=TRUE)

#Aqui faremos o teste do qui²
sjt.xtab(var.row = base_tcc$auto_ideologia,
         var.col = base_tcc$escolaridade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = base_tcc$auto_ideologia,
         var.col = base_tcc$praticante_religiao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


sjt.xtab(var.row = base_tcc$auto_ideologia,
         var.col = base_tcc$fonte_informacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = base_tcc$auto_ideologia,
         var.col = base_tcc$apoia_regime_militar,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = base_tcc$interesse_politica,
         var.col = base_tcc$genero,
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
