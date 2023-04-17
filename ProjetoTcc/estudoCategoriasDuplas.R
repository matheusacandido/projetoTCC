pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", 
             "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap",
             "gganimate",
             "gifski",
             "ade4",
             "readxl",
             "dplyr")

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
colnames(base_tcc)[1] = "cidade"
colnames(base_tcc)[2] = "idade" 
colnames(base_tcc)[3] = "genero" 
colnames(base_tcc)[4] = "escol"
colnames(base_tcc)[5] = "religiao"
colnames(base_tcc)[6] = "fonte"
colnames(base_tcc)[7] = "interesse"
colnames(base_tcc)[8] = "ideologia"
colnames(base_tcc)[9] = "esquerda"
colnames(base_tcc)[10] = "direita"
colnames(base_tcc)[11] = "sindicato"
colnames(base_tcc)[12] = "greve"
colnames(base_tcc)[13] = "repressao"
colnames(base_tcc)[14] = "reg_mil"
colnames(base_tcc)[15] = "leis"
colnames(base_tcc)[16] = "partidos"
colnames(base_tcc)[17] = "meritocracia"
colnames(base_tcc)[18] = "socialismo"
colnames(base_tcc)[19] = "aborto"
colnames(base_tcc)[20] = "desarmamento"
colnames(base_tcc)[21] = "taxa"
colnames(base_tcc)[22] = "saude"
colnames(base_tcc)[23] = "familia"


#removendo a coluna com a variável cidade_residencia
base_tcc <- base_tcc[,2:23]

#removendo a linha que a pessoa coloca o genero como "translado"
base_tcc <- slice(base_tcc, -123)

#removendo a linha que a pessoa coloca o genero como "nao existe outro"
base_tcc <- slice(base_tcc, -93)

#removendo a linha que a pessoa coloca em branco o "significado de direita"
base_tcc <- slice(base_tcc, -9)

##Comeco do processo para conseguir extrair informacoes para o modelo
#transformando os tipos das variáveis em fatores
base_tcc <- as.data.frame(unclass(base_tcc), stringsAsFactors=TRUE)

summary(base_tcc)


baseRELIGIAO  <- select(base_tcc, ideologia, religiao)
baseINTERESSE <- select(base_tcc, ideologia, interesse)
baseESQUERDA  <- select(base_tcc, ideologia, esquerda)
baseDIREITA   <- select(base_tcc, ideologia, direita)
baseSINDICATO <- select(base_tcc, ideologia, sindicato)
baseGREVE     <- select(base_tcc, ideologia, greve)
baseREPRESSAO <- select(base_tcc, ideologia, repressao)
baseLEIS      <- select(base_tcc, ideologia, leis)
basePARTIDOS  <- select(base_tcc, ideologia, partidos)
baseDESARMAMENTO <- select(base_tcc, ideologia, desarmamento)
baseREGMIL       <- select(base_tcc, ideologia, reg_mil)
baseMERITOCRACIA <- select(base_tcc, ideologia,  meritocracia)
baseSOCIALISMO   <- select(base_tcc, ideologia, socialismo)
baseTAXA         <- select(base_tcc, ideologia, taxa)
baseSAUDE        <- select(base_tcc, ideologia, saude)
baseFAMILIA      <- select(base_tcc, ideologia, familia)
baseABORTO       <- select(base_tcc, ideologia, aborto)


######preparacao para plotagem baseRELIGIAO
######

#Transformando os tipos das variaveis em fatores
baseRELIGIAO <- as.data.frame(unclass(baseRELIGIAO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegReg <- dudi.acm(baseRELIGIAO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegReg$eig / sum(ACMsegReg$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseRELIGIAO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegReg$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Religiao") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######preparacao para plotagem baseINTERESSE
######

#Transformando os tipos das variaveis em fatores
baseINTERESSE <- as.data.frame(unclass(baseINTERESSE), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegInt <- dudi.acm(baseINTERESSE, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegInt$eig / sum(ACMsegInt$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseINTERESSE,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))


# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegInt$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Interesse") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

######preparacao para plotagem baseESQUERDA
######

#Transformando os tipos das variaveis em fatores
baseESQUERDA <- as.data.frame(unclass(baseESQUERDA), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegEsq <- dudi.acm(baseESQUERDA, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegEsq$eig / sum(ACMsegEsq$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseESQUERDA,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegEsq$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Esquerda") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######preparacao para plotagem baseDIREITA
######

#Transformando os tipos das variaveis em fatores
baseDIREITA <- as.data.frame(unclass(baseDIREITA), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegDir <- dudi.acm(baseDIREITA, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegDir$eig / sum(ACMsegDir$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseDIREITA,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegDir$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Direita") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################

######preparacao para plotagem baseSINDICATO
######

#Transformando os tipos das variaveis em fatores
baseSINDICATO <- as.data.frame(unclass(baseSINDICATO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegSIN <- dudi.acm(baseSINDICATO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegSIN$eig / sum(ACMsegSIN$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSINDICATO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegSIN$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Sindicato") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################

######preparacao para plotagem baseGREVE
######

#Transformando os tipos das variaveis em fatores
baseGREVE <- as.data.frame(unclass(baseGREVE), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegGre <- dudi.acm(baseGREVE, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegGre$eig / sum(ACMsegGre$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseGREVE,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegGre$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Greve") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################


######preparacao para plotagem baseREPRESSAO
######

#Transformando os tipos das variaveis em fatores
baseREPRESSAO <- as.data.frame(unclass(baseREPRESSAO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegRep <- dudi.acm(baseREPRESSAO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegRep$eig / sum(ACMsegRep$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseREPRESSAO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegRep$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Repressao") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################
######preparacao para plotagem baseLEIS
################################################################

#Transformando os tipos das variaveis em fatores
baseLEIS <- as.data.frame(unclass(baseLEIS), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegLei <- dudi.acm(baseLEIS, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegLei$eig / sum(ACMsegLei$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseLEIS,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegLei$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Leis") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


################################################################
######preparacao para plotagem basePARTIDOS
################################################################

#Transformando os tipos das variaveis em fatores
basePARTIDOS <- as.data.frame(unclass(basePARTIDOS), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegPart <- dudi.acm(basePARTIDOS, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegPart$eig / sum(ACMsegPart$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(basePARTIDOS,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegPart$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Partidos") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################
######preparacao para plotagem baseDESARMAMENTO
################################################################

#Transformando os tipos das variaveis em fatores
baseDESARMAMENTO <- as.data.frame(unclass(baseDESARMAMENTO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegDes <- dudi.acm(baseDESARMAMENTO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegDes$eig / sum(ACMsegDes$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseDESARMAMENTO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegDes$c1, Variável = rep(names(quant_categorias),
                                                   quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Desarmamento") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################
######preparacao para plotagem baseREGMIL
################################################################

#Transformando os tipos das variaveis em fatores
baseREGMIL <- as.data.frame(unclass(baseREGMIL), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegReg <- dudi.acm(baseREGMIL, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegReg$eig / sum(ACMsegReg$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseDESARMAMENTO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegReg$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x RegimeMilitar") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()



################################################################
######preparacao para plotagem baseMERITOCRACIA
################################################################

#Transformando os tipos das variaveis em fatores
baseMERITOCRACIA <- as.data.frame(unclass(baseMERITOCRACIA), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegMer <- dudi.acm(baseMERITOCRACIA, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegMer$eig / sum(ACMsegMer$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseMERITOCRACIA,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegMer$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Capitalismo") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()




################################################################
######preparacao para plotagem baseSOCIALISMO
################################################################

#Transformando os tipos das variaveis em fatores
baseSOCIALISMO <- as.data.frame(unclass(baseSOCIALISMO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegSoc <- dudi.acm(baseSOCIALISMO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegSoc$eig / sum(ACMsegSoc$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSOCIALISMO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegSoc$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x ProblemasSociais") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


################################################################
######preparacao para plotagem baseTAXA
################################################################

#Transformando os tipos das variaveis em fatores
baseTAXA <- as.data.frame(unclass(baseTAXA), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegTax <- dudi.acm(baseTAXA, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegTax$eig / sum(ACMsegTax$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseTAXA,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegTax$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Taxas") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


################################################################
######preparacao para plotagem baseSAUDE
################################################################

#Transformando os tipos das variaveis em fatores
baseSAUDE <- as.data.frame(unclass(baseSAUDE), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegSau <- dudi.acm(baseSAUDE, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegSau$eig / sum(ACMsegSau$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSAUDE,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegSau$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Saude") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################
######preparacao para plotagem baseFAMILIA
################################################################

#Transformando os tipos das variaveis em fatores
baseFAMILIA <- as.data.frame(unclass(baseFAMILIA), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegFam <- dudi.acm(baseFAMILIA, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegFam$eig / sum(ACMsegFam$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseFAMILIA,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegFam$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Familia") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################
######preparacao para plotagem baseABORTO
################################################################

#Transformando os tipos das variaveis em fatores
baseABORTO <- as.data.frame(unclass(baseABORTO), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegAbo <- dudi.acm(baseABORTO, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegAbo$eig / sum(ACMsegAbo$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseABORTO,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegAbo$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual
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



