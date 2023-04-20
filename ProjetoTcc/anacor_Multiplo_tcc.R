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
             "dplyr",
             "sqldf")

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

#alterando o nome das colunas
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
colnames(base_tcc)[17] = "capitalismo"
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

#Aqui faremos o teste do qui-quadrado para verificar a significancia da associacao entre duas váriaveis

#p-valor >0.05
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$idade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#p-valor >0.05
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#p-valor >0.05
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$escol,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$religiao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$fonte,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$interesse,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#siginificante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$esquerda,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$direita,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$sindicato,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$greve,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$repressao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$reg_mil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$leis,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$partidos,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$capitalismo,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$socialismo,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$aborto,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$desarmamento,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$taxa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$saude,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

#significante
sjt.xtab(var.row = base_tcc$ideologia,
         var.col = base_tcc$familia,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


#outra forma de visualizar em tabela
tab_xtab(base_tcc$ideologia, base_tcc$familia,
         CSS = list(css.table = "border: 2px solid;",
                    css.tdata = "border: 1px solid;",
                    css.horline = "border-bottom: double blue;"))


#Removendo as colunas em que o teste qui² mostrou baixa significancia estatistica entre as duas variaveis
base_tcc <- base_tcc[,4:22]

############INICIO DA ANALISE POR CORREPONDENCIA MULTIPLA ####################

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
ggplotly(
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
)


######################################################################################

###########INICIO ESTUDO POR GRUPOS DE PERGUNTAS#####################################

#Construindo tabela com o primeiro segmento de perguntas (baseSeg1)
# Você segue alguma religião?
# Por onde se informa mais? 
# Tem interesse em política?
# Quando falamos em Esquerda, qual dessas afirmações faz mais sentido para você?
# Quando falamos em Direita, qual dessas afirmações faz mais sentido para você?

######################################################################################

baseSeg1 <- select(base_tcc, ideologia, religiao, fonte, interesse, esquerda, direita)
#baseSeg1 <- select(base_tcc, ideologia, religiao, interesse, esquerda, direita)

#baseSeg1 <- NULL

baseSeg1  <-   mutate(baseSeg1,
                      interesse = case_when(
                        interesse=="Sim, me interesso por política" ~ "sim",
                        interesse=="É indiferente para mim"         ~ "indiferente",
                        interesse=="Não me interesso por política"  ~ "nao",
                        TRUE                                        ~ "other"
                      )
)
#Transformando os tipos das variaveis em fatores
baseSeg1 <- as.data.frame(unclass(baseSeg1), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMseg1 <- dudi.acm(baseSeg1, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMseg1$eig / sum(ACMseg1$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSeg1,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMseg1$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual da primeira base

df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Grupo 1") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######################################################################################

#Construindo tabela com o segundo segmento de perguntas (baseSeg2)
#  Você apoia a existência de sindicatos?
#  Você apoia o direito de greve trabalhista?
#  O governo deve usar agentes da lei para acabar com manifestações?
#  Leis devem ser sempre obedecidas (sendo elas justas ou não)?
#  O governo deve poder proibir a existência de algum partido político?
#  É a favor do desarmamento civil, especialmente em centros urbanos?

######################################################################################


baseSeg2 <- select(base_tcc, ideologia, sindicato, greve, repressao, leis, partidos, desarmamento)
#baseSeg2 <- select(base_tcc, ideologia, sindicato, greve, repressao, leis, partidos, desarmamento)

#baseSeg2 <- NULL

#Transformando os tipos das variaveis em fatores
baseSeg2 <- as.data.frame(unclass(baseSeg2), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMseg2 <- dudi.acm(baseSeg2, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMseg2$eig / sum(ACMseg2$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSeg2,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMseg2$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual da segunda base
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Grupo 2") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######################################################################################

#Construindo tabela com o terceiro segmento de perguntas (baseSeg3)
#  O regime militar deveria voltar?
#  No capitalismo, quem se esforça sempre é recompensado?
#  No socialismo, todos os problemas sociais são sanados?


######################################################################################


baseSeg3 <- select(base_tcc, ideologia, reg_mil, capitalismo, socialismo)
#baseSeg3 <- select(base_tcc, ideologia, reg_mil, capitalismo, socialismo)

#baseSeg3 <- NULL


#Transformando os tipos das variaveis em fatores
baseSeg3 <- as.data.frame(unclass(baseSeg3), stringsAsFactors=TRUE)


#gerando a Anacor multipla
ACMseg3 <- dudi.acm(baseSeg3, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMseg3$eig / sum(ACMseg3$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSeg3,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMseg3$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual da terceira base
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Grupo 3") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######################################################################################

#Construindo tabela com o quarto segmento de perguntas (baseSeg4)
# As grandes fortunas (investimentos e bens acima de 20 milhões de reais) devem ser taxadas e, com essa arrecadação, investir em melhorias para a população?
# Toda pessoa deveria ter direito à saúde de graça e com qualidade, oferecida pelo governo?


######################################################################################


baseSeg4 <- select(base_tcc, ideologia, taxa, saude)
#baseSeg4 <- select(base_tcc, ideologia, taxa, saude)

#baseSeg4 <- NULL

#Transformando os tipos das variaveis em fatores
baseSeg4 <- as.data.frame(unclass(baseSeg4), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMseg4 <- dudi.acm(baseSeg4, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMseg4$eig / sum(ACMseg4$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSeg4,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMseg4$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual da quarta base
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Grupo 4") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


######################################################################################

#Construindo tabela com o quinto segmento de perguntas (baseSeg5)
# Você considera a família como união de duas ou mais pessoas, independentemente do gênero destas?
# Você é contra o aborto, sendo a situação prevista ou não em lei?

######################################################################################


baseSeg5 <- select(base_tcc, ideologia, familia, aborto)
#baseSeg5 <- select(base_tcc, ideologia, familia, aborto)

#baseSeg5 <- NULL

#Transformando os tipos das variaveis em fatores
baseSeg5 <- as.data.frame(unclass(baseSeg5), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMseg5 <- dudi.acm(baseSeg5, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMseg5$eig / sum(ACMseg5$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseSeg5,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))



# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMseg5$c1, Variável = rep(names(quant_categorias),
                                                quant_categorias))

# Plotando o mapa perceptual da quinta base
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Grupo 5") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

###########FIM ESTUDO POR GRUPOS DE PERGUNTAS#####################################

###########INICIO ESTUDO POR DUPLA DE PERGUNTAS(ACS) #############################

baseRELIGIAO  <- select(base_tcc, ideologia, religiao)
baseINTERESSE <- select(base_tcc, ideologia, interesse)
baseFONTE     <- select(base_tcc, ideologia, fonte)
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

######preparacao para plotagem baseFONTE
######

#Transformando os tipos das variaveis em fatores
baseFONTE <- as.data.frame(unclass(baseFONTE), stringsAsFactors=TRUE)

#gerando a Anacor multipla
ACMsegFon <- dudi.acm(baseFONTE, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACMsegFon$eig / sum(ACMsegFon$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(baseFONTE,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACMsegFon$c1, Variável = rep(names(quant_categorias),
                                                  quant_categorias))

# Plotando o mapa perceptual

df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  ggtitle("Ideologia x Fonte") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

################################################################


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
quant_categorias <- apply(baseREGMIL,
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

###########FIM ESTUDO POR DUPLA DE PERGUNTAS(ACS) #############################

