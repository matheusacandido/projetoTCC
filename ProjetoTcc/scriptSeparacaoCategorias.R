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

#outra forma de visualizar em tabela
tab_xtab(base_tcc$ideologia, base_tcc$socialismo,
         CSS = list(css.table = "border: 2px solid;",
                    css.tdata = "border: 1px solid;",
                    css.horline = "border-bottom: double blue;"))


#Removendo as colunas em que o teste qui² mostrou baixa significancia estatistica entre as duas variaveis
base_tcc <- base_tcc[,4:22]


######################################################################################

#Construindo tabela com o primeiro segmento de perguntas (baseSeg1)
# Você segue alguma religião?
# Por onde se informa mais? 
# Tem interesse em política?
# Quando falamos em Esquerda, qual dessas afirmações faz mais sentido para você?
# Quando falamos em Direita, qual dessas afirmações faz mais sentido para você?

######################################################################################

baseSeg1 <- select(base_tcc, ideologia, religiao, interesse, esquerda, direita)
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


baseSeg3 <- select(base_tcc, ideologia, reg_mil, meritocracia, socialismo)
#baseSeg3 <- select(base_tcc, ideologia, reg_mil, meritocracia, socialismo)

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
  ggtitle("Segmento 3") +
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
  ggtitle("Segmento 4") +
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
  ggtitle("Segmento 5") +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()
