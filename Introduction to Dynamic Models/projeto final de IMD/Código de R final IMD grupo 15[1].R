################################################################################

# Introdução a Modelos Dinâmicos: Código Completo do Trabalho

# Docente: Maria Figueiredo

# Grupo 15
# Realizado por:
#
#        Miguel Celestino, 
#        Ana Almeida, 
#        Rita Guerreiro, 
#        Mariana Pimenta, 
#        Filipe Duarte, 

# Abril 2024 #

################################################################################

################################################################################

#Bibliotecas usadas:

################################################################################

#Usada a função describe(), para facilitar na análise estatística dos dados.
library(psych)

#Usada a função separate(), para ajudar no tratamento/processamento das variáveis.
library(tidyr)

#Usada a função case_when(), para a criação da variável attractivness.
library(dplyr)

#Usada a função rescale(), tambem utilizada na criação da variável attractivness 
library(scales)

#usada a função ggplot(), para a criação de gráficos mais complexos.
library(ggplot2)

#usada a função corrplot(), para a visualização de correlações.
library(corrplot)

#usada a função dummy_cols(), para facilitar a criação de variáveis dummy.
library(fastDummies)

#usada a função mice(), para fazer imputação de valores.
library(mice)

#usada a função etaSquared(), para o calculo do valor eta.
library(lsr)

#Uso da função glm(), para a criação de modelos de regressão linear generalizada.
library(MASS)

#Uso da função outlierTest(), para avaliar quais as observações extremas em modelos criados
library(car)

#Uso da função ymd(), para ajudar no tratamento da variável last_review que 
#apenas contem datas.
library(lubridate)

#Uso da função coeftest(), para a correção dos erros residuais dos modelos criados.
library(sandwich)

#Uso da função createDataPartition(), para dividir o dataframe em amostras de treino e teste.
library(caret)

#Para a execução do breuch-Pagan test
library(lmtest)

#Para a execução do jarque.bera.test
library(tseries)

#Para criar visualizações coloridas e mais informativas
library(RColorBrewer)

################################################################################

# Descrição Geral da Base de Dados

################################################################################


#Remover tudo da base de dados
rm(list=ls(all=TRUE))

#Ler base de dados 
bd <- read.csv(file.choose())


# Descrição Geral dos dados

# Primeiras 6 linhas da base de dados
head(bd)

# Últimas 6 linhas da base de dados
tail(bd)

# Dimenção do dataset (9388 linhas e 18 colunas)
dim(bd)

# Quais as colunas
names(bd)

# Espicificações iniciais:
str(bd)

#Existência de linhas duplicadas
sum(duplicated(bd))


################################################################################

# #Data Understanding (RAW) de cada Variável

################################################################################


# Sumário Geral
summary(bd)

# Descritivos
describe(bd)



################################################################################
# Variável "id" 
################################################################################


#Sumário da Variável
describe(bd$id)
summary(bd$id)

#Boxplot da Variável id
boxplot(bd$id, main = "Boxplot dos Ids")

#Histograma dos Ids
hist(bd$id)

#Verificação de valores omissos
sum(bd$id=='')
sum(bd$id==' ')
sum(bd$id=='NA')
sum(is.na(bd$id))

#Existência de anúncios duplicados
sum(duplicated(bd$id))



################################################################################
# Variável "name" 
################################################################################

#Sumário da Variável
summary(bd$name)

#Tabela que mostra os primeiros 6 valores da variável 
head(sort(table(bd$name),decreasing = TRUE))

#Quantos elementos únicos na variável
length(unique(bd$name))

#Verificação de valores omissos
sum(bd$name=='')
sum(bd$name==' ')
sum(bd$name=='NA')
sum(is.na(bd$name))

#O valor com maior frequência é 
#"Rental unit in Praha 1 · ★New · 1 bedroom · 1 bed · 1 bath" com uma f
#requência absoluta de 93. Existe uma grande variedade de valores, 
#6920 valores distintos. Não existem valores omissos. 



################################################################################
# Variável "host_id" 
################################################################################

#Sumário da Variável
describe(bd$host_id)
summary(bd$host_id)

#Boxplot da Variável
hist(bd$host_id, main = "Distribuição dos Ids dos anfitriões")

#Verificação de Valores omissos
sum(is.na(bd$host_id))
sum(bd$host_id=='')
sum(bd$host_id==' ')
sum(bd$host_id=='NA')



################################################################################
# Variável "host_name" 
################################################################################

#Sumário da Variável
summary(bd$host_name)

#Tabela dos 6 host_names com maior frequência absoluta
head(sort(table(bd$host_name),decreasing = TRUE))

#Elementos únicos da variável
length(unique(bd$host_name))

#Verificação de Valores omissos
sum(is.na(bd$host_name))
sum(bd$host_name=='')
sum(bd$host_name==' ')
sum(bd$host_name=='NA')


#Mesma situação que a variável name. O nome com maior frequência absoluta é 
#"Prague Days". Muitos valores distintos, 1418. Não existem valores omissos.



################################################################################
# Variável "neighbourhood_group"  
################################################################################

# Coluna de valores omissos

#Sumário da Variável
summary(bd$neighbourhood_group)



################################################################################
# Variável "neighbourhood"  
################################################################################

#Sumário da Variável
summary(bd$neighbourhood)

# Tabela dos 6 distritos municipais de Praga com maior frequência absoluta 
head( sort(table(bd$neighbourhood), decreasing = TRUE))

# Criar uma paleta de cores pasteis
cores_pasteis <- brewer.pal(6, "Pastel1")

#Gráfico de Barras da tabela anterior
barplot(head(sort(table(bd$neighbourhood), decreasing = TRUE)), 
        cex.names = 0.8,
        main = "Gráfico de Barras dos 6 distritos municipais de Praga com maior F.A",
        col = cores_pasteis)

#Número de distritos municipais diferentes em Praga
length(unique(bd$neighbourhood))

#Verificação de Valores omissos
sum(is.na(bd$neighbourhood))
sum(bd$neighbourhood=='')
sum(bd$neighbourhood==' ')
sum(bd$neighbourhood=='NA')

#O valor com maior frequência absoluta, 3477, é "Praha 1". 
#Muita variedade de valores distintos, 51. Sem valores omissos.



################################################################################
# Variável "latitude"  
################################################################################

#Sumário da Variável
describe(bd$latitude)
summary(bd$latitude)

#Boxplot da Variável
boxplot(bd$latitude, main = "Boxplot da Latitude", ylab="Valores em Graus")

#Verificação de Valores omissos
sum(is.na(bd$latitude))
sum(bd$latitude=='')
sum(bd$latitude==' ')
sum(bd$latitude=='NA')



################################################################################
# Variável "longitude"  
################################################################################

#Sumário da Variável
describe(bd$longitude)
summary(bd$longitude)

#Boxplot da Variável
boxplot(bd$longitude, main = "Boxplot da Longitude", ylab="Valores em Graus")

#Verificação de Valores omissos
sum(is.na(bd$longitude))
sum(bd$longitude=='')
sum(bd$longitude==' ')
sum(bd$longitude=='NA')


#Verificação de anúncios com a mesma latitude e longitude
sum(duplicated(bd[
  , c("latitude", "longitude")]) | duplicated(bd[
    , c("longitude", "latitude")]))


# Muitos outliers no gráfico mas deve ser normal dado que é um indicador 
# geográfico. 
# A maior parte dos Airbnbs devem estar localizados no centro da cidade talvez.
# Existem 1010 anúncios onde existe a mesma latitude e longitude (conjuntas)



################################################################################
# Variável "room_type"  
################################################################################

# Sumário da Variável
summary(bd$room_type)

# Tabela dos tipos de Arrendamento
room_type_table1 <- sort(table(bd$room_type), decreasing = TRUE)
room_type_table1

# Peso de cada Frequência absoluta, face ao total
round(prop.table(room_type_table1)*100, 3)

# Número de categorias diferentes na variável
length(unique(bd$room_type))

# Frequências relativas
freq_rel <- room_type_table1 / sum(room_type_table1) * 100

# Gráfico de pizza sobre a variável
pie(as.vector(room_type_table1),
    labels = paste(names(room_type_table1), "\n", round(freq_rel, 2), "%", sep = ""),
    main = "Gráfico de Pizza sobre Tipos de Propriedades",
    cex = 1.3,         # Ajusta o tamanho do texto nas fatias
    cex.main = 1.8     # Ajusta o tamanho do título
)



# Verificação de Valores omissos
sum(is.na(bd$room_type))
sum(bd$room_type=='')
sum(bd$room_type==' ')
sum(bd$room_type=='NA')

#A esmagadora maioria é da categoria "Entire home/apt". Não existem valores 
#omissos.



################################################################################
# Variável "price"  
################################################################################

# Sumário da Variável
describe(bd$price)
summary(bd$price)

# Boxplot da distribuição do preço
boxplot(bd$price, main = "Preço das casas em Praga, em coroas checas")

# Histograma da distribuição do preço (não logaritmizado)
hist(bd$price, main ="Histograma da distribuição do preço, não logaritmizado")

# Histograma da distribuição do preço logaritmizado
hist(log(bd$price), col = cores_pasteis, main = "Histograma da distribuição do Preço (logaritmizado)")

# Verificar a presença de NAs
sum(is.na(bd$price))
sum(bd$price=='')
sum(bd$price==' ')
sum(bd$price=='NA')

# Observação dos 6 outliers que mais se destacam na variável
head(sort(boxplot(bd$price,plot=FALSE)$out,
          decreasing =TRUE))

#Observação dos 4 mais influenciadores
bd[c(5968,8032,8298,8343),]

#A maior parte dos valores estão nos 1727-3864, depois há uma série de outliers 
#muito distantes



################################################################################
# Variável "minimum_nights"  
################################################################################

# Sumário da Variável
describe(bd$minimum_nights)
summary(bd$minimum_nights)

# Boxplot do mínimo de noites que tem que se arrendar
boxplot(bd$minimum_nights, main=  "Numero mínimo de noites possível arrendar, 
        em dias")


# Verificação de Omissos
sum(is.na(bd$minimum_nights))
sum(bd$minimum_nights=='')
sum(bd$minimum_nights==' ')
sum(bd$minimum_nights=='NA')


#A maior parte dos valores varia entre 1-2 noites, 
#no entanto tem alguns outliers que, tendo em conta o contexto, podem ser erros.



################################################################################
# Variável "number_of_reviews"  
################################################################################

# Sumário da Variável
describe(bd$number_of_reviews)
summary(bd$number_of_reviews)

# Boxplot do número de reviews feitas ao anúncio
boxplot(bd$number_of_reviews, xlab = "Boxplot das Avaliações feitas ao anúncio")

# Verificação de Valores omissos
sum(is.na(bd$number_of_reviews))
sum(bd$number_of_reviews=='')
sum(bd$number_of_reviews==' ')
sum(bd$number_of_reviews=='NA')



################################################################################
# Variável "last_review"  
################################################################################

# Sumário da Variável
summary(bd$last_review)

# Tabela que mostra as 6 datas com maior frequância absoluta
head(sort(table(bd$last_review), decreasing = TRUE))

# Tabela de frequências absolutas
freq_abs <- table(bd$last_review)

#Top 6 observações por ordem decrescente 
top_reviews <- head(sort(freq_abs, decreasing = TRUE), 6)

# Frequência relativa total
freq_rel_total <- prop.table(freq_abs) * 100

# Frequência relativa das top 6 observações
freq_rel_top6 <- prop.table(top_reviews) * 100

# Data frame com os 6 valores com maior frequência absoluta, 
#e as respectivas frequências relativas face ao total de observações e face ao top 
df_reviews <- data.frame(
  Datas = names(top_reviews),
  Frequência_Absoluta = as.numeric(top_reviews),
  Frequência_Relativa_Total = as.numeric(freq_rel_total[names(top_reviews)]),
  Frequência_Relativa_Top6 = as.numeric(freq_rel_top6)
)

# Visualização do Data frame
print(df_reviews)

# Número de datas diferentes
length(unique(bd$last_review))

# Verificação de Omissos
sum(is.na(bd$last_review))
sum(bd$last_review=='')
sum(bd$last_review==' ')
sum(bd$last_review=='NA')

# Nesta variável existe também muita variedade de valores, 959 valores distintos. 
#O valor com mais frequência é um valor omisso '', com frequência absoluta de 
#1027, o que corresponde a 10% dos valores totais.



################################################################################
# Variável "reviews_per_month"  
################################################################################

# Sumário da Variável
describe(bd$reviews_per_month)
summary(bd$reviews_per_month)

# Boxplot do número de reviews por mês
boxplot(bd$reviews_per_month, xlab = "Boxplot da Média de Avaliações por Mês")

# Verificação de Omissos
sum(is.na(bd$reviews_per_month))
sum(bd$reviews_per_month=='')
sum(bd$reviews_per_month==' ')
sum(bd$reviews_per_month=='NA')

#Destacam-se ligeiramente 3 outliers mais prominentes.



################################################################################
# Variável "calculated_host_listings_count"  
################################################################################

# Sumário da Variável
describe(bd$calculated_host_listings_count)
summary(bd$calculated_host_listings_count)

# Observar número de listings por ID
Host_list_count <- data.frame( bd$host_id, bd$host_name, 
                               bd$calculated_host_listings_count)

head(Host_list_count)

# Lista Ordenada
Host_list_count[order(Host_list_count$bd.calculated_host_listings_count, 
                      decreasing = TRUE), ]

# Tabela dos 10 hosts_id com maior número de listings
Host_list_count_10 <- head(unique(Host_list_count[order(
  Host_list_count$bd.calculated_host_listings_count, decreasing = TRUE), ]
),n = 10)

names(Host_list_count_10) <- c("Id do Host", "Nome do Host", 
                               "Número de listings") 

Host_list_count_10


# boxplot dos vários anúncios por host
boxplot(bd$calculated_host_listings_count, 
        main = "Boxplot do número de anúncios por host (unidades)")

# Gráfico de dispersão da Variável, sem a maioria dos Outliers
plot(bd$price,bd$calculated_host_listings_count, 
     main = "Gráfico de Dispersão do Preço consoante Anúncios por Host s/ outliers", 
     xlab = "Price", 
     ylab = "calculated_host_listings_count")

#Gráfico de dispersão da Variável, Excepto os 6 outliers maiores no preço 
#e calculated_host_listings_count

plot(bd$price[-c(5968,8032,8298,8343,4672,6522)],
     bd$calculated_host_listings_count[-c(5968,8032,8298,8343,4672, 6522)], 
     main = "Gráfico de Dispersão", 
     xlab = "Price", 
     ylab = "calculated_host_listings_count")


# Verificação de Omissos
sum(is.na(bd$calculated_host_listings_count))
sum(bd$calculated_host_listings_count=='')
sum(bd$calculated_host_listings_count==' ')
sum(bd$calculated_host_listings_count=='NA')


#Maior parte dos valores entre 1-20. Mais nada de especial a apontar



################################################################################
# Variável "availability_365"  
################################################################################

# Sumário da Variável
describe(bd$availability_365)
summary(bd$availability_365)

#data.frame entre o preço e a disponibilidade
host_price_availability <- data.frame( bd$host_id,bd$host_name, 
                                       bd$price, bd$minimum_nights, 
                                       bd$availability_365)
head(host_price_availability)

# Se a disponibilidade for 0
host_price_availability0 <- subset(
  host_price_availability, host_price_availability$bd.availability_365 == 0)

nrow(host_price_availability0)

# Número de observações onde o preço é NA e a disponibilidade é 0
length(which(is.na(host_price_availability0$bd.price)))


# data.frame Se o Preço for NA
host_priceNA_availability <- subset(
  host_price_availability, is.na(host_price_availability$bd.price ))

head(host_priceNA_availability)

# Se o Preço for NA e a disponibilidade para arrendar são diferentes de 0 dias
which(host_priceNA_availability$bd.availability_365 != 0)

# Observações onde a linha de código anterior se verifica
host_priceNA_availability[c( 31, 146, 511, 512, 514, 572, 575, 618, 645, 
                             671, 672, 679, 680),]

# Boxplot da Variável
boxplot(bd$availability_365, 
        main = "Boxplot da disponibilidade de arrendar a casa, em dias")

# Verificação de Omissos
sum(is.na(bd$availability_365))

sum(bd$availability_365=='')
sum(bd$availability_365==' ')
sum(bd$availability_365=='NA')


#Dos 9388 dados, existem 2388 dos quais na sua recolha já teriam sido reservados
#por um utilizador, ou então bloqueados pelo host

#Dos 2388, 667 têm o preço como NA

#Existem anúncios onde existe disponibilidade para arrendar, no entanto
#não têm preço.



################################################################################
# Variável "number_of_reviews_ltm"  
################################################################################

# Sumário da Variável
describe(bd$number_of_reviews_ltm)
summary(bd$number_of_reviews_ltm)

# Dataframe com o IDs, host, e  reviews feitas nos últimos 12 meses
ID_H_name_n_revs <- data.frame(bd$id, bd$host_name, bd$number_of_reviews_ltm) 

head(ID_H_name_n_revs[order(ID_H_name_n_revs$bd.number_of_reviews_ltm, 
                            decreasing = TRUE),])

# Boxplot do número de reviews, feitas nos últimos 12 meses
boxplot(bd$number_of_reviews_ltm,
        main = "Número de reviews feitas a um anúncio no último ano (unidades)")

# Verificação de Omissos
sum(is.na(bd$number_of_reviews_ltm))
sum(bd$number_of_reviews_ltm=='')
sum(bd$number_of_reviews_ltm==' ')
sum(bd$number_of_reviews_ltm=='NA')

# O maior número de revisões feitas foi 295 no listing de ID 1.345710e+07 ,
#no host Hana

# Existem muitos Outliers



################################################################################
# Variável "license"  
################################################################################

# Outra variável apenas com valores omissos 

# Sumário da Variável
summary(bd$license)


################################################################################


# Limpeza dos dados (inicial)


################################################################################


################################################################################
# Variável "name": Processo de separação da string em diferentes variáveis
################################################################################


# Separação da variável "name" em múltiplas variáveis (elimina por defeito a 
#variável.) 

bd_name_treat <- separate(bd, name, into = c("type_of_establishment", 
                                             "rating", 
                                             "nr_bedrooms", 
                                             "nr_beds", 
                                             "nr_baths"), sep = "·")

# Foram criadas as variáveis:

# Variável "type_of_establishment": tipo específico de establecimento a arrendar.
# Variável "rating": classificação dada pelos utentes ao anúncio.
# Variável "nr_bedrooms": número de quartos que o anúncio disponibliza.
# Variável "nr_beds": número de camas que o anúncio disponibliza.
# Variável "nr_baths": número e tipo de casa de banho que o anúncio disponibliza



#Verificar elementos únicos nas variáveis
unique(bd_name_treat$type_of_establishment) 
unique(bd_name_treat$rating)
unique(bd_name_treat$nr_bedrooms)
unique(bd_name_treat$nr_beds)
unique(bd_name_treat$nr_baths)


#Existem alguns listings que não estão totalmente preenchidos. O que acontece 
#é que ao separar em colunas, quando há informação em falta, existem valores
#que acabam por ser preenchidos na coluna errada.
#É preciso alocar os valores para as variáveis corretas:


#Criação de uma nova cópia da base de dados para este tratamento específico
bd_name_treat_fix <- bd_name_treat

#Alocação dos valores corretos para a variável nr_beds
bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_beds)), ]$nr_baths <- bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_beds)), ]$nr_beds
bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_beds)), ]$nr_beds <- NA

#Alocação dos valores corretos para a variável nr_bedrooms
bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_baths <- bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms
bd_name_treat_fix[which(grepl("bed ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_beds <- bd_name_treat_fix[which(grepl("bed ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms
bd_name_treat_fix[which(grepl(" 1 bed", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_beds <- bd_name_treat_fix[which(grepl(" 1 bed", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms
bd_name_treat_fix[which(grepl("beds ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_beds <- bd_name_treat_fix[which(grepl("beds ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms
bd_name_treat_fix[which(grepl("bath", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms <- NA
bd_name_treat_fix[which(grepl("bed ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms <- NA
bd_name_treat_fix[which(grepl(" 1 bed", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms <- NA
bd_name_treat_fix[which(grepl("beds ", bd_name_treat$nr_bedrooms) & !grepl("room", bd_name_treat$nr_bedrooms)), ]$nr_bedrooms <- NA

#Alocação dos valores corretos para a variável rating
bd_name_treat_fix[which(grepl("bedroom", bd_name_treat$rating)), ]$nr_bedrooms <- bd_name_treat_fix[which(grepl("bedroom", bd_name_treat$rating)), ]$rating
bd_name_treat_fix[which(grepl("Studio", bd_name_treat$rating)), ]$nr_bedrooms <- bd_name_treat_fix[which(grepl("Studio", bd_name_treat$rating)), ]$rating
bd_name_treat_fix[which(grepl("bed", bd_name_treat$rating) & !grepl("room", bd_name_treat$rating)), ]$nr_beds <- bd_name_treat_fix[which(grepl("bed", bd_name_treat$rating)& !grepl("room", bd_name_treat$rating)), ]$rating
bd_name_treat_fix[which(grepl("bedroom", bd_name_treat$rating)), ]$rating <- NA
bd_name_treat_fix[which(grepl("Studio", bd_name_treat$rating)), ]$rating <- NA
bd_name_treat_fix[which(grepl("bed", bd_name_treat$rating) & !grepl("room", bd_name_treat$rating)), ]$rating <- NA
bd_name_treat_fix[which(grepl("New", bd_name_treat$rating)), ]$rating <- NA

#Retirar a "Estrela" da variável rating
bd_name_treat_fix$rating <- as.numeric(gsub("★", "", bd_name_treat_fix$rating))

head(bd_name_treat_fix)



#///////////////////////////////////////////////////////////////////////////////

# Análise (Raw) dos novos campos:

#///////////////////////////////////////////////////////////////////////////////


#///////////////////////////////////////////////////////////////////////////////
# Variável "type_of_establishment" 
#///////////////////////////////////////////////////////////////////////////////

# Sumário da Variável
summary(bd_name_treat_fix$type_of_establishment)

# Tipos de establecimentos.
length(unique(bd_name_treat_fix$type_of_establishment))

# Tipos de establecimentos com maiores frequências absolutas
head( sort(table(bd_name_treat_fix$type_of_establishment), 
           decreasing = TRUE))

# Gráfico de barras da tabela anterior
barplot(head( sort(table(bd_name_treat_fix$type_of_establishment), 
                   decreasing = TRUE)), cex.names = 0.7, 
        main = "Top 6 tipos de quartos classificados pelo host", space = 0.5)

# Verificação de Omissos
sum(is.na(bd_name_treat_fix$type_of_establishment))
sum(bd_name_treat_fix$type_of_establishment=='')
sum(bd_name_treat_fix$type_of_establishment==' ')
sum(bd_name_treat_fix$type_of_establishment=='NA')



#///////////////////////////////////////////////////////////////////////////////
# Variável "rating"
#///////////////////////////////////////////////////////////////////////////////

# Sumário da Variável
summary(bd_name_treat_fix$rating)
describe(bd_name_treat_fix$rating)

# Tipos de Ratings dados pelos utilizadores 
unique(bd_name_treat_fix$rating)

# Boxplot da variável
boxplot(bd_name_treat_fix$rating, 
        main="Boxplot sobre o rating médio dados aos quartos, em estrelas")


# Verificação de Omissos
sum(is.na(bd_name_treat_fix$rating))
sum(bd_name_treat_fix$rating=='')
sum(bd_name_treat_fix$rating==' ')
sum(bd_name_treat_fix$rating=='NA')



#///////////////////////////////////////////////////////////////////////////////
# Variável "nr_bedrooms"
#///////////////////////////////////////////////////////////////////////////////

# Sumário da Variável
summary(bd_name_treat_fix$nr_bedrooms)

# Tipos de números de Quartos
unique(bd_name_treat_fix$nr_bedrooms)

#Tabela das 6 maiores frequências absolutas do número de camas, estipuladas 
#no anúncio

head(sort(table(bd_name_treat_fix$nr_bedrooms), decreasing = TRUE))

# Gráfico de barras da tabela anterior
barplot(head( sort(table(bd_name_treat_fix$nr_bedrooms), decreasing = TRUE)), 
        cex.names = 0.7, main = "Número de Número, classificados pelo host", 
        space = 0.5)

#Verificação de Omissos
sum(is.na(bd_name_treat_fix$nr_bedrooms))
sum(bd_name_treat_fix$nr_bedrooms=='')
sum(bd_name_treat_fix$nr_bedrooms==' ')
sum(bd_name_treat_fix$nr_bedrooms=='NA')


#///////////////////////////////////////////////////////////////////////////////
# Variável "nr_beds"
#///////////////////////////////////////////////////////////////////////////////

# Sumário da Varável
summary(bd_name_treat_fix$nr_beds)

# Tipos de classificação do número de camas
unique(bd_name_treat_fix$nr_beds)

# Tabela das 6 maiores frequências absolutas sobre o número de camas
head(sort(table(bd_name_treat_fix$nr_beds), decreasing = TRUE))

# Gráfico de Barras sobre o número de camas
barplot(head( sort(table(bd_name_treat_fix$nr_beds), 
                   decreasing = TRUE)), cex.names = 0.7, 
        main = "Top 6 Número de camas, classificados pelo host", 
        space = 0.5)

#Verificação de Omissos
sum(is.na(bd_name_treat_fix$nr_beds))
sum(bd_name_treat_fix$nr_beds=='')
sum(bd_name_treat_fix$nr_beds==' ')
sum(bd_name_treat_fix$nr_beds=='NA')



#///////////////////////////////////////////////////////////////////////////////
# Variável "nr_baths"
#///////////////////////////////////////////////////////////////////////////////

# Sumário da Variável
summary(bd_name_treat_fix$nr_baths)

# Elementos únicos na variável
unique(bd_name_treat_fix$nr_baths)

# Tabela das 6 classificações de casas de banho com maiores 
#frequências absolutas

head(sort(table(bd_name_treat_fix$nr_baths), decreasing = TRUE))

# Gráfico de barras sobre a tabela anterior
barplot(head( sort(table(bd_name_treat_fix$nr_baths), decreasing = TRUE)),
        cex.names = 0.7,
        main = "Top 6 Quantidade de casas de banho, classificados pelo host", 
        space = 0.5)

#Verificação de Omissos
sum(is.na(bd_name_treat_fix$nr_baths))
sum(bd_name_treat_fix$nr_baths=='')
sum(bd_name_treat_fix$nr_baths==' ')
sum(bd_name_treat_fix$nr_baths=='NA')




################################################################################
# Variáveis "neighbourhood_group" e "licence": Remover colunas da base de dados
################################################################################

#Remover Colunas licence e neighbourhood_group (variáveis apenas com NAs)
bd_no_license_neig_group <- subset(bd_name_treat_fix, select=-c(
  neighbourhood_group, license))

#Base de dados para a restante limpeza dos dados
bd_cleaning <- bd_no_license_neig_group



################################################################################
# Variável room_type: Pôr letras em lower_case
################################################################################

#re-escrever todas as instâncias da variável room_type para lowercase
bd_cleaning$room_type <- tolower(bd_cleaning$room_type)



################################################################################
# Variável nr_baths: Uniformização das "half-baths" e remoção de espaços
################################################################################

# Uniformização das "half-baths"
bd_cleaning$nr_baths <- gsub("half-bath", "0.5", bd_cleaning$nr_baths)

# Remoção da palavra "bath"
bd_cleaning$nr_baths <- gsub("baths*\\b", "", bd_cleaning$nr_baths)



################################################################################
# Variável nr_beds: Remoção da palavra "beds" e mudança do tipo para númerico
################################################################################

# Remoção da palavra "beds"
bd_cleaning$nr_beds <- gsub("beds?\\b", "", bd_cleaning$nr_beds)

# Transformação da variável para tipo numérico
bd_cleaning$nr_beds <- as.numeric(bd_cleaning$nr_beds)



################################################################################
# Variável nr_bedrooms: Remoção da palavra "bedroom" e mudança do tipo para númerico
################################################################################

# Remoção da palavra "bedroom"
bd_cleaning$nr_bedrooms<-gsub("bedrooms*\\b","",bd_cleaning$nr_bedrooms)

# Passagem das observações de "Studio" para 0 (Pois todas as observações para 
#estúdio tinham o número de quartos em falta, e então decidimos convencionar
#que se uma observação é estúdio não tem quartos.)
bd_cleaning$nr_bedrooms<-gsub("Studio",0,bd_cleaning$nr_bedrooms)

# Passagem da variável de string para numérico
bd_cleaning$nr_bedrooms<-as.numeric(bd_cleaning$nr_bedrooms)



################################################################################
# Variável number_of_reviews: Passagem de valores omissos para 0
################################################################################

# Subsituição de valores NA para 0
bd_cleaning$reviews_per_month[is.na(bd_cleaning$reviews_per_month)] <- 0


################################################################################
# Variável type_of_establishment: Retirar apenas o tipo de casa e aumentar
# a letra "pension" e "casa particular"
################################################################################

# Remoção da localização do anúncio nesta variável (no que está depois o "in")

# Exemplo
bd_cleaning$type_of_establishment[1]

# Código que faz a remoção
bd_cleaning$type_of_establishment <- sapply(
  strsplit(as.character(
    bd_cleaning$type_of_establishment), " in "), function(x) x[1])

# Resultado final
bd_cleaning$type_of_establishment[1]


# Alteração de "pension" para "Pension"
bd_cleaning$type_of_establishment[
  bd_cleaning$type_of_establishment == "pension"] <- "Pension"


#  Alteração de "casa particular" para "Casa Particular"
bd_cleaning$type_of_establishment[
  bd_cleaning$type_of_establishment == "casa particular"] <- "Casa particular"


################################################################################

#  Criação de novas variáveis: "Attractiveness" e "Attractiveness_ltm"

################################################################################



################################################################################
# Variável "attractiveness"
################################################################################

# Cálculo dos limites dos percentis
percentile_limits <- quantile(bd_cleaning$number_of_reviews, 
                              probs = seq(0, 1, by = 0.2))

# Criação de nova variável 'attractiveness' com base nos percentis
bd_cleaning <- bd_cleaning %>%
  mutate(attractiveness = case_when(
    number_of_reviews <= percentile_limits[2] ~ round(1 + (number_of_reviews - min(number_of_reviews)) / (percentile_limits[2] - min(number_of_reviews)) * 0.99, 1),
    number_of_reviews <= percentile_limits[3] ~ round(2 + (number_of_reviews - percentile_limits[2]) / (percentile_limits[3] - percentile_limits[2]) * 0.99, 1),
    number_of_reviews <= percentile_limits[4] ~ round(3 + (number_of_reviews - percentile_limits[3]) / (percentile_limits[4] - percentile_limits[3]) * 0.99, 1),
    number_of_reviews <= percentile_limits[5] ~ round(4 + (number_of_reviews - percentile_limits[4]) / (percentile_limits[5] - percentile_limits[4]) * 0.99, 1),
    TRUE ~ 5.0
  ))

# Verificação o resultado
head(bd_cleaning)


# Aplicação da transformação logarítmica e rescaling
bd_cleaning <- bd_cleaning %>%
  mutate(
    attractiveness_log = log1p(number_of_reviews), # Transformação logarítmica
    attractiveness = rescale(attractiveness_log, to = c(1, 5), from = range(
      attractiveness_log, na.rm = TRUE)) # Mudança da escala para valores entre 1 e 5
  )


################################################################################
# Variável "attractiveness_ltm"
################################################################################

# Cálculo dos limites dos percentis_ltm
percentile_limits_ltm <- quantile(bd_cleaning$number_of_reviews_ltm, probs = seq(0, 1, by = 0.2))

# Criação de nova variável 'attractiveness_ltm' com base nos percentis
bd_cleaning <- bd_cleaning %>%
  mutate(attractiveness_ltm = case_when(
    number_of_reviews_ltm <= percentile_limits_ltm[2] ~ round(1 + (number_of_reviews_ltm - min(number_of_reviews_ltm)) / (percentile_limits_ltm[2] - min(number_of_reviews_ltm)) * 0.99, 1),
    number_of_reviews_ltm <= percentile_limits_ltm[3] ~ round(2 + (number_of_reviews_ltm - percentile_limits_ltm[2]) / (percentile_limits_ltm[3] - percentile_limits_ltm[2]) * 0.99, 1),
    number_of_reviews_ltm <= percentile_limits_ltm[4] ~ round(3 + (number_of_reviews_ltm - percentile_limits_ltm[3]) / (percentile_limits_ltm[4] - percentile_limits_ltm[3]) * 0.99, 1),
    number_of_reviews_ltm <= percentile_limits_ltm[5] ~ round(4 + (number_of_reviews_ltm - percentile_limits_ltm[4]) / (percentile_limits_ltm[5] - percentile_limits_ltm[4]) * 0.99, 1),
    TRUE ~ 5.0
  ))

# Verificação o resultado
head(bd_cleaning)

# existem NaN's na variável, pois não se pode logaritmizar zero
unique(bd_cleaning$attractiveness_ltm)

#substituir os NaN's por 2 (dado que o valor mais baixo de attractivness é 2.3,
#assim este valor mantem-se consistente com o resto da escala)
bd_cleaning[which(is.na(bd_cleaning$attractiveness_ltm)),]$attractiveness_ltm <- 2.0

################################################################################

#Novas análises e conclusões

################################################################################

#Base de dados para a análises
bd_analise <- bd_cleaning



################################################################################
# Análise do preço por tipo de establecimento
################################################################################

#boxplot dos preços por categorias de uma variável categórica
ggplot(bd_analise, aes(x = type_of_establishment, y = price)) +
  geom_boxplot(outlier.shape = NA) + #removes outliers
  labs(title = "Prices per establishment",
       x = "establishment",
       y = "price") + coord_flip() +  ylim(0, 50000)

#Criação de um dataframe apenas com id, tipo de establecimento e preço
price_per_type <- subset(bd_analise, select = c("id", "type_of_establishment", "price") )

price_per_type <- subset(price_per_type, !(is.na(price_per_type$price)))

#Tabela do preço médio por tipo de establecimento
result <- price_per_type %>%
  group_by(type_of_establishment) %>%
  summarise(Total_price = mean(price))

#Ver resultados
print(result, n = length(unique(price_per_type$type_of_establishment)))

#Podemos observar através do boxplot e da tabela criada que o preço médio de casa
# por tipo varia entre 2000 - 10000 coroas checas, salvo algumas exceções como
# na Dome, Tower, Villa e Boutique Hotel, sendo o valor médio maior em Tower com
# 19353 coroas checas.



################################################################################

#  Omissão dos valores omissos

################################################################################

################################################################################
# Variável "Price": Eliminação de linhas omissas
################################################################################

#Criação da base de dados que irá omitir as linhas que têm preço como NA
#(não podem existir valores omissos na variável dependente)
bd_omit<- bd_cleaning[!is.na(bd_cleaning$price),]

#São excluídas 680 observações da base de dados
nrow(bd_omit)



################################################################################


# Pré-processamento de dados e manipulação de features 
#(agrupar, juntar, eliminar, transformar as variáveis)


################################################################################

################################################################################
#  Agrupamento da variável "type_of_establishment": Agrupar os diferentes tipos
#   de casas em apenas 4 tipos: "Budget", "Quality", "Fancy", "Rich"
################################################################################

#Criação de base de dados para o agrupamento desta variável

bd_type_group <- bd_omit

#Criação de um novo dataframe apenas com o type_of_establishment e o preço
price_toe <- bd_type_group[c(1,13)]
price_toe$type_of_establishment <- factor(bd_type_group$type_of_establishment, 
                                          levels = unique(bd_type_group$type_of_establishment))
nrow(price_toe)


#Gráfico para visualizar a distribuição de preços através de boxplots por cada 
#type_of_establishment

ggplot(price_toe, aes(x = type_of_establishment, y = price)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Prices per type of establishment",
       x = "type of establishment",
       y = "price") +
  ylim(0, 50000)  + coord_flip()
#Existem 27 obs acima de 50000 coroas checas


#summary do preço por cada type_of_establishment para melhor 
#compreender a distribuição
for (i in unique(price_toe$type_of_establishment)){
  print(i)
  print(length(price_toe$price[which(price_toe$type_of_establishment == i)]))
  print(summary(price_toe$price[which(price_toe$type_of_establishment == i)]))
  
}

#Criação das 4 categorias diferentes de establishments consoante a análise feita,
#em que se agruparam os establecimentos de acordo com a semelhança da 
#distribuições dos seus preços.
budget_establishment <- c("Rental unit", "Home", "Hostel", "Cabin",
                          "Tiny home", "Condo", "Guesthouse", "Townhouse",
                          "Guest suite", "Camper/RV", "Vacation home",
                          "Farm stay", "Tent", "Cave", "Casa particular",
                          "Cottage")
quality_establishment <- c("Hotel", "Aparthotel", "Loft", "Serviced apartment", 
                           "Bed and breakfast", "Houseboat", "Bungalow",
                           "Yurt", "Pension", "Hut")
fancy_establishment <- c("Boutique hotel", "Boat", "Place to stay",
                         "Castle")

rich_establishment <- c("Villa", "Dome", "Tower")

#Adição desta nova variável categórica ao dataframe (establishment_group)
bd_type_group$establishment_group <- "000"
for (i in 1:nrow(bd_type_group)){
  if (bd_type_group[i,2] %in% budget_establishment){
    bd_type_group[i,24] <- "Budget"
  }
  else if (bd_type_group[i,2] %in% quality_establishment){
    bd_type_group[i,24] <- "Quality"
  }
  else if (bd_type_group[i,2] %in% fancy_establishment){
    bd_type_group[i,24] <- "Fancy"
  }
  else if (bd_type_group[i,2] %in% rich_establishment){
    bd_type_group[i,24] <- "Rich"
  }
}

#Tabela de frequências absolutas
table(bd_type_group$establishment_group)

#Gráfico para visualizar a distribuição de preços através de boxplots por 
# cada type_of_establishment
ggplot(bd_type_group, aes(x = type_of_establishment, y = price)) +
  geom_point(aes(x = establishment_group)) +
  labs(title = "Preços por tipo de establecimento",
       x = "Tipo de establecimento",
       y = "Preço") +
  ylim(0, 50000)  

#Modelo anova: Relação entre o preço e os tipos de establecimento agrupados
anova_lm_eg <- aov(log(price)~establishment_group, data=bd_type_group)
summary(anova_lm_eg) # O facto do p-value do anova test ser praticamente zero
#significa que este agrupamento das categorias da váriavel establishment têm
#distribuições significativamente distintas do preço.


################################################################################
# Agrupamento da variável "neighbourhood": Agrupar os diferentes tipos
#   de casas em 3 tipos, de acordo com a sua proximidade do centro da 
#   cidade: "close", "mid-range", "far"
################################################################################

#Criação de base de dados para o agrupamento desta variável
bd_neigh_group <- bd_type_group

# Ver qual o centro do municipio praha1 (Praha 1 é o "centro" da cidade e 
# o distrito minucipal com maior frequência absoluta)
centro_praha1 <- c(mean(
  bd_neigh_group$latitude[which(
    bd_neigh_group$neighbourhood == "Praha 1")
  ]
), mean(
  bd_neigh_group$longitude[which(
    bd_neigh_group$neighbourhood == "Praha 1")
  ]
)
)

#Centro da cidade
centro_praha1

#ver qual hotel está mais longe
distances <- sqrt(
  (bd_neigh_group$latitude - centro_praha1[1])^2 + (
    bd_neigh_group$longitude - centro_praha1[2])^2)


distances

#criação da nova variável,consoante a distância ao centro
bd_neigh_group$distance_category <- "000"

# Atribuição das categorias
bd_neigh_group[which(distances<max(distances)/25),]$distance_category <- "close"

bd_neigh_group[
  which(
    max(
      distances)/25<=distances & distances<max(
        distances)/10),]$distance_category <- "mid-range"

bd_neigh_group[which(distances>=max(distances)/10),]$distance_category <- "far"

#Tabela de frequências absolutas
table(bd_neigh_group$distance_category)

# Scatterplot 
ggplot(bd_neigh_group, aes(x = neighbourhood, y = price)) +
  geom_point(aes(x = distance_category)) +
  labs(title = "Preços por distância ao centro",
       x = "Distância ao Centro",
       y = "preço") +
  ylim(0, 50000)


#Modelo anova. Mostra a diferença entre as distribuições do preço face à variável criada.
anova_lm1 <- aov(log(price)~distance_category, data=bd_neigh_group)
summary(anova_lm1) #tal como anteriormente, o p-value é praticamente zero, o que
#significa que existe uma distinção entre as variás distribuições de preços para
#cada categoria da variável criada.


################################################################################
# Nova Variável "last_review_days"
################################################################################

#transformação da variável last_review
bd_neigh_group$last_review <- as.Date(bd_neigh_group$last_review)
bd_neigh_group$last_review_days <- abs(as.numeric(bd_neigh_group$last_review-ymd("2023-12-31")))
summary(aov(log(price)~last_review_days,data=bd_neigh_group))

#Sumário da variável
summary(bd_neigh_group$last_review_days)



################################################################################
# Variável "Price": logaritmização
################################################################################

#Criação da base de dados para remover os outliers do preço  
bd_price_group <- bd_neigh_group

#logaritmização do preço
bd_price_group$pricelog <- log(bd_price_group$price)



################################################################################
# 
# Criação de Varáveis dummy
#
################################################################################

#Base de dados para a criação de variáveis dummy
bd_dummies <- bd_price_group 


################################################################################
# Variável "nr_baths": Criação de dummies para "shared" e "private"
################################################################################

#Criação das variáveis dummy "private" e "shared". Se não for nem private ou 
# shared, então ambas dummies serão 0

bd_dummies$bath_private <- ifelse(grepl("private", bd_dummies$nr_baths),1, 0)
bd_dummies$bath_shared <- ifelse(grepl("shared", bd_dummies$nr_baths),1, 0)

#Eliminação das palavras "private" e "shared" e conversão da variável para 
# numérico

bd_dummies$nr_baths <- gsub("private|shared", "", bd_dummies$nr_baths)
bd_dummies$nr_baths <- as.numeric(bd_dummies$nr_baths)


################################################################################
# Variáveis "room_type", "establishment_group", "distance_category": 
#  Criação de dummies, excluíndo a que tem maior frequência absoluta.
################################################################################

#Criação de dummies
bd_dummies <-dummy_cols(
  bd_dummies, select_columns =  c(
    'room_type', "establishment_group","distance_category"),   
  remove_selected_columns = FALSE, remove_most_frequent_dummy = TRUE) 

#Ajustar nomes
names(bd_dummies)[c(30, 31, 32)] <- c(
  "room_type_hotel_room","room_type_private_room","room_type_shared_room")
names(bd_dummies)

#Foram criadas:
# - "Room_type": "room_type_shared_room", "room_type_hotel_room", 
#   "room_type_private_room". Foi eliminada a coluna "entire home/apt"
# 
# - "establishment_group: "establishment_group_Fancy" ,
#   "establishment_group_Quality" e "establishment_group_Rich". Foi eliminada a 
#   coluna "budget"
#
# - "distance_category": "distance_category_close" e "distance_category_far".
#    Foi eliminada a coluna "mid-range"



################################################################################

# Imputação de valores omissos 

################################################################################

#Criação da base de dados para a imputação
bd_imputada <- bd_dummies

names(bd_imputada)

#Normalização 0-1
normalise <- function(x){
  return ((x-min(x))/max(x)-min(x))
}

#Observação das variáveis com describe
describe(normalise(na.omit(subset(
  bd_imputada, select=c('nr_baths','nr_beds','nr_bedrooms','rating')))))

#nenhuma destas variáveis tem distribuição normal, mesmo sem outliers 
# (observa-se pelos boxplots) então não optámos por métodos que tenham 
# como pressuposto uma distribuição normal. 
names(bd_imputada)

#Escolher variáveis como preditoras para aquelas que têm NAs
bd_imputada2 <- bd_imputada[,-c(
  1, 2,7, 8,9, 10, 11, 16,22,23,27,28,29,30,31, 32, 33,34, 35,36,37 )]

#Nomes das variáveis escolhidas
names(bd_imputada2)

#Mudar as variáveis "room_type", "establishment_group" e "distance_category"
#Para Factor (para poderem ser usadas neste método de imputação)
bd_imputada2 <- bd_imputada2 %>%
  mutate(room_type = as.factor(room_type)) %>% 
  mutate(establishment_group = as.factor(establishment_group)) %>% 
  mutate(distance_category = as.factor(distance_category))

#imputação de dados nas variáveis nr_beds e nr_bedrooms com mice()
dados_imputados <- mice(bd_imputada2, maxit = 0)

#Podemos observar que as primeiras 4 variáveis são aquelas que queremos imputar
# "nr_beds", "nr_baths", "nr_bedrooms", "rating"


#Sumário antes da imputação
summary(dados_imputados)

#Definição dos métodos de imputação para as variáveis a que queremos fazer
# imputação. As restantes não serão imputadas, mas serão usadas como 
# preditores para a execução da imputação (aquelas = a "")
meth <- dados_imputados$method

meth[c("nr_baths")]="rf"
meth[c("rating")]="rf"
meth[c("nr_bedrooms")]="pmm"
meth[c("nr_beds")]="pmm"
meth[c("room_type")]=""
meth[c("price")]=""
meth[c("minimum_nights")]=""
meth[c("number_of_reviews")]=""
meth[c("reviews_per_month")]=""
meth[c("calculated_host_listings_count")]=""
meth[c("availability_365")]=""
meth[c("number_of_reviews_ltm")]=""
meth[c("attractiveness")]=""
meth[c("establishment_group")]=""
meth[c("distance_category")]=""
meth[c("last_review_days")]="rf"

#As variáveis "nr_bedrooms" e "nr_beds" têm uma percentagem de valores omissos 
# muito baixa. Por isso optámos pelo método pmm (Predictive Mean Matching), 
# que tem em conta as relações multivariadas entre os dados.

#As variáveis "rating", "nr_baths" e "last_review_days" têm uma grande percentagem de valores 
# omissos, por isso optámos pelo método "rf", que é mais robusto e bom em dados
# com distribuições complexas ou com alta dimensionalidade.


#Imputação dos dados (com multiple imputations = 5)
dados_imputados <- mice(bd_imputada2, method = meth, seed = 7)

# Sumário após a imputação
summary(dados_imputados)

# Função que completa as variáveis com NAs, pelos valores estimados
dados_imputados <- complete(dados_imputados,1)


#nova bd com os dados imputados 
bd_imputada <- cbind(dados_imputados[,c(1, 2, 3, 4,16)],bd_imputada)

names(bd_imputada)

#Verificar novas imputações na variável nr_baths
ver_bath <- bd_imputada[which(is.na(bd_imputada[, c(11)]) ), c(11, 4)]
names(ver_bath) <- c("nr_baths_Antiga", "nr_baths_Imputada")

ver_bath

#Verificar novas imputações na variável rating
ver_rating <- bd_imputada[which(is.na(bd_imputada[, c(8)]) ), c(8, 1)]
names(ver_rating) <- c("rating_Antiga", "rating_Imputada")

ver_rating


#Verificar novas imputações na variável nr_bedrooms
ver_brooms <- bd_imputada[which(is.na(bd_imputada[, c(9)]) ), c(9, 2)]
names(ver_brooms) <- c("nr_bedrooms_Antiga", "nr_bedrooms_Imputada")

ver_brooms


#Verificar novas imputações na variável nr_beds
ver_beds <- bd_imputada[which(is.na(bd_imputada[, c(10)]) ), c(10, 3)]
names(ver_beds) <- c("nr_beds_Antiga", "nr_beds_Imputada")

ver_beds

#verificar novas imputações na variável last_review_days
ver_reviews <- bd_imputada[which(is.na(bd_imputada[, c(31)]) ), c(31, 5)]
names(ver_reviews) <- c("last_review_days_Antiga", "last_review_days_Imputada")

ver_reviews

#Remover Variáveis antigas com NAs
bd_imputada <- bd_imputada[,-c(8,9,10,11,31)]


names(bd_imputada)



################################################################################


# Correlações


################################################################################

#Seleção de variáveis candidatas a preditoras
bd_pred <- bd_imputada[,-c(6, 8, 9, 11, 12, 17 )]

# Foram removidas à partida as variáveis "id", "host_id", host_name", "latitude", 
#  "longitude" e "last_review"

#Nomes das variáveis retidas
names(bd_pred)

# Seleção das variáveis quantitativas para correlações lineares (sem dummies) 
# (com preço como Variável Alvo, logaritmizado)

bd_lin_cor <- subset(bd_pred, select = c(
  'pricelog', 'nr_baths', 'rating', 'nr_beds',
  'minimum_nights', 'number_of_reviews','calculated_host_listings_count',
  'availability_365', 'number_of_reviews_ltm',
  'attractiveness', 'attractiveness_log','last_review_days')) 


# Correlação de Pearson
lin_cor <- round(cor(bd_lin_cor) ,3)

#Gráfico de correlações de Pearson
corrplot(lin_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.9 )


#Correlação de Spearman
sp_cor <-  round(cor(bd_lin_cor, method = "spearman") ,3)

#Gráfico de correlações de Spearman
corrplot(sp_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.9  )


# Medida de associação eta para as variáveis qualitativas "type_of_establishment"
#"neighbourhood", "room_type", "establishment_group", "distance_category"

for (i in c( 6, 7, 8, 19, 20)) { 
  anova_ <- aov( pricelog ~ bd_pred[,i] , bd_pred)
  print(sqrt(etaSquared(anova_ )[,1])) 
} 

#data.frame das correlações com pricelog
nom_cor <- data.frame(Variáveis = colnames(bd_pred[, c(6, 7, 8, 19, 20)]) , 
                      Eta = c(0.2366167, 0.2743257, 0.3294916, 0.1960783, 
                              0.2298101))

#Eta por variável
nom_cor

#Correlações de spearman com as variáveis dummy

bd_dummy_lin_cor <- subset(bd_pred, select = c(
  'pricelog', 'bath_private', 'bath_shared', 'room_type_hotel_room',
  'room_type_private_room', 'room_type_shared_room',
  'establishment_group_Fancy',
  'establishment_group_Quality', 'establishment_group_Rich',
  'distance_category_close', 'distance_category_far'))

#Correlação de spearman nas variáveis dummy
sp_dummy_cor <-  round(cor(bd_dummy_lin_cor, method = "spearman") ,3)


#Gráfico de correlações de spearman
corrplot(sp_dummy_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.9  )


#Correlação de pearson nas variáveis dummy
P_dummy_cor <-  round(cor(bd_dummy_lin_cor, method = "pearson") ,3)


#Gráfico de correlações de pearson
corrplot(P_dummy_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.9  )


#Medidas de correlação Pearson entre as dummies e as vars. numéricas
P_dummy_num <- round(cor(bd_dummy_lin_cor, bd_lin_cor, method = "pearson") ,3)


#Gráfico de correlações de pearson
corrplot(P_dummy_num, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.9)


################################################################################


# Uso algoritmos de aprendizagem supervisionada sobre o conjunto de dados


################################################################################


################################################################################
# Modelo com todas as variáveis numéricas
################################################################################

#dataframe com todas as variáveis
bd_pred

#dataframe com todas as variáveis númericas 
bd_model <- bd_pred[c(
  1,2,3,4,5,10,11,12,13,14,15,16,21,22,23,24,25,26,27,28,29,30,31)]

#Modelo da escolha de variáveis númericas exclusivamente
model_all <- lm(pricelog~., data= bd_model)

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_all)
par(mfrow=c(1,1))

# Sumário do modelo
summary(model_all) #rsquared= 0.3766, rse = 0.5834 

#Testes dos pressupostos
mean(model_all$residuals) # a média dos errors igual a zero verifica-se
bptest(model_all) #nenhum dos presupostos se verificam
bgtest(model_all) #nenhum dos presupostos se verificam


################################################################################
# Modelos sem os influenciadores ( ao remover 20 ou todos os influenciadores)
################################################################################

#Teste de outliers (bonferroni)
outlierTest(model_all)

#Ver cooks distance, para determinar quais as observações que mais influenciam 
# individualmente o modelo
cooks_distances <- cooks.distance(model_all)

ordered_cooks_distances <- cooks_distances[order(
  cooks_distances, decreasing = TRUE)]

#ver influentials
influential <- as.numeric(names(sort(cooks_distances,decreasing = TRUE))[
  (sort(cooks_distances,decreasing = TRUE) > 4*mean(
    sort(cooks_distances,decreasing = TRUE), na.rm=T))])

influential #176 observações


#o dataframe sem 20 dos valores mais influenciantes
bd_model_outl_v1 <- bd_model[-c(influential[1:20]),] #nr 8688

#Modelo sem os 20 influenciadores mais influenciantes
model_outl_v1 <- lm(pricelog~., data= bd_model_outl_v1)

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_outl_v1)
par(mfrow=c(1,1))

#Sumário do modelo
summary(model_outl_v1) #r^2 0.4054, RSE 0.5653 

#Testes dos pressupostos
bptest(model_outl_v1) #falha
bgtest(model_outl_v1) #falha
  

#o dataframe sem todos os valores considerados influenciantes
bd_model_outl_v2 <- bd_model[-c(influential),] #nr 8532

#Modelo sem todos os valores considerados influenciantes
model_outl_v2 <- lm(pricelog~., data= bd_model_outl_v2)


#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_outl_v2)
par(mfrow=c(1,1))

#Sumário do Modelo
summary(model_outl_v2) #r^2 0.468, RSE 0.4976 

#Testes dos pressupostos
bptest(model_outl_v2) #falha
bgtest(model_outl_v2) #falha



################################################################################
# Modelo anterior, mas através de linearização de preditores
################################################################################

#verificar quais as variáveis númericas que têm natureza linear ou não
#e caso possam ser linearizadas com uma potência, fazer essa transformação
# através do powerTransform(): (Determina a potência que se deve de elevar a 
# variável para a incluír no modelo de regressão linear.)

#Apenas foi necessária a linearização de variáveis para as seguintes: 
# "rating", "last_review_days", "minimum_nights" e "calculated_host_listings_count"

#Sumário do powerTransform para a variável "rating"
summary(powerTransform(bd_model_outl_v2$rating))

#Comparação da correlação com e sem a linearização
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$rating)
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$rating^10.7)


#Sumário do powerTransform para a variável "last_review_days"
summary(powerTransform(bd_model_outl_v2$last_review_days))

#Comparação da correlação com e sem a linearização
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$last_review_days)
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$last_review_days^-0.36)


#Sumário do powerTransform para a variável "minimum_nights"
summary(powerTransform(bd_model_outl_v2$minimum_nights))

#Comparação da correlação com e sem a linearização
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$minimum_nights)
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$minimum_nights^-0.83)


#Sumário do powerTransform para a variável "calculated_host_listings_count"
summary(powerTransform(bd_model_outl_v2$calculated_host_listings_count))

#Comparação da correlação com e sem a linearização
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$calculated_host_listings_count)
cor(bd_model_outl_v2$pricelog, bd_model_outl_v2$calculated_host_listings_count^-0.08)

# Exemplo de Variável que não precisa de linearização
summary(powerTransform(bd_model_outl_v2$attractiveness))#não precisa de linearização

# Gráficos que observam se deverá de ser feita linearização nas variáveis
crPlots(model_outl_v2)

#Observar se existe autocorrelação entre as variáveis 
vif(model_outl_v2) #alguns valores elevados, mas nenhum que passe de 5

#criação do modelo com as variáveis linearizadas
model_linear <- lm(pricelog~I(rating^10.7)+nr_bedrooms+nr_beds+nr_baths+
                     I(last_review_days^-0.36)+I(minimum_nights^-0.83)+
                     number_of_reviews+ I(calculated_host_listings_count^-0.08)
                   +reviews_per_month+number_of_reviews+
                     availability_365+number_of_reviews_ltm+attractiveness+
                     bath_private+ bath_shared + room_type_hotel_room+
                     room_type_private_room+room_type_shared_room+
                     establishment_group_Fancy+establishment_group_Quality+
                     establishment_group_Rich+distance_category_close+
                     distance_category_far, data=bd_model_outl_v2)

#Sumário do Modelo
summary(model_linear) #r^2 0.4727, RSE 0.4954

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_linear)
par(mfrow=c(1,1))

#Testes dos pressupostos
mean(model_linear$residuals) #passa
bptest(model_linear) #falha
bgtest(model_linear) #falha



################################################################################
# Modelo anterior, mas removendo alguns preditores mais correlacionado entre si
################################################################################

#verificar a correlação entre variáveis para tentar decidir quais remover do modelo
par(mfrow=c(1,1))
bd_model_cor <- cor(bd_model_outl_v2)
corrplot(bd_model_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.5  )

###criação do modelo com as variáveis linearizadas, removendo algumas das mais 
# correlacionadas entre si
model_linear_v2 <- lm(pricelog~I(rating^10.7)+nr_bedrooms+nr_baths+ room_type_shared_room+
                     I(last_review_days^-0.36)+I(minimum_nights^-0.83)+
                     I(calculated_host_listings_count^-0.08)+
                     availability_365+bath_shared+
                     establishment_group_Fancy+establishment_group_Quality+
                     establishment_group_Rich+
                     distance_category_far, data=bd_model_outl_v2)


#Sumário do Modelo
summary(model_linear_v2) #r^2 0.4091, RSE 0.5241 


#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_linear_v2)
par(mfrow=c(1,1))

#Testes dos pressupostos
mean(model_linear_v2$residuals) #passa
bptest(model_linear_v2) #falha
bgtest(model_linear_v2) #falha

#comparar AICs
AIC(model_outl_v2, model_linear, model_linear_v2)
#de acordo com o AIC, o modelo mais recente é o pior, apesar 
# de reduzir o número de variáveis. 



################################################################################
# Modelo anterior, mas adicionando pesos (Método WLS: Weighted Least Squares)
################################################################################

#criar weights para tentar corrigir os presupostos
weight <- 1/((1:length(bd_model_outl_v2$pricelog))^0.5)

#weight <- 1/sqrt(predict(model_weights, data=bd_model_outl_v2)) 
model_weights <- lm(pricelog~I(rating^10.7)+nr_bedrooms+nr_baths+ room_type_shared_room+
                        I(last_review_days^-0.36)+I(minimum_nights^-0.83)+
                        I(calculated_host_listings_count^-0.08)+
                        availability_365+bath_shared+
                        establishment_group_Fancy+establishment_group_Quality+
                        establishment_group_Rich+
                        distance_category_far, data=bd_model_outl_v2,
                        weights=weight)

#Sumário do Modelo
summary(model_linear_v2) #r^2 0.4091, RSE 0.5241 

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_weights)
par(mfrow=c(1,1))

#Testes dos pressupostos
mean(model_linear_v2$residuals) #passa
bptest(model_linear_v2) # //
bgtest(model_linear_v2) #presupostos não se verificam



################################################################################
# Modelo anterior, sem os pesos, mas através da transformação da variável 
#  dependente
################################################################################

#função que sugere uma transformação da variável dependente para resolver 
# problemas de heterostacidade
spreadLevelPlot(model_linear_v2) #suggested power transformation: 0.007609978

#aplicar a transformação sugerida à variável dependente
suggested_power <- -0.08659598 
transformed_dependent_variable <- bd_model_outl_v2$pricelog^suggested_power
bd_model_const_var <- bd_model_outl_v2
bd_model_const_var$pricelog <- transformed_dependent_variable

#recriar um modelo para verificar se a variabilidade dos erros é constante
model_const_var <- lm(pricelog~I(rating^10.7)+nr_bedrooms+nr_baths+ room_type_shared_room+
                      I(last_review_days^-0.36)+I(minimum_nights^-0.83)+
                      I(calculated_host_listings_count^-0.08)+
                      availability_365+bath_shared+
                      establishment_group_Fancy+establishment_group_Quality+
                      establishment_group_Rich+
                      distance_category_far, data=bd_model_const_var)

#Sumário da Variável
summary(model_const_var) #residual standard error = 0.004749  e r^2 = 0.4211

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_const_var)
par(mfrow=c(1,1))

#Testes dos pressupostos
mean(model_const_var$residuals) #sim
bptest(model_const_var) #falha
bgtest(model_const_var) #falha



################################################################################
# Modelo anterior, mas removendo ainda mais variáveis correlacionadas entre si
#, e com pesos 
################################################################################

#Novo dataframe com as variáveis do modelo anterior, mais as linearizações 
#  feitas
bd_const_var_cor <- bd_model_const_var[c(1,2,4,18,5,6,9,10,13,15,19,20,21,23)]

#Linearizações realizadas anteriormente
bd_const_var_cor$rating <- bd_const_var_cor$rating^10.7
bd_const_var_cor$last_review_days <- bd_const_var_cor$last_review_days^-0.36
bd_const_var_cor$minimum_nights <- bd_const_var_cor$minimum_nights^-0.83
bd_const_var_cor$calculated_host_listings_count <- bd_const_var_cor$calculated_host_listings_count^-0.08

#correlações
test_cor <- cor(bd_const_var_cor)
corrplot(test_cor, method = "number", type = "upper", tl.col = "black", 
         tl.cex = 0.7, number.cex = 0.7  )

#remover calculated host_listings, establishment_group_rich
#nr_baths, room_type_shared_room, availability
bd_const_var_cor_v2 <- bd_const_var_cor[c(-7,-13,-3,-4,-8)]

#Modelo
model_const_var_v2 <- lm(pricelog~., data=bd_const_var_cor_v2, weights=weight)

#Sumário do Modelo
summary(model_const_var_v2) # R-quadrado = 0.3302, RSE = 0 (praticamente)


#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(model_const_var_v2)
par(mfrow=c(1,1))

#Testes de pressupostos que estão a falhar
bptest(model_const_var_v2) #falha
bgtest(model_const_var_v2) #falha



################################################################################
# Modelo com apenas 2 variáveis preditoras ("nr_bedrooms" e "distance_category_far")
  # mais correlacionadas com a Alvo e menos correlacionadas entre elas
################################################################################

#Modelo com apenas as duas variáveis mais correlacionadas com a variável 
#dependente e que aparentam ser ter menos correlação entre si
mod_2_var <- lm(pricelog~nr_bedrooms+distance_category_far, data=bd_const_var_cor)

#Sumário do Modelo
summary(mod_2_var) # RSE = 0.005568 , R^2 = 0.2032

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(mod_2_var)
par(mfrow=c(1,1))


#Testes dos pressupostos
bptest(mod_2_var) #passa no teste de variância constante (p-value = 0.3137)
bgtest(mod_2_var) #mas não no de autcorrelação dos erros
dwtest(mod_2_var) #não passa
jarque.bera.test(mod_2_var$residuals) #não passa



################################################################################
# Modelo anterior, mas com pesos
################################################################################

#exprimentar exatamente o mesmo, mas atribuindo pesos diferentes a cada observação
#weight <- 1/((1:length(bd_const_var_cor$pricelog))^0.5)
weight <- 1/sqrt(predict(mod_2_var, data=bd_const_var_cor)) 
mod_2_var_w <- lm(pricelog~nr_bedrooms+distance_category_far, 
                data=bd_const_var_cor, weights = weight)

#Sumário do Modelo
summary(mod_2_var_w) # RSE = 0.005822 , R^2 = 0.2034

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(mod_2_var_w)
par(mfrow=c(1,1))

#Teste dos pressupostos
bptest(mod_2_var_w) #passa no teste de variância constante
bgtest(mod_2_var_w) #mas não no de autcorrelação dos erros
#acaba por ser o mesmo, não vale a pena manter os weights


#tentar corrigir a autocorrelação dos erros, atribuindo um peso diferente a 
# cada observação baseando-nos nos resultados do modelo anterior
coeftest(mod_2_var_w, vcov = vcovHC(mod_2_var, type = "HC0"))

weight <- 1 / resid(lm(pricelog~nr_bedrooms+distance_category_far, 
                       data=bd_const_var_cor))^2
mod_2_var_w2 <- lm(pricelog~nr_bedrooms+distance_category_far,
           data=bd_const_var_cor, weights = weight)


#Sumário do Modelo
summary(mod_2_var_w2) 

#Plot do gráfico
options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2))
plot(mod_2_var_w2)
par(mfrow=c(1,1))

#Testes dos pressupostos
bptest(mod_2_var_w2) #não passa
bgtest(mod_2_var_w2) #não passa


################################################################################


# Criação de amostras de treino e teste


################################################################################

set.seed(333) #criação de uma seed para se poder recriar os resultados

train_prop <- 0.8 #proporção treino/teste
train_indices <- createDataPartition(bd_const_var_cor$pricelog, p = train_prop, list = FALSE)


#amostras de teste e treino
train_data <- bd_const_var_cor[train_indices, ]
test_data <- bd_const_var_cor[-train_indices, ]




################################################################################


# Validar o modelo escolhido e fazer a previsão da variável dependente/alvo
#  (sobre o conjunto de teste).


################################################################################

# Foi escolhido para recriação o modelo que melhor funcionou até ao momento, 
# com um r quadrado de 0.20, e com os presupostos da média e da variância 
# verificados

#criação do modelo com 10 divisões (folds)
k_folds <- 10
model_folds <- train(pricelog ~ nr_bedrooms+distance_category_far,
                     data = bd_const_var_cor, method = "lm",
                     trControl = trainControl(method = "cv", number = k_folds))

#Sumário do modelo
summary(model_folds) #R quadrado, 0.2032, RSE 0.005568 

#prever os valores da amostra de teste com o modelo criado
k_fold_prediction <- predict(model_folds, newdata = test_data[-5])

#calcular o r-quadrado do modelo face à amostra
SSres <- sum((test_data$pricelog - k_fold_prediction)^2)
SStot <- sum((test_data$pricelog - mean(test_data$pricelog))^2)
R_squared <- 1 - (SSres / SStot)
R_squared # r^2 = 0.218, que é semelhante ao valor dado pelo sumário do modelo


residuals <- test_data$pricelog - k_fold_prediction
#bptest(residuals ~ k_fold_prediction) 
#bgtest(residuals ~ k_fold_prediction) 
bptest(model_folds$finalModel) #verifica-se (p-value = 0.3137)
bgtest(model_folds$finalModel) #não se verifica

#NeweyWest(model_folds$finalModel, lag = NULL, prewhite = FALSE, verbose = FALSE)


#Gráfico
plot(k_fold_prediction[c(1:250)], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y", 
     main = "valores observados VS valores estimados")

lines(bd_const_var_cor$pricelog[c(1:250)], pch = 18, col = "blue", type = "b", lty = 2)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"),
       lty = 1:2, cex=0.8)






