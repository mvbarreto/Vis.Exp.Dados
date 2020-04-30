
# Leitura de dados brasileiros - COVID19

library(tidyverse)
library(viridis)

dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)


# Covid-19 na cidade de São Paulo

linhas <- which(dados$city=='São Paulo')
dados.SaoPaulo <- dados[linhas,]

plot(apply(as.matrix(dados.SaoPaulo$confirmed), 2, rev),
     col="red", type = "l", ylab = "Número de casos", xlab = "Dias a partir de 25/02",
     main = "Covid-19 na cidade de São Paulo")

# Covid-19 na cidade de Guarujá

linhas <- which(dados$city=='Guarujá')
dados.Guaruja <- dados[linhas,]

plot(apply(as.matrix(dados.Guaruja$confirmed), 2, rev),
     col="red", type = "l", ylab = "Número de casos", xlab = "Dias a partir de 31/03",
     main = "Covid-19 na cidade de Guarujá")


# Covid-19 no estado de São Paulo

linhas <- which(dados$state=='SP' & dados$place_type == "state")
dados.SaoPaulo <- dados[linhas,]


plot(apply(as.matrix(dados.SaoPaulo$confirmed), 2, rev),
     col="red", type = "l", ylab = "Número de casos", xlab = "Dias a partir de 25/02",
     main = "Covid-19 no estado de São Paulo")

# Covid-19 na cidade de São Carlos

linhas <- which(dados$city=='São Carlos')
dados.Sanca <- dados[linhas,]

plot(apply(as.matrix(dados.Sanca$confirmed), 2, rev),
     col="red", type = "l", ylab = "Número de casos", xlab = "Dias a partir de 06/04",
     main = "Covid-19 na cidade de São Carlos")


# Corona vírus por estado

linhas <- which(dados$is_last=="True" & dados$place_type == "state")
dados.Corte <- dados[linhas,]
dados.Corte = dados.Corte[order(dados.Corte$confirmed, decreasing = T),]

barplot(dados.Corte$confirmed, names.arg = dados.Corte$state,
     col=viridis(27), ylab = "Número de casos", xlab = "Estado",
     main = "Covid-19 no Brasil")

# 10 cidades mais contaminadas

linhas <- which(dados$is_last=="True" & dados$place_type == "city")
dados.Corte <- dados[linhas,]
dados.Corte = dados.Corte[order(dados.Corte$confirmed, decreasing = T),][1:10,]

barplot(dados.Corte$confirmed, names.arg = dados.Corte$city,
        col=viridis(10), ylab = "Número de casos", xlab = "",
        main = "As 10 cidades mais contaminadas", las = 2)


# Corona vírus por estado para cada 100 mil habitantes

linhas <- which(dados$is_last=="True" & dados$place_type == "state")
dados.Corte <- dados[linhas,]
dados.Corte = dados.Corte[order(dados.Corte$confirmed_per_100k_inhabitants, decreasing = T),]

barplot(dados.Corte$confirmed_per_100k_inhabitants, names.arg = dados.Corte$state,
        col=viridis(27), ylab = "Número de casos", xlab = "Estado",
        main = "Covid-19 no Brasil", sub = "(casos por 100 mil habitantes)")

# 10 cidades mais contaminadas para 100mil habitantes

linhas <- which(dados$is_last=="True" & dados$place_type == "city")
dados.Corte <- dados[linhas,]
dados.Corte = dados.Corte[order(dados.Corte$confirmed_per_100k_inhabitants, decreasing = T),][1:10,]

barplot(dados.Corte$confirmed_per_100k_inhabitants, names.arg = dados.Corte$city,
        col=viridis(10), ylab = "Número de casos", xlab = "Cidade",
        main = "As 10 cidades mais contaminadas", sub = "(casos por 100 mil habitantes)")


