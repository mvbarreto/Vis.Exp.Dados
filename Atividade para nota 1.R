##################################################################
### ATIVIDADE AVALIATIVA DE VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS ###
# Data de entrega: 10/05/2020
# ALUNO: Matheus Vinícius Barreto de Farias    N.USP:11810175
##################################################################

## Para ver este arquivo sem problema de acentuação use o encoding
# UTF-8. Salve com o mesmo encoding.

## Todos devem preencher o cabeçalho acima no arquivo .R ou 
# nos manuscritos.

## A atividade pode ser feita com ou sem o uso do R. Sem o 
# uso do R significa fazer "à mão".

## Quem optar por usar o R deve:
# - usar o conjunto de dados completo ou seja o Cars93 do pacote MASS 
# - entregar este arquivo .R mudando o nome do arquivo para o número USP.
# - escrever as interpretações, respostas ou comentários usando o # na 
# frente e em seguida ao comando utilizado para isso.

## Quem optar por fazer à mão deve:
# - usar o conjunto de dados parcial que está em pdf.
# - entregar as soluções em um único arquivo pdf ou jpg, sendo o nome 
# do arquivo o número USP.
# - escrever as interpretações, respostas ou comentários em cada item.

## Todos devem entregar o arquivo pelo escaninho da atividade no 
# e-disciplinas.

##################################################################

# Conjunto de dados
library(MASS)
dados = Cars93

#1) Construa uma tabela de frequências para o número de passageiros.
tabela = as.data.frame(table(dados$Passengers))

tabela[length(tabela) + 1] = tabela$Freq / sum(tabela$Freq)
tabela[length(tabela) + 1] = cumsum(tabela$Freq)
tabela[length(tabela) + 1] = cumsum(tabela$Freq) / sum(tabela$Freq)

names(tabela) = c("Número de Passageiros", "Frequência absoluta", "Frequência relativa",
                  "Frequência acumulada", "Frequência relativa acumulada")

View(tabela) #Resposta


#2) Quantos modelos de carros são considerados no conjunto de dados?
length(dados$Model) # (podia-se usar qualquer função que retorne o número de observações)
#Resposta: São considerados 93 modelos de carros.


#3) Quantas montadoras (manufacturer) de carros há?
length(table(dados$Manufacturer))
#Resposta: Há 32* montadoras.
# *a marca "Chrylser" é um erro de digitação de "Chrysler", logo haveriam 31 montadoras


#4) Qual marca tem o preço médio dos seus carros maior?
tabela = as.data.frame(table(dados$Manufacturer)) # prepara uma tabela com cada uma das marcas
names(tabela) = c("Marca", "Média")

for (i in seq(1, length(tabela$Marca), 1)) { 
  tabela[i,2] = mean(dados$Price[dados$Manufacturer == tabela[i,1]]) 
} # o loop percorre a tabela de marcas e tira a média dos preços para cada uma

tabela$Marca[tabela$Média == max(tabela$Média)] # retorna a marca com o maior valor médio
#Resposta: Infiniti é a marca com maior preço em média


#5) Faça um gráfico de Pareto para a variável airbags.
library(qcc)
pareto.chart(table(dados$AirBags), main = "Airbags", las = 1,
             names.arg = c("Somente motorista", "Nenhum", "Motorista e passageiro"),
             xlab = "Categoria", ylab = "Frequência", ylab2 = "Porcentagem acumulada")


#6) Faça um gráfico de setores para os tipos de carros (small, midsize, compact)

# Vale considerar que não exitem só os tipos small, midsize e compact, mas também os tipos
# large, sporty e van. Ou ainda, podemos pensar que os tipos compact, sporty e van fazem
# parte dos tipos small, midsize e large, respectivamente.

# Gráfico considerando somente os tipos small, midsize e compact
tabela = table(dados$Type[dados$Type == "Small" | dados$Type == "Midsize" | dados$Type == "Compact"])
tabela = tabela / sum(tabela)
tabela = tabela[-c(which(tabela == 0))]

pie(tabela, labels = paste(names(tabela), " - ", 100*round(tabela, 4), "%"),
    main = "Tipos de carros")

# Gráfico considerando todos os tipos de carros
tabela = table(dados$Type)
tabela = tabela / sum(tabela)

pie(tabela, labels = paste(names(tabela), " - ", 100*round(tabela, 4), "%"),
    main = "Tipos de carros")

# Gráfico considerando os tipos compact, sporty e van como parte dos tipos small, midsize e large
tabela = table(dados$Type[dados$Type == "Small" | dados$Type == "Midsize" | dados$Type == "Large"])
tabela = tabela[-c(which(tabela == 0))]

tabela2 = table(dados$Type[dados$Type == "Compact" | dados$Type == "Sporty" | dados$Type == "Van"])
tabela2 = tabela2[-c(which(tabela2 == 0))]

tabela[1] = tabela[1] + tabela2[3]
tabela[2] = tabela[2] + tabela2[2]
tabela[3] = tabela[3] + tabela2[1]

tabela = tabela / sum(tabela)
pie(tabela, labels = paste(names(tabela), " - ", 100*round(tabela, 4), "%"),
    main = "Tipos de carros")


#7) Faça um histograma para o comprimento do carro. 
hist(dados$Length, xlab = "Comprimento do carro (polegadas)", ylab = "Frequência", main = NULL)


#8) Explore a distribuição dos preços dos carros. Para isso, use medidas estatísticas e gráficos.
#Interprete os valores obtidos e os gráficos (pelo menos 3 medidas e um gráfico).

graph = boxplot(dados$Price, horizontal = T, xlab = "Preços", pch = "*")
graph$out

# Pelo gráfico boxplot, existem três valores atipicos nesse conjunto.

dados.corte = dados[order(dados$Price, decreasing = T),][1:10,]
graph = barplot(dados.corte$Price,las = 1, main = "10 modelos mais caros", ylab = "Preço",xlab = "Modelo e montadora",
        names.arg = paste(dados.corte$Model, "\n", dados.corte$Manufacturer))
text(graph, dados.corte$Price / 2, dados.corte$Price)

# Podemos notar que esses valores são referentes a marcas que produzem
# carros de luxos e tiveram apenas um modelo analisado

median(dados$Price) # mediana: 17.7
max(dados$Price) - min(dados$Price) # amplitude: 54.5
sd(dados$Price) # desvio padrão: 9.65943

3*(mean(dados$Price)-median(dados$Price))/sd(dados$Price) # 2 coeficiente de assimetria de Pearson

graph = hist(dados$Price) # Histograma dos preços

# Pelo histograma e pelas medidas, podemos verificar que a maioria dos carros
# do conjunto é de valor popular. Inclusive podemos notar uma assimetria (de cauda) à direita,
# indicada pelo 2 coeficiente de assimetria de Pearson e confirmada pelo histograma.
