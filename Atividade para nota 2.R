##################################################################
### ATIVIDADE AVALIATIVA DE VISUALIZAÇÃO E EXPLORAÇÃO DE DADOS ###
# Data de entrega: 02/07/2020
# ALUNO: Matheus Vinícius Barreto de Farias  N.USP:11810175
##################################################################

### LEIA ATENTAMENTE AS INSTRUÇÕES A SEGUIR ###
### ALGUMAS REGRAS QUE NÃO FOREM RESPEITADAS TÊM UM CUSTO EM PONTOS ###

## Para ver este arquivo sem problema de acentuação use o encoding
# UTF-8. Salve com o mesmo encoding. (Não atender a este critério 
# custará 0,5 ponto a menos)

## Todos devem preencher o cabeçalho acima no arquivo .R ou 
# nos manuscritos. (Não atender a este critério custará 0,5 ponto
# ponto a menos)

## A atividade pode ser feita com ou sem o uso do R. Sem o 
# uso do R significa fazer "à mão".

## Quem optar por usar o R deve:
# - usar o conjunto de dados completo ou seja o Cars93 do pacote MASS 
# - entregar este arquivo .R mudando o nome do arquivo para o número USP.
# - escrever as interpretações, respostas ou comentários usando o # na 
# frente e em seguida ao comando utilizado para isso.
# - deixar os install.packages comentados com # ou não os deixar no 
# código (Não atender a este critério custará 0,2 ponto ponto a menos cada)

## Quem optar por fazer à mão deve:
# - usar o conjunto de dados parcial que está em pdf.
# - entregar as soluções em um único arquivo pdf ou jpg, sendo o nome 
# do arquivo o número USP.
# - escrever as interpretações, respostas ou comentários em cada item.

## Todos devem entregar o arquivo pelo escaninho da atividade no 
# e-disciplinas.

##################################################################

rm(list = ls(all = TRUE))

## Carregando os pacotes e lendo o conjunto de dados
library(ggplot2)
library(ggrepel)
library(MASS)

dados = Cars93

## Traduzindo algumas partes do conjunto de dados para facilitar o entendimento
# de um leitor brasileiro.
levels(dados$Man.trans.avail) = c("Não", "Sim")
levels(dados$DriveTrain) = c("4 Rodas", "Frontal", "Traseira")
levels(dados$AirBags) = c("Motorista e passageiros", "Apenas motorista", "Nenhum")
levels(dados$Type) = c("Compacto", "Grande", "Médio", "Pequeno", "Esporte", "Van")


#1) Analise a relação das variáveis EngineSize, Fuel.tank.capacity,
# Horsepower e Weight duas a duas. Use gráficos e/ou medidas estatísticas, 
# o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

## Inicialmente, podemos imaginar que todas essas medidas estão relacionadas. Afinal,
# um motor mais potente precisa ser maior, sendo, portanto, mais pesado, e ainda
# deve consumir mais gasolina. Para confirmar ou negar essa hipótese, usarei o gráfico
# de dispersão e a medida estatística de correlação linear entre duas variáveis.

## Comparação das observações duas a duas
# Relação entre EngineSize e Fuel.tank.capacity

coeff = lm(Fuel.tank.capacity ~ EngineSize, dados)$coefficients # ajusta uma reta para as variáveis

ggplot(dados, aes(x = EngineSize, y = Fuel.tank.capacity) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Tamanho do motor (litros)", y = "Capacidade do tanque (galões americanos)")

cor(dados$EngineSize, dados$Fuel.tank.capacity) # 0.7593062

# Relação entre EngineSize e Horsepower

coeff = lm(Horsepower ~ EngineSize, dados)$coefficients

ggplot(dados, aes(x = EngineSize, y = Horsepower) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Tamanho do motor (litros)", y = "Potência do Motor (cavalo-vapor)")

cor(dados$EngineSize, dados$Horsepower) # 0.7321197

# Relação entre EngineSize e Weight

coeff = lm(Weight ~ EngineSize, dados)$coefficients

ggplot(dados, aes(x = EngineSize, y = Weight) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Tamanho do motor (litros)", y = "Peso do carro (libras)")

cor(dados$EngineSize, dados$Weight) # 0.8450753

# Relação entre Fuel.tank.capacity e Horsepower

coeff = lm(Horsepower ~ Fuel.tank.capacity, dados)$coefficients

ggplot(dados, aes(x = Fuel.tank.capacity, y = Horsepower) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Capacidade do tanque (galões americanos)", y = "Potência do Motor (cavalo-vapor)")

cor(dados$Fuel.tank.capacity, dados$Horsepower) # 0.7117903

# Relação entre Fuel.tank.capacity e Weight

coeff = lm(Weight ~ Fuel.tank.capacity, dados)$coefficients

ggplot(dados, aes(x = Fuel.tank.capacity, y = Weight) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Capacidade do tanque (galões americanos)", y = "Peso do carro (libras)")

cor(dados$Fuel.tank.capacity, dados$Weight) # 0.8940181

# Relação entre Horsepower e Weight

coeff = lm(Weight ~ Horsepower, dados)$coefficients

ggplot(dados, aes(x = Horsepower, y = Weight) )+
  geom_point() +
  geom_abline(intercept = coeff[1], slope = coeff[2], color = "red") + 
  labs(x = "Potência do Motor (cavalo-vapor)", y = "Peso do carro (libras)")

cor(dados$Horsepower, dados$Weight) # 0.7387975

## Tanto a observação dos gráficos quanto das medidas de correlação (todas maiores que 0.7)
# confirmam a hipótese inicial de que essas medidas estariam fortemente relacionadas.

## O ajuste linear foi feito com base neste link:
# http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines


#2) Analise a relação das variáveis DriveTrain e Horsepower. Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

## Assim como no item anterior, podemos imaginar que há uma relação entre as
# variáveis DriveTrain (tipo de tração) e Horsepower (potência do motor).
# Assim, recorrerei ao gráfico boxplot, de modo que seja fácil observar
# as faixas de potência para cada tipo de tração. Além disso, será possível
# encontrar candidatos à outlier. Essa informação seria de grande utilidade para,
# por exemplo, um comprador que quisesse ir atrás dos modelos que entregam maior
# performance de acordo com o tipo de tração.

p = ggplot(dados, aes(x = Horsepower, y = DriveTrain)) +
  geom_boxplot() + labs(x = "Potência do motor (cavalo-vapor)", y = "Tipo de tração")
p

## Observa-se que o carros que usam tração traseira tem um escopo de potência mais
# amplo que o das outras categorias, bem como uma maior potência mediana.
# Além disso, destacam-se dois possíveis outliers, um nos modelos que possuem tração
# dianteira e o outro nos que têm tração nas quatro roda.

dados.corte = dados[order(dados$Horsepower, decreasing = T),][1:5,]
graph = barplot(dados.corte$Horsepower,las = 1, main = "Os cinco modelos mais potentes",
                ylab = "Potência do motor (cavalos-vapor)",xlab = "Modelo e tipo de tração",
                names.arg = paste(dados.corte$Make, "\n", dados.corte$DriveTrain))
text(graph, dados.corte$Horsepower / 2, dados.corte$Horsepower)

## De cara vemos que esse dois outliers são o Dodge Stealth (tração nas quatro rodas)
# e o Cadillac Seville (tração nas rodas da frente). Atualmente, esses são carros
# de colecionadores. O primeiro é um modelo que era fabricado pela Mitsubishi com
# ajuda Chrysler (https://www.allpar.com/cars/dodge/stealth.html) marca famosa também
# pelo Dodge Charge. Já o segundo é um modelo da montadora General Motors. Tendo sido
# lançado como carro de luxo nos anos nos anos 70, o Cadillac fez tanto sucesso
# que foi fabricado até o ano de 2004 (https://pt.wikipedia.org/wiki/Cadillac_Seville).


#3) Analise a relação das variáveis AirBags e DriveTrain. Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

## Para essa análise, podemos nos perguntas quais tipos de carros são mais seguros
# (em relação ao uso de airbags) de acordo com o tipo de tração. Para isso
# será utilizada uma tabela de proporções em relação à marginal y.

tab = table(dados$AirBags, dados$DriveTrain)
tab

addmargins(tab, 1) # *Apesar de não fazer parte da hipótese inicial, observa-se também
# um número maior de carros com tração fronteira em relação aos outros tipos. Será
# possível visualizar graficamente essa disposição no item 5.

round(prop.table(tab, margin = 2) * 100, 2)

## Fica evidente que os carros do conjunto que têm tração traseira são os mais
# seguros em relação à presença de airbags pois apenas 6% deles não apresentam o dispositivo,
# além de apresentarem a maior taxa de airbags para o motorista e para os passageiros (31.25%).
# Já os carros com tração nas quatro rodas seriam os menos seguros, visto que metade deles
# não tem airbags e a outra metade só tem para o motorista.


#4) Analise o comportamento da variável tipo do carro (Type) dado
# o tipo de direção (Man.trans.avail). Use gráficos 
# e/ou medidas estatísticas, o que achar adequado, além de palavras 
# para dar sua resposta. Interprete os resultados.

## Assim como no item anterior, uma tabela de proporções (dessa vez em relação à marginal x)
# é bastante adequada para verificarmos se cada tipo de cada carro está relacionado a um
# tipo de transmissão. Em seguida, será construido um gráfico de mosaico para confirmar essa
# relação e compará-la com um cenário em que não há depedência entre as variáveis.

## Tabela de proproções
tab = table(dados$Type, dados$Man.trans.avail)
tab

round(prop.table(tab, margin = 1) * 100, 2)

## Gráfico Mosaico
tab_marg = addmargins(tab, 1:2) # é melhor criar uma segunda tabela para não afetar os dados orginais

# auxiliares
k = nrow(tab_marg) - 1
m = ncol(tab_marg) - 1

n = sum(tab)

# construindo a tabela em que se supõe independência
tab_ind = tab_marg[1:k, m+ 1] %*% t(tab_marg[k + 1, 1:m]) / n
tab_ind

# renomeando as linhas e colunas
rownames(tab_ind) = rownames(tab)
colnames(tab_ind) = colnames(tab)

# construindo o gráfico
par(mfrow = c(1, 2)) # cria a grade para exibir dois gráficos
mosaicplot(tab, ylab = "Câmbio manual disponível", xlab = "Tipo de carro",
           col = "white", main = "Dados observados")
mosaicplot(tab_ind, ylab = "Câmbio manual disponível", xlab = "Tipo de carro",
           col = "white", main = "Sob independência")

par(mfrow = c(1, 1)) # volta para o padrão

## A tabela e o gráfico nos mostram que há sim uma forte relação entre as variáveis,
# confirmando a hipótese. Esse tipo de informação nas mãos de uma montadora poderia
# definir a estratégia de mercado adotada. Se essa empresa quisesse lançar um produto
# que já é consolidado no mercado, bastaria combinar o tipo de carro com o tipo
# de câmbio. Já se a empresa buscasse a criação de um "outlier" - tanto para atender um nicho
# quanto na esperança de revolucionar - ela poderia escolher a transmissão de maneira
# oposta ao comum no mercado.


#5) Analise as variáveis Horsepower, Weight, Man.trans.avail, Passengers
# e DriveTrain. Use gráficos e/ou medidas estatísticas, o que achar 
# adequado, além de palavras para dar sua resposta. Interprete os 
# resultados.

## Podemos visualizar todas essas variáveis ao mesmo tempo em um gráfico de dispersão
p = ggplot(dados, aes(x = Horsepower, y = Weight)) +
  geom_point( aes(colour = Man.trans.avail, size = Passengers, shape = DriveTrain) ) +
  labs(x = "Potência do Motor (cavalo-vapor)", y = "Peso do carro (libras)",
       colour = "Câmbio manual\ndisponível", size = "Nº de Passageiros", shape = "Tipo de tração")
p

## Nota-se que o número de passageiros cresce junto à potência do motor e ao peso
# do carro. Além disso, vemos que carros mais leves e menos potentes costumam
# ter tração nas rodas da frente, enquanto que os carros de porte médio em diante
# são mais variados nesse quesito. Quanto ao câmbio, nota-se uma tendência semelhante:
# carros mais leves e menos potentes têm transmissão manual disponível, enquanto os
# mais pesados e mais potentes apresentam uma maior variabilidade.

## Um cliente poderia querer comparar os modelos de acordo com esse gráfico
p + geom_text_repel(label = dados$Model) # *melhor visualizado dando zoom no gráfico

## Podemos ver os outliers analisados no item 2 e mais alguns carros que fogem do
# padrão. Uma análise interessante seria comparar o preço do carro em vez do peso
# (claro, isso vai depender do objetivo da análise).

p = ggplot(dados, aes(x = Horsepower, y = Price)) +
  geom_point( aes(colour = Man.trans.avail, size = Passengers, shape = DriveTrain) ) +
  labs(x = "Potência do Motor (cavalo-vapor)", y = "Preço do carro (milhares de dólares)",
       colour = "Câmbio manual\ndisponível", size = "Nº de Passageiros", shape = "Tipo de tração") +
  geom_text_repel(label = dados$Model)
p

## Observamos que o Dodge Stealth entrega muito mais potência em relação aos concorrentes
# na mesma categoria de preço. Com esse tipo de gráfico fica fácil comparar os modelos
# em vários quesitos ao mesmo tempo.
