# Exercício 1
# a)
dados = read.table("2019-sme0803-lista2dadosK2.14.R", header = T)

distribuicao = table(dados)
distribuicao

# b)
pie(distribuicao, labels = paste(names(distribuicao),": ", distribuicao))


# Exercício 2
# a)
dados = read.table("2019-sme0803-lista2dadosK2.15.R", header = T)

distribuicao = table(dados)
distribuicao

pie(distribuicao, labels = paste(names(distribuicao), ": ", distribuicao))


# Exercício 3
# a)
dados = read.table("2019-sme0803-lista2dadosK2.17.R", header = T)

freq_relativa = dados
freq_relativa$Frequ..ncia = freq_relativa$Frequ..ncia / sum(dados$Frequ..ncia)
colnames(freq_relativa) = c("Grau", "Frequência Relativa")
freq_relativa

# b)
dados2 = t(matrix(dados$Frequ..ncia))
colnames(dados2) = paste(dados$Grau)

grafico = barplot(dados2, xlab = "Grau", ylab = "Frequência", col = "lightblue")
text(grafico, dados$Frequ..ncia / 2, dados$Frequ..ncia)
box(bty = "L")

pie(dados$Frequ..ncia, labels = paste(dados$Grau, ": ", dados$Frequ..ncia))

# c)
n_estudantes = sum(dados$Frequ..ncia)
n_estudantes

proporcao = (n_estudantes - dados$Frequ..ncia[5]) / n_estudantes
proporcao


# Exercício 4
# a)
dados = read.table("2019-sme0803-lista2dadosK2.19.R", sep = "\n", header = T)

distribuicao = table(dados)
distribuicao

freq_relativa = distribuicao / sum(distribuicao)
freq_relativa

# b)
grafico = barplot(distribuicao, xlab = "Marca", ylab = "Frequência", col = "lightblue")
text(grafico, distribuicao / 2, distribuicao)
box(bty = "L")

pie(distribuicao, labels = paste(names(distribuicao), ": ", distribuicao))

# c)
proporcao = sum(distribuicao[1], distribuicao[2]) / sum(distribuicao)
proporcao

# d)
proporcao = (sum(distribuicao) - distribuicao[3]) / sum(distribuicao)
proporcao


# Exercício 5
# a)
library(readxl)

dados = read_xls("2019-sme0803-lista2dadosK2.30.xls")
dados = data.frame(dados)

Freq_com_cinto = t(matrix(dados$Com.cinto / sum(dados$Com.cinto)))

barplot(Freq_com_cinto, names.arg = paste(dados$Hora), xlab = "Hora do dia",
        ylab = "Frequência Relativa", las = 1, col = "lightblue", 
        main = "Com cinto")

# b)
Freq_sem_cinto = t(matrix(dados$Sem.cinto / sum(dados$Sem.cinto)))

barplot(Freq_sem_cinto, names.arg = paste(dados$Hora), xlab = "Hora do dia",
        ylab = "Frequência Relativa", las = 1, col = "lightblue", 
        main = "Sem cinto")

# c) Pode-se notar que o número de mortes em acidentes sem cinto é maior de madrugada,
# enquanto em acidentes com cinto, o número de mortes é maior no período da tarde


# Exercício 6
library(readxl)

cores = c("#00BFFF", "#7B68EE", "#F4A460", "#32CD32", "violet")

dados = read_xls("2019-sme0803-lista2dadosK2.33.xls")
dados = data.frame(dados)

Freq = dados
Freq$Freq.Homens = Freq$Freq.Homens / sum(Freq$Freq.Homens)
Freq$Freq.Mulheres = Freq$Freq.Mulheres / sum(Freq$Freq.Mulheres)

#Gráfico de barras empilhadas
barplot(as.matrix(Freq[-1]), col = cores, xlim = c(0, 3.55))
legend(x = 2.57278, y = 0.9908546, col = cores, 
       legend= paste(Freq$Classificação),
       pch = 15)

# Gráfico de barras lado a lado
barplot(t(as.matrix(Freq[-1])), col = c("lightblue", "orange"), beside = T,
                  xlim = c(0, 30), names.arg = paste(Freq$Classificação))
box(bty = "L")
legend(x = 20, y = 0.2, col = c("lightblue", "orange"), 
       legend= c("Homens", "Mulheres"),
       pch = 15)


# Exercício 7
# a)
library(readxl)

dados = read_xls("2019-sme0803-lista2dadosK2.34.xls")
dados = data.frame(dados)

barplot(t(as.matrix(dados[-1])), beside =T, names.arg = paste(dados$Grupo.de.idade),
        xlab = "Grupo de Idade", ylab = "Frequência Relativa",
        col = c("lightpink", "lightblue"), ylim = c(0,1.5), main = "Porcentagem da população trabalhando")
legend("topright", col = c("lightpink", "lightblue"),
       legend= c("Homens", "Mulheres"),
       pch = 15)

# b) Não, pois não temos a frequência relativa das idade de mulheres ou de homens


# Exercício 8
library(readxl)

dados = read_xls("2019-sme0803-lista2dadosK2.53.xls")
dados = data.frame(dados)$Dados

A_c = 2

limites = seq(min(dados), max(dados) + A_c, A_c)

freq = table(cut(dados, breaks = limites, right = F))
freq


# Exercício 9
dados = read.table("dados9.R", header = T)

barplot(dados$Frequência.relativa, names.arg = dados$Classe, las = 2, col = "orange",
        xlab = "Classe", ylab = "Frequência Relativa")


#Exercício 11
dados = read.table("dados11.R", sep = "\t", header = T)

barplot(dados$Frequência, names.arg = dados$Classe, las = 1, col = "lightgreen",
        xlab = "Classe", ylab = "Frequência")

# Exercício 13
# a)
library(readxl)

dados = read_xls("2019-sme0803-lista2dadosK2.52.xls")
dados = data.frame(dados)$Duração

stem(dados, scale = 0.3)
