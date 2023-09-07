library(data.table)
base <- fread(input = paste0("base_modif_2.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# ANALISE COM O PACOTE FSA
library(FSA)
result1 <- Summarize(salario_USD ~ experiencia, data=base)
result1

# COEFICIENTE DE VARIACAO
coef.var <- function (variavel){
  mu <- mean(variavel, na.rm=TRUE)
  s <- sd(variavel, na.rm=TRUE)
  cv <- (s*100)/mu
  cv
}
coef.var(base$salario)

# ANALISE COM O PACOTE PSYCH
library(psych)
result2 <- describeBy(salario_USD~experiencia, data=base, mat=T)
result2 <- result2[,c(2,4,5,6)]
result2
# incluir o cálculo do coeficiente de variação na tabela result2
,
# ANALISE COM O PACOTE DOBY
library(doBy)
result3 <- summaryBy(salario_USD ~ experiencia, data = base,
                     FUN = function(x) { c(n = length(x), m = mean(x), s = sd(x), cv = sd(x)/mean(x)*100) } )
result3
# incluir a ano na tabela do result3

# GRÁFICO COM MEDIDA DE VARIACAO
library(ggplot2)
ggplot(result3) +
  geom_bar( aes(x=experiencia, y=salario_USD.m), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=experiencia, ymin=salario_USD.m-salario_USD.s, ymax=salario_USD.m+salario_USD.s), width=0.4, colour="red", alpha=0.7, size=1)
