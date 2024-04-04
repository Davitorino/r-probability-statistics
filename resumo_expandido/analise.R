library(data.table)
library(ggplot2)

tabela.freq <- function(variavel, var.nome) {
  var.tabela <- table(variavel, useNA = 'no')
  porc.var.tabela <- round(prop.table(var.tabela) * 100, 1)
  var.tabela <- data.frame(var.tabela, porc.var.tabela)
  var.tabela <- var.tabela[, -3]
  var.tabela
}

var.barplot = function(variavel, var.nome, orientacao = 1) {
  var.tabela <- tabela.freq(variavel, var.nome)
  colnames(var.tabela) <- c(var.nome, 'Frequencia', 'Porcentagem')
  
  barplot(
    height = var.tabela$Frequencia,
    names = var.tabela[[var.nome]],
    col=rgb(0.36, 0.12, 0.64, 1),
    xlab = var.nome,
    ylab = 'Frequência',
    las = orientacao
  )
}

qual.plot <- function(var.main, var.estr, nomes) {
  tabela <- table(var.main, var.estr, useNA = 'no')
  freq <- round(prop.table(tabela, 1) * 100, 1)
  tabela <- data.frame(tabela, freq)
  colnames(tabela) <- c(nomes[1], nomes[2], 'freq')
  ggplot(tabela, aes_string(fill=nomes[[2]], y='freq', x=nomes[[1]])) + 
    geom_bar(position='fill', stat='identity') +
    ylab('Porcentagem')
}

quan.qual.plot <- function(var.quan, var.qual, nomes) {
  tabela.medias <- aggregate(var.quan, by=list(var.qual), FUN='mean')
  colnames(tabela.medias) <- c(nomes[1], nomes[2])
  
  ggplot(tabela.medias, aes_string(x=nomes[1], y=nomes[2])) + 
    geom_bar(stat="identity")
}

base <- fread(
  input = paste0('suicidios_2010_a_2019.csv'), 
  header = T,
  na.strings = 'NA',
  data.table = FALSE
)

names(base)[7] <- 'sexo'
names(base)[8] <- 'raca'

idade.maxima <- 117
base$idade <- strtoi(substr(base$DTOBITO, 1, 4)) - strtoi(substr(base$DTNASC, 1, 4))
base <- base[!is.na(base$idade),]
base <- base[base$idade > 0 & base$idade < idade.maxima,]

tabela.sexo <- tabela.freq(base$sexo, 'Sexo')
colnames(tabela.sexo) <- c('Sexo', 'Frequencia', 'Porcentagem')
pie(
  tabela.sexo$Frequencia,
  paste(tabela.sexo$Sexo, ' (',tabela.sexo$Porcentagem, '%)', sep = ''),
  main = 'Frequência por sexo'
)
var.barplot(base$ano, 'Ano')
var.barplot(base$estado, 'Estado', 2)
var.barplot(base$idade, 'Idade', 2)
var.barplot(base$raca, 'Raça')

qual.plot(base$estado, base$raca, c('Estado', 'Raça'))
qual.plot(base$estado, base$sexo, c('Estado', 'Sexo'))
qual.plot(base$raca, base$sexo, c('Raça', 'Sexo'))

quan.qual.plot(base$idade, base$estado, c('Estado', 'Idade'))
quan.qual.plot(base$idade, base$raca, c('Raça', 'Idade'))
quan.qual.plot(base$idade, base$sexo, c('Sexo', 'Idade'))

qual.plot(base$ano, base$raca, c('Ano', 'Raça'))
qual.plot(base$ano, base$sexo, c('Ano', 'Sexo'))
qual.plot(base$ano, base$estado, c('Ano', 'Estado'))
