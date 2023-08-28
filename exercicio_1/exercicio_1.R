# Importação das bibliotecas
library(data.table)
library(dplyr)
library(ggplot2)
library(psych)

# Base de dados
dados <- fread(
  input = paste0('exercicio_1/mobile.csv'),
  header = T,
  na.strings = 'NA',
  data.table = FALSE,
  dec = ','
)
dados$price_range <- recode(dados$price_range, `0`='0:BAIXO', `1`='1:MEDIO', `2`='2:CARO', `3`='3:MUITO-CARO')
dados$wifi <- recode(dados$wifi, `0`='0:NAO', `1`='1:SIM')

# Tabela de faixa de preço
price.tabela <- table(dados$price_range, useNA = 'ifany')
porc.price.tabela <- round(prop.table(price.tabela) * 100, 1)
price.tabela <- data.frame(price.tabela, porc.price.tabela)
price.tabela <- price.tabela[, -3]
colnames(price.tabela) <- c('Faixa_Preco', 'Frequencia', 'Porcentagem')
price.tabela

# Gráfico da tabela de faixa de preço
barplot(
  height = price.tabela$Frequencia,
  names = price.tabela$Faixa_Preco,
  col=rgb(0.36, 0.12, 0.64, 1),
  xlab = 'Faixa de Preço',
  ylab = 'Frequência',
  main = ''
)

# Tabela de wifi x faixa de preço
wifi.price.tabela <- table(dados$wifi, dados$price_range)
porc.wifi.price.tabela <- round(prop.table(wifi.price.tabela, 1) * 100, 1)
wifi.price.tabela <- data.frame(wifi.price.tabela, porc.wifi.price.tabela)
wifi.price.tabela <- wifi.price.tabela[, c(-4, -5)]
colnames(wifi.price.tabela) <- c('Wifi', 'Faixa_Preco', 'Frequencia', 'Porcentagem')
wifi.price.tabela

# Gráfico de relação Wifi x Faixa de preço
ggplot(wifi.price.tabela, aes(fill=Wifi, y=Frequencia, x=Faixa_Preco)) +
  geom_bar(position='fill', stat='identity') +
  xlab('Faixa de Preço') +
  ylab('Porcentagem')

# Tabela de faixa de preço x ram
price.ram.tabela <- aggregate(dados$ram, by=list(dados$price_range), FUN='mean')
colnames(price.ram.tabela) <- c('Faixa de preço', 'RAM')
price.ram.tabela

# Classificação baseada na RAM
dados$ram_interval[dados$ram <= 800] <- 'RAM1'
dados$ram_interval[dados$ram > 800 & dados$ram <= 1600] <- 'RAM2'
dados$ram_interval[dados$ram > 1600 & dados$ram <= 2400] <- 'RAM3'
dados$ram_interval[dados$ram > 1600 & dados$ram <= 2400] <- 'RAM3'
dados$ram_interval[dados$ram > 2400 & dados$ram <= 3200] <- 'RAM4'
dados$ram_interval[dados$ram > 3200] <- 'RAM5'

# Tabela intervalo de RAM
ram_interval.tabela <- table(dados$ram_interval)
porc.ram_interval.tabela <- round(prop.table(ram_interval.tabela) * 100, 1)
ram_interval.tabela <- data.frame(ram_interval.tabela, porc.ram_interval.tabela)
ram_interval.tabela <- ram_interval.tabela[, -3]
colnames(ram_interval.tabela) <- c('Intervalo_Ram', 'Frequencia', 'Porcentagem')
ram_interval.tabela