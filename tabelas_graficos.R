library(data.table)
base <- fread(input = paste0('dados_salarios.csv'), header = T, na.strings = "NA", data.table = FALSE, dec = ",")

base$trab_remoto <- as.character(base$trab_remoto)

library(dplyr)
base$trab_remoto <- recode(base$trab_remoto, `0`='1:Não', `50`='2:Parcial', `100`='3:Total')
base$experiencia <- recode(base$experiencia, `EN`='1:EN', `MI`='2:MI', `SE`='3:SE', `EX`='4:EX')

write.csv2(base, 'dados_salarios_alt.csv', row.names = FALSE)
#write.csv2(base, 'dados_salarios_alt.csv', row.names = FALSE, fileEncoding = 'latin')

freq.tabela <- table(base$experiencia, useNA = 'ifany')
porc.tabela <- round(prop.table(freq.tabela) * 100, 1)
freq.tabela <- data.frame(freq.tabela, porc.tabela)
freq.tabela <- freq.tabela[,-3]
colnames(freq.tabela) <- c('Experiencia', 'Frequencia', 'Porcentagem')

png(file = 'barras.png')
barplot(
  height = freq.tabela$Frequencia, names = freq.tabela$Experiencia,
  col=rgb(0.9, 0.6, 0.5, 0.5),
  xlab = 'Experiência',
  ylab = 'Frequência',
  main = ''
)
dev.off()

png(file = 'torta.png')
pie(freq.tabela$Frequencia, freq.tabela$Experiencia, main = 'Tempo de Experiência')
dev.off()

freq.tabela <- table(base$experiencia, base$trab_remoto)
porc.tabelaL <- round(prop.table(freq.tabela, 1) * 100, 1)
tabela <- data.frame(freq.tabela, freq.tabela)
colnames(tabela) <- c('Experiencia', 'Trab_Remoto', 'Freq')

library(ggplot2)
ggplot(tabela, aes(fill=Trab_Remoto, y=Freq, x=Experiencia)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Porcentagem")

library(dplyr)
library(ggplot2)
hist(base$salario_USD)
hist(base$salario_USD,
     breaks=6)
hist(base$salario_USD,
     main="Distribuição Salarial",
     xlab="Salários",
     col="darkmagenta",
     xaxt = 'n',
     freq=FALSE)
myTicks = axTicks(1)
axis(1, at = myTicks, labels = formatC(myTicks, format = 'd'))

tabela.medias <- aggregate(base$salario_USD, by=list(base$experiencia), FUN="mean")
colnames(tabela.medias) <- c("Experiencia","Sal_Medio")

library(ggplot2)
ggplot(tabela.medias, aes(x=Experiencia, y=Sal_Medio)) + 
  geom_bar(stat="identity")

ggplot(data = iris, aes(Petal.Length, Petal.Width)) +
  geom_point() 
png(file = "dispersao.png")
ggplot(data = iris, aes(Petal.Length, Petal.Width, color=Species)) +
  geom_point() 
dev.off()

library(psych)
base$Cat_Salario[base$salario_USD <100000]  = "G1"
base$Cat_Salario[base$salario_USD >=100000 & base$salario_USD <200000]  = "G2"
base$Cat_Salario[base$salario_USD >=200000 & base$salario_USD <300000]  = "G3"
base$Cat_Salario[base$salario_USD >=300000 & base$salario_USD <400000]  = "G4"
base$Cat_Salario[base$salario_USD >=400000]  = "G5"
freq.tabela <- table(base$Cat_Salario, useNA = "ifany") 
freq.tabela

mean(base$salario_USD)
min(base$salario_USD)
max(base$salario_USD)