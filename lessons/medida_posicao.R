# média ponderada
freq <- c(16, 8, 2)
peso <- freq / sum(freq)
valores <- c(15, 16, 17)
media.ponderada <- weighted.mean(valores, peso)

library(data.table)
base <- fread(paste0('base_modif.csv'), header = T, na.strings = 'NA', data.table = FALSE, dec = ',')

# analise descritiva
summary(base$salario_USD)

# analise descritiva por grupo
base2020 <- base[base$ano == 2020, ]
summary(base2020$salario_USD)
base2021 <- base[base$ano == 2021, ]
summary(base2021$salario_USD)
base2022 <- base[base$ano == 2022, ]
summary(base2022$salario_USD)

# mesma coisa que acima mas de forma direta
tapply(base$salario_USD, base$ano, summary)

# percentis
quantile(base$salario_USD, probs=0.3)
by(base$salario_USD, base$experiencia, quantile, probs=0.3)

# analise descritiva - psych
library(psych)
resultados <- describeBy(salario_USD ~ ano, data=base, mat=T)
resultados$group1 <- as.numeric(resultados$group1)

# grafico
library(ggplot2)
ggplot(data=resultados, aes(x=group1, y=mean)) +
  geom_line(color='blue', lwd=0.8)

ggplot(data=resultados, aes(x=group1)) +
  geom_line(aes(y=mean),color="blue") +
  geom_line(aes(y=median),color="red") +
  ylab("Salário USS") + 
  xlab("Ano") +
  ggtitle("Evolução Salarial")

resultados2 <- describeBy(salario_USD ~ ano+experiencia, data=base, mat=T)
resultados2$group1 <- as.numeric(resultados$group1)

ggplot(data=resultados2, aes(x=group1, y=median, fill=group2)) +
  geom_line()+
  geom_line(aes(color=group2)) +
  scale_color_manual(values=c("black","blue","red","green"))
