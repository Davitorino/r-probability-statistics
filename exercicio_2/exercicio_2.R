library(data.table)
library(dplyr)
library(psych)

base <- fread(input = paste0('mobile.csv'), header = T, na.strings = "NA", data.table = FALSE, dec = '.')
amostra <- base[sample(nrow(base), size = 400),]

amostra$price_range <- recode(amostra$price_range, `0`='0:BAIXO', `1`='1:MEDIO', `2`='2:CARO', `3`='3:MUITO-CARO')

# cálculos da variável “battery_power”
summary(amostra$battery_power) # média, mediana, percentil 25 e 75, mínimo e máximo 
quantile(amostra$battery_power, probs = 0.05) # percentil 5
quantile(amostra$battery_power, probs = 0.95) # percentil 95

# cálculos da variável “ram”
summary(amostra$ram) # média, mediana, percentil 25 e 75, mínimo e máximo 
quantile(amostra$ram, probs = 0.05) # percentil 5
quantile(amostra$ram, probs = 0.95) # percentil 95

# cálculos da variável “battery_power” categorizados por “price_range”
by(amostra$battery_power, amostra$price_range, summary)
by(amostra$battery_power, amostra$price_range, quantile, probs = 0.05)
by(amostra$battery_power, amostra$price_range, quantile, probs = 0.95)

# cálculos da variável “ram” categorizados por “price_range”
by(amostra$ram, amostra$price_range, summary)
by(amostra$ram, amostra$price_range, quantile, probs = 0.05)
by(amostra$ram, amostra$price_range, quantile, probs = 0.95)
