# A partir dos valores de x e y obter os seguintes resultados:
#ler arquivo csv
dados = read.csv("./spotify-2023.csv", encoding = "UTF-8")
# str(dados)
# names(dados)
# remover linhas com valores NUll
# dados = dados[complete.cases(dados),]
# head(dados)

# escolher as colunas x e y
x = dados$in_spotify_playlists
y = dados$bpm

# a) Média, variância, desvio padrão e mediana para x e y.
# media
media_x = mean(x)
media_y = mean(y)

print(sprintf("media de x: %.3f", media_x))
print(sprintf("media de y: %.3f", media_y))
# variancia
variancia_x = var(x)
variancia_y = var(y)

print(sprintf("variancia de x: %.3f", variancia_x))
print(sprintf("variancia de y: %.3f", variancia_y))

# desvio padrao
desvio_padrao_x = sd(x)
desvio_padrao_y = sd(y)
print(sprintf("desvio padrao de x: %.3f", desvio_padrao_x))
print(sprintf("desvio padrao de y: %.3f", desvio_padrao_y))

# mediana
mediana_x = median(x)
mediana_y = median(y)
print(sprintf("mediana de x: %.3f", mediana_x))
print(sprintf("mediana de y: %.3f", mediana_y))



print('x = coluna bpm')
print(sprintf("media de x: %.3f", media_x))
print(sprintf("variancia de x: %.3f", variancia_x))
print(sprintf("desvio padrao de x: %.3f", desvio_padrao_x))
print(sprintf("mediana de x: %.3f", mediana_x))

print('y = coluna in_spotify_playlists')
print(sprintf("media de y: %.3f", media_y))
print(sprintf("variancia de y: %.3f", variancia_y))
print(sprintf("desvio padrao de y: %.3f", desvio_padrao_y))
print(sprintf("mediana de y: %.3f", mediana_y))

# b) O histograma de x e y.
# histograma de x

hist(x, col="darkblue", border="black")
# histograma de y
hist(y, col="darkred", border="black")

# c) O boxplot de x e y.

# boxplot de x
boxplot(x)
# boxplot de y
boxplot(y)

# d) O coeficiente de correlação de x e y.
print('correlacao')
print(cor(x, y))


# e) Fazer o teste de normalidade para  y e x.
# teste de normalidade para x
shapiro.test(x)
# teste de normalidade para y
shapiro.test(y)

# f) Fazer o gráfico de densidade junto com o histograma para as variáveis x e y.
# gráfico de densidade junto com o histograma para x
plot(density(x))
hist(x, add = TRUE,freq=false, col = "red",border="black")
# gráfico de densidade junto com o histograma para x
h<-hist(x, breaks=10, col="red", main="Histogram X")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
# gráfico de densidade junto com o histograma para y
plot(density(y))
hist(y, add = TRUE, col = "blue",border="black")
h<-hist(y, breaks=50, col="red", main="Histogram Y")
xfit<-seq(min(y),max(y),length=40)
yfit<-dnorm(xfit,mean=mean(y),sd=sd(y))
yfit <- yfit*diff(h$mids[1:2])*length(y)

lines(xfit, yfit, col="blue", lwd=2)




