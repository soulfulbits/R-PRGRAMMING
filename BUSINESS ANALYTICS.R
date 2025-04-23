#QUESTION 1 (a)mtcars
datasets::mtcars
print(mtcars)
mean(mtcars$mpg)
mean(mtcars$hp)
mean(mtcars$wt)
median(mtcars$mpg)
median(mtcars$hp)
median(mtcars$wt)
sd(mtcars$mpg)
sd(mtcars$hp)
sd(mtcars$wt)

#1 (b)
top5_mpg<- head(mtcars[order(-mtcars$mpg),],5)
top5_mpg

#1 (c)
cor_matrix<- cor(mtcars[,c("mpg","hp","wt")])
cor_matrix

#1 (d)
mtcars$cyl <- as.factor(mtcars$cyl)
install.packages("ggplot2")
library(ggplot2)
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  labs(title = "Average MPG by Cylinder Count", x = "Cylinders", y = "MPG")

#QUESTION 2 (a)
library(datasets)
AirPassengers
class("airpassengers")
plot(AirPassengers, main="Monthly Air Passengers (1949-1960)",
     ylab="Number of Passengers (in thousands)", xlab="Year")

abline(lm(AirPassengers ~ time(AirPassengers)), col="red")


#2 (b)
install.packages("forecast")
library(forecast)
fit <- auto.arima(AirPassengers)
forecast_vals <- forecast(fit,h=12)
plot(forecast_vals)


#QUESTION 3 
budget<- data.frame(
 category=c("rent","groceries","utilities","transport","savings","entertaintment"),
 amount= c(15000,5000,2000,1500,4000,2500),
 colour= c("red","green","blue","orange","purple","yellow")
 )
print(budget)

ggplot(budget,aes(x="",y=amount,fill=category))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar(theta= "y")+
  scale_fill_manual(values = budget$colour)+
  labs(title = "household expenditure budget")+
  theme_void()

#QUESTION 4
x <- seq(10,50,by = 4)
print(x)
results<- c(x)
for (i in x){
  value <- i^3 - 3*i
  results<- c(results,value)
}
print(results)

#QUESTION 5
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tm)
library(wordcloud)
library(RColorBrewer)

url <- "https://www.gutenberg.org/files/1342/1342-0.txt"
download.file(url, destfile = "pride.txt", method = "auto")

text <- readLines("pride.txt", encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

wordcloud(words = df$word, freq = df$freq,
          min.freq = 5,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
