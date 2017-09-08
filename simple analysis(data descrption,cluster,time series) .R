library(rJava)
library(Rwordseg)
library(tm)
d=as.character(data3$Job_Description)
words=lapply(d,removeNumbers)
words=gsub(pattern="[a-zA-Z]+","",words)
wordsegment=function(x){
  library(Rwordseg)
  segmentCN(x)
}
words=lapply(words,wordsegment)
corpus=Corpus(VectorSource(words))
meta(corpus,"cluster")=data3$Job_Description
(words.dtm=DocumentTermMatrix(corpus,control=list(wordLenghts=c(1,Inf))))
words.dtm2=removeSparseTerms(words.dtm,sparse=0.9)
dtm_matrix=as.data.frame(inspect(words.dtm2))
dist.dtm<- dissimilarity(words.dtm2, method = "cosine")
job=hclust(dist(dtm_matrix[1:300,]),method="average")
plot(job)


#换表格#
d2=as.data.frame(summary(data$PositionType))
d1=as.character(data$PositionType)
words1=lapply(d1,removeNumbers)
words1=gsub(pattern="[a-zA-Z]+","",words)
wordsegment=function(x){
  library(Rwordseg)
  segmentCN(x)
}
words1=lapply(words1,wordsegment)
corpus1=Corpus(VectorSource(words1))
meta(corpus1,"cluster")=data$PositionType
(words.dtm1=DocumentTermMatrix(corpus1,control=list(wordLenghts=c(1,Inf))))
words.dtm12=removeSparseTerms(words.dtm1,sparse=0.9)
dtm_matrix1=as.data.frame(inspect(words.dtm12))
dist.dtm1<- dissimilarity(words.dtm12, method = "cosine")
job=hclust(dist(dtm_matrix[1:300,]),method="average")
plot(job)




#简单分析#
barplot(D,col=rainbow(2))
barplot(E,col=rainbow(3))
names(E)=c("本科","博士","初中","大专","高中","硕士","不限","中专")


#问题3#
data_an=read.csv("E:/T3AN.csv",T)
data_wa=read.csv("E:/T3WA.csv",T)
data_time=read.csv("E:/time.csv",T)
library(tseries)
#时间序列分析接下来的需求情况#
watime=as.data.frame(summary(data_wa$CreateTime))
matplot(data_time,type="l",lty=2,col=2,xlab="Time",ylab="Job Amount")
acf(data_time$AMOUNT)
pacf(data_time$AMOUNT)
Box.test(data_time$AMOUNT, type="Ljung-Box",lag=6)  
m1=arima(data_time$AMOUNT, order = c(1,0,0),method="ML") 
summary(m1)
r=m1$residuals
Box.test(r,type="Ljung-Box",lag=6, fitdf=1)
prop.fore = predict(m1, n.ahead =5)
as.data.frame(prop.fore)#predict#
#简单的分析#
#city#
C3=read.csv("E:/CC3.csv",F)
c3=C3$V2
n=as.vector(C3$V1)
names(c3)=n
barplot(c3,col=rainbow(2))
#education#
e3=as.vector(summary(data_an$Education))
names(e3)=c("本科","博士","大专","硕士","不限")
barplot(e3,col=rainbow(3))
#positiontype#
PP3=as.vector(summary(data_an$PositionType))

P3=read.csv("E:/P3.csv",F)
na=as.vector(P3$V1)
names(PP3)=na
barplot(PP3,col=rainbow(3))
plot(PP3,col=rainbow(2))

pf3=as.vector(summary(data_an$PositionFirstType))
names(pf3)=c("1","产品","技术","金融","设计","营销","运营","职能")
barplot(pf3,col=rainbow(2))

#数据挖掘#
wa_time=read.csv("E:/WATIME.csv",T)
matplot(wa_time,type="l",lty=2,col=2,xlab="Time",ylab="Job Amount")
acf(wa_time$AMOUT)
pacf(wa_time$AMOUT)
Box.test(wa_time$AMOUT, type="Ljung-Box",lag=6)  
m2=arima(wa_time$AMOUT, order = c(1,0,0),method="ML") 
summary(m2)
r1=m2$residuals
Box.test(r1,type="Ljung-Box",lag=6, fitdf=1)
prop.fore = predict(m2, n.ahead =5)
as.data.frame(prop.fore)#predict#
#简单的分析#
#city#
CC3=as.data.frame(summary(data_wa$City))

CC=read.csv("E:/CC3.csv",F)
barplot(cc,col=rainbow(2))
#education#
E3=as.data.frame(summary(data_wa$Education))
E3=as.vector(summary(data_wa$Education))
names(E3)=c("本科","博士","大专","硕士","不限")
barplot(E3,col=rainbow(3))
#positiontype#
PP3=as.data.frame(summary(data_wa$PositionType))
PP3=as.vector(summary(data_wa$PositionType))

P3=read.csv("E:/ee3.csv",F)
na=as.vector(P3$V1)
names(PP3)=na
barplot(PP3,col=rainbow(3))


pf3=as.vector(summary(data_wa$PositionFirstType))
names(pf3)=c("1","产品","技术","金融","设计","营销","运营","职能")
barplot(pf3,col=rainbow(2))

