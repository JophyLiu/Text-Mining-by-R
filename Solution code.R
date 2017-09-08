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
setwd("E://修改泰迪")
jobdescr=read.csv("E:/修改泰迪/数据分析师.csv",T)
a=as.character(jobdescr$Job_Description)

library(Rcpp)
library(jiebaR)
cutter= worker()
a_segment=cutter[a]
filter_words = c("我","你","它","大家") 
filter_segment(a_segment,filter_words) 
cutter=worker(bylines = TRUE) 
a_segment_line=cutter[a]
cutter$write 
cutter["files.path"] 

#可以使用 vector_keywords 对一个文本向量提取关键词。  
keyworker=worker("keywords",topn=2)#关键词个数
cutter = worker() 
vector_keywords(cutter[a],keyworker)

#可以使用 vector_keywords 对一个文本向量提取关键词。
keyworker=worker("keywords",topn=5)#关键词个数
cutter = worker() 
for(i in 1:length(a)){
  str1<-a[i]
  print(vector_keywords(cutter[str1],keyworker))
}
mixseg = worker()
mixseg[a]
## 相当于 segment(a,mixseg )  
## 或者mixseg<=a

#词性
tagger=worker("tag") 
tagger[a] 

#Simhash 与海明距离
simhasher=worker("simhash",topn=2) #关键词的simhash距离
simhasher<=a[1]
distance(a[1],a[2], simhasher)


##快速模式
qseg[a]

library(tm)
a_ovid=Corpus(VectorSource(a_segment_line))
summary(a_ovid)

inspect(a_ovid[1])

library(SnowballC)

a_ovid<-tm_map(a_ovid,removeNumbers)#去除数字
a_ovid<-tm_map(a_ovid,stripWhitespace)#去除多余空格
a_ovid<-tm_map(a_ovid,removePunctuation)#去除标点符号
a_ovid<-tm_map(a_ovid,removeWords, stopwords("english"))#将英文中的停词删掉：例如把that　at　等英文介词去掉。
a_ovid<-tm_map(a_ovid,PlainTextDocument)#去掉空文件

dtm<-DocumentTermMatrix(a_ovid,control=list(dictionary=as.character(a_segment),removePunctuation =TRUE,stopwords=TRUE, wordLengths = c(1, Inf)))
# Punctuation是否去掉标点符号默认falseremoveNumbers是否去掉数字默认false，
#dictionary设置要统计的中文词语，如果不设置的话，默认会从所有的语料库里统计
#wordLengths设置如果词的长度大于X时舍去。
dtm2=removeSparseTerms(dtm, sparse=0.9)
df_dtm2<-as.data.frame(inspect(dtm2))#将词频矩阵转换为数据框格式得到

data.frame(inspect(DocumentTermMatrix(a_ovid)))

##文档聚类
d <- dist(dtm2, method = "euclidean")
fit <- hclust(d, method="ward.D")
fit$labels<-jobdescr$PositionId
plot(fit)


library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(RColorBrewer)
library(wordcloud)
library(tmcn)

#创建停止词
mystopwords <- read.table(file = file.choose(), stringsAsFactors = FALSE)
head(mystopwords)
class(mystopwords)
#需要将数据框格式的数据转化为向量格式
mystopwords <- as.vector(mystopwords[,1])
head(mystopwords)
#自定义删除停止词的函数
removewords <- function(target_words,stop_words){
  target_words = target_words[target_words%in%stop_words==FALSE]
  return(target_words)
}
segword <- sapply(X = a_ovid, FUN = removewords, mystopwords)
segword[[1]]
#绘制文字图
word_freq <- getWordFreq(string = unlist(segword))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#绘制出现频率最高的前50个词
wordcloud(words = word_freq$Word, freq = word_freq$Freq, max.words = 50, random.color = TRUE, colors = rainbow(n = 7))
par(opar)

v = table(unlist(segword)) #计算每个单词的词频
v = sort(v, decreasing = T) #按降序排列
d = data.frame(segword = names(v), freq = v) #将词频矩阵转换为数据框格式
d$segword=as.vector(d$segword) #将单词字段规整为字符串格式
rbind(d[nchar(d$segword)==1,][1:10,],d[nchar(d$segword)==2,][1:20,],d[nchar(d$segword)>=3,][1:20,])->result_r #提取不同字数的单词中词频最高的TOP50单词，作为词云绘制的素材
write.table(result_r,file="E:/修改泰迪/ciyun.csv",sep=",",row.names = F,quote = F)

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

a=read.csv("E:/1.csv",F)
a=as.vector(a$V1)
names(a)
names(a) <- c("博士","硕士","本科","大专","高中","不限")
pie(a,main = "学历饼图") 

a=read.csv("E:/1.csv",F)         
b=as.vector(a$V1)
names(b)
names(b)=as.vector(a$V2)
pie(b,main = "职位饼图") 

a=read.csv("E:/1.csv",F)         
matplot(a$V2,type="l",lty=2,col=2,xlab="Time",ylab="IT Job Amount")
