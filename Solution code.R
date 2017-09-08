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


#������#
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




#�򵥷���#
barplot(D,col=rainbow(2))
barplot(E,col=rainbow(3))
names(E)=c("����","��ʿ","����","��ר","����","˶ʿ","����","��ר")
setwd("E://�޸�̩��")
jobdescr=read.csv("E:/�޸�̩��/���ݷ���ʦ.csv",T)
a=as.character(jobdescr$Job_Description)

library(Rcpp)
library(jiebaR)
cutter= worker()
a_segment=cutter[a]
filter_words = c("��","��","��","���") 
filter_segment(a_segment,filter_words) 
cutter=worker(bylines = TRUE) 
a_segment_line=cutter[a]
cutter$write 
cutter["files.path"] 

#����ʹ�� vector_keywords ��һ���ı�������ȡ�ؼ��ʡ�  
keyworker=worker("keywords",topn=2)#�ؼ��ʸ���
cutter = worker() 
vector_keywords(cutter[a],keyworker)

#����ʹ�� vector_keywords ��һ���ı�������ȡ�ؼ��ʡ�
keyworker=worker("keywords",topn=5)#�ؼ��ʸ���
cutter = worker() 
for(i in 1:length(a)){
  str1<-a[i]
  print(vector_keywords(cutter[str1],keyworker))
}
mixseg = worker()
mixseg[a]
## �൱�� segment(a,mixseg )  
## ����mixseg<=a

#����
tagger=worker("tag") 
tagger[a] 

#Simhash �뺣������
simhasher=worker("simhash",topn=2) #�ؼ��ʵ�simhash����
simhasher<=a[1]
distance(a[1],a[2], simhasher)


##����ģʽ
qseg[a]

library(tm)
a_ovid=Corpus(VectorSource(a_segment_line))
summary(a_ovid)

inspect(a_ovid[1])

library(SnowballC)

a_ovid<-tm_map(a_ovid,removeNumbers)#ȥ������
a_ovid<-tm_map(a_ovid,stripWhitespace)#ȥ������ո�
a_ovid<-tm_map(a_ovid,removePunctuation)#ȥ��������
a_ovid<-tm_map(a_ovid,removeWords, stopwords("english"))#��Ӣ���е�ͣ��ɾ���������that��at����Ӣ�Ľ��ȥ����
a_ovid<-tm_map(a_ovid,PlainTextDocument)#ȥ�����ļ�

dtm<-DocumentTermMatrix(a_ovid,control=list(dictionary=as.character(a_segment),removePunctuation =TRUE,stopwords=TRUE, wordLengths = c(1, Inf)))
# Punctuation�Ƿ�ȥ��������Ĭ��falseremoveNumbers�Ƿ�ȥ������Ĭ��false��
#dictionary����Ҫͳ�Ƶ����Ĵ����������õĻ���Ĭ�ϻ�����е����Ͽ���ͳ��
#wordLengths��������ʵĳ��ȴ���Xʱ��ȥ��
dtm2=removeSparseTerms(dtm, sparse=0.9)
df_dtm2<-as.data.frame(inspect(dtm2))#����Ƶ����ת��Ϊ���ݿ��ʽ�õ�

data.frame(inspect(DocumentTermMatrix(a_ovid)))

##�ĵ�����
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

#����ֹͣ��
mystopwords <- read.table(file = file.choose(), stringsAsFactors = FALSE)
head(mystopwords)
class(mystopwords)
#��Ҫ�����ݿ��ʽ������ת��Ϊ������ʽ
mystopwords <- as.vector(mystopwords[,1])
head(mystopwords)
#�Զ���ɾ��ֹͣ�ʵĺ���
removewords <- function(target_words,stop_words){
  target_words = target_words[target_words%in%stop_words==FALSE]
  return(target_words)
}
segword <- sapply(X = a_ovid, FUN = removewords, mystopwords)
segword[[1]]
#��������ͼ
word_freq <- getWordFreq(string = unlist(segword))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#���Ƴ���Ƶ����ߵ�ǰ50����
wordcloud(words = word_freq$Word, freq = word_freq$Freq, max.words = 50, random.color = TRUE, colors = rainbow(n = 7))
par(opar)

v = table(unlist(segword)) #����ÿ�����ʵĴ�Ƶ
v = sort(v, decreasing = T) #����������
d = data.frame(segword = names(v), freq = v) #����Ƶ����ת��Ϊ���ݿ��ʽ
d$segword=as.vector(d$segword) #�������ֶι���Ϊ�ַ�����ʽ
rbind(d[nchar(d$segword)==1,][1:10,],d[nchar(d$segword)==2,][1:20,],d[nchar(d$segword)>=3,][1:20,])->result_r #��ȡ��ͬ�����ĵ����д�Ƶ��ߵ�TOP50���ʣ���Ϊ���ƻ��Ƶ��ز�
write.table(result_r,file="E:/�޸�̩��/ciyun.csv",sep=",",row.names = F,quote = F)

#����3#
data_an=read.csv("E:/T3AN.csv",T)
data_wa=read.csv("E:/T3WA.csv",T)
data_time=read.csv("E:/time.csv",T)
library(tseries)
#ʱ�����з������������������#
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
#�򵥵ķ���#
#city#
C3=read.csv("E:/CC3.csv",F)
c3=C3$V2
n=as.vector(C3$V1)
names(c3)=n
barplot(c3,col=rainbow(2))
#education#
e3=as.vector(summary(data_an$Education))
names(e3)=c("����","��ʿ","��ר","˶ʿ","����")
barplot(e3,col=rainbow(3))
#positiontype#
PP3=as.vector(summary(data_an$PositionType))

P3=read.csv("E:/P3.csv",F)
na=as.vector(P3$V1)
names(PP3)=na
barplot(PP3,col=rainbow(3))
plot(PP3,col=rainbow(2))

pf3=as.vector(summary(data_an$PositionFirstType))
names(pf3)=c("1","��Ʒ","����","����","���","Ӫ��","��Ӫ","ְ��")
barplot(pf3,col=rainbow(2))

#�����ھ�#
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
#�򵥵ķ���#
#city#
CC3=as.data.frame(summary(data_wa$City))

CC=read.csv("E:/CC3.csv",F)
barplot(cc,col=rainbow(2))
#education#
E3=as.data.frame(summary(data_wa$Education))
E3=as.vector(summary(data_wa$Education))
names(E3)=c("����","��ʿ","��ר","˶ʿ","����")
barplot(E3,col=rainbow(3))
#positiontype#
PP3=as.data.frame(summary(data_wa$PositionType))
PP3=as.vector(summary(data_wa$PositionType))

P3=read.csv("E:/ee3.csv",F)
na=as.vector(P3$V1)
names(PP3)=na
barplot(PP3,col=rainbow(3))


pf3=as.vector(summary(data_wa$PositionFirstType))
names(pf3)=c("1","��Ʒ","����","����","���","Ӫ��","��Ӫ","ְ��")
barplot(pf3,col=rainbow(2))

a=read.csv("E:/1.csv",F)
a=as.vector(a$V1)
names(a)
names(a) <- c("��ʿ","˶ʿ","����","��ר","����","����")
pie(a,main = "ѧ����ͼ") 

a=read.csv("E:/1.csv",F)         
b=as.vector(a$V1)
names(b)
names(b)=as.vector(a$V2)
pie(b,main = "ְλ��ͼ") 

a=read.csv("E:/1.csv",F)         
matplot(a$V2,type="l",lty=2,col=2,xlab="Time",ylab="IT Job Amount")