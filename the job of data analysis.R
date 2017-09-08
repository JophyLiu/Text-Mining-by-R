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
