setwd("E://�޸�̩��")
jobdescr=scan("jobdescr.csv",what="charactor",sep=',',skip=1)
evens<-seq(1,100,by=2)#50���������2��������#
odds<-evens+1#ż��#
jobid<-jobdescr[evens]
weibo1<-jobdescr[odds]
weibo2<-weibo1[weibo1!=""]
weibo<-weibo2[-45]


library(Rcpp)
library(jiebaR)
cutter= worker()
weibo_segment=cutter[weibo]
filter_words = c("��","��","��","���") 
filter_segment(weibo_segment,filter_words) 
cutter=worker(bylines = TRUE) 
weibo_segment_line=cutter[weibo]
cutter$write 
cutter["files.path"] 


#����ʹ�� vector_keywords ��һ���ı�������ȡ�ؼ��ʡ�  
keyworker=worker("keywords",topn=2)#�ؼ��ʸ���
cutter = worker() 
vector_keywords(cutter[weibo],keyworker)
#����ʹ�� vector_keywords ��һ���ı�������ȡ�ؼ��ʡ�


keyworker=worker("keywords",topn=5)#�ؼ��ʸ���
cutter = worker() 
for(i in 1:length(weibo)){
str1<-weibo[i]
print(vector_keywords(cutter[str1],keyworker))
}
mixseg = worker()
mixseg[weibo]
## �൱�� segment(weibo,mixseg )  
## ����mixseg<=weibo
#����
tagger=worker("tag") 
tagger[weibo] 
#Simhash �뺣������
simhasher=worker("simhash",topn=2) #�ؼ��ʵ�simhash����
simhasher<=weibo[1]
distance(weibo[1],weibo[2], simhasher)
##����ģʽ
qseg[weibo]

library(tm)
weibo_ovid=Corpus(VectorSource(weibo_segment_line))
summary(weibo_ovid)

inspect(weibo_ovid[1])

library(SnowballC)

weibo_ovid<-tm_map(weibo_ovid,removeNumbers)#ȥ������
weibo_ovid<-tm_map(weibo_ovid,stripWhitespace)#ȥ������ո�
weibo_ovid<-tm_map(weibo_ovid,removePunctuation)#ȥ��������
weibo_ovid<-tm_map(weibo_ovid,removeWords, stopwords("english"))#��Ӣ���е�ͣ��ɾ���������that��at����Ӣ�Ľ��ȥ����
weibo_ovid<-tm_map(weibo_ovid,PlainTextDocument)#ȥ�����ļ�

dtm<-DocumentTermMatrix(weibo_ovid,control=list(dictionary=as.character(weibo_segment),removePunctuation =TRUE,stopwords=TRUE, wordLengths = c(1, Inf)))
# Punctuation�Ƿ�ȥ��������Ĭ��falseremoveNumbers�Ƿ�ȥ������Ĭ��false��
#dictionary����Ҫͳ�Ƶ����Ĵ����������õĻ���Ĭ�ϻ�����е����Ͽ���ͳ��
#wordLengths��������ʵĳ��ȴ���Xʱ��ȥ��
dtm2=removeSparseTerms(dtm, sparse=0.9)
df_dtm2<-as.data.frame(inspect(dtm2))#����Ƶ����ת��Ϊ���ݿ��ʽ�õ�

data.frame(inspect(DocumentTermMatrix(weibo_ovid)))
##�ĵ�����
d <- dist(dtm2, method = "euclidean")
fit <- hclust(d, method="ward.D")
fit$labels<-jobid[1:47]
plot(fit)

library(skmeans)
v.dist<-skmeans_xdist(dtm2) 
fit2<-skmeans(v.dist,k=7)
names(fit2)
weibo$cla<-as.vector(fit2$cluster)


