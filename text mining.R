setwd("E://修改泰迪")
jobdescr=scan("jobdescr.csv",what="charactor",sep=',',skip=1)
evens<-seq(1,100,by=2)#50个数，间隔2个，单数#
odds<-evens+1#偶数#
jobid<-jobdescr[evens]
weibo1<-jobdescr[odds]
weibo2<-weibo1[weibo1!=""]
weibo<-weibo2[-45]


library(Rcpp)
library(jiebaR)
cutter= worker()
weibo_segment=cutter[weibo]
filter_words = c("我","你","它","大家") 
filter_segment(weibo_segment,filter_words) 
cutter=worker(bylines = TRUE) 
weibo_segment_line=cutter[weibo]
cutter$write 
cutter["files.path"] 


#可以使用 vector_keywords 对一个文本向量提取关键词。  
keyworker=worker("keywords",topn=2)#关键词个数
cutter = worker() 
vector_keywords(cutter[weibo],keyworker)
#可以使用 vector_keywords 对一个文本向量提取关键词。


keyworker=worker("keywords",topn=5)#关键词个数
cutter = worker() 
for(i in 1:length(weibo)){
str1<-weibo[i]
print(vector_keywords(cutter[str1],keyworker))
}
mixseg = worker()
mixseg[weibo]
## 相当于 segment(weibo,mixseg )  
## 或者mixseg<=weibo
#词性
tagger=worker("tag") 
tagger[weibo] 
#Simhash 与海明距离
simhasher=worker("simhash",topn=2) #关键词的simhash距离
simhasher<=weibo[1]
distance(weibo[1],weibo[2], simhasher)
##快速模式
qseg[weibo]

library(tm)
weibo_ovid=Corpus(VectorSource(weibo_segment_line))
summary(weibo_ovid)

inspect(weibo_ovid[1])

library(SnowballC)

weibo_ovid<-tm_map(weibo_ovid,removeNumbers)#去除数字
weibo_ovid<-tm_map(weibo_ovid,stripWhitespace)#去除多余空格
weibo_ovid<-tm_map(weibo_ovid,removePunctuation)#去除标点符号
weibo_ovid<-tm_map(weibo_ovid,removeWords, stopwords("english"))#将英文中的停词删掉：例如把that　at　等英文介词去掉。
weibo_ovid<-tm_map(weibo_ovid,PlainTextDocument)#去掉空文件

dtm<-DocumentTermMatrix(weibo_ovid,control=list(dictionary=as.character(weibo_segment),removePunctuation =TRUE,stopwords=TRUE, wordLengths = c(1, Inf)))
# Punctuation是否去掉标点符号默认falseremoveNumbers是否去掉数字默认false，
#dictionary设置要统计的中文词语，如果不设置的话，默认会从所有的语料库里统计
#wordLengths设置如果词的长度大于X时舍去。
dtm2=removeSparseTerms(dtm, sparse=0.9)
df_dtm2<-as.data.frame(inspect(dtm2))#将词频矩阵转换为数据框格式得到

data.frame(inspect(DocumentTermMatrix(weibo_ovid)))
##文档聚类
d <- dist(dtm2, method = "euclidean")
fit <- hclust(d, method="ward.D")
fit$labels<-jobid[1:47]
plot(fit)

library(skmeans)
v.dist<-skmeans_xdist(dtm2) 
fit2<-skmeans(v.dist,k=7)
names(fit2)
weibo$cla<-as.vector(fit2$cluster)



