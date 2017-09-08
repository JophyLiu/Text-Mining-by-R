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

