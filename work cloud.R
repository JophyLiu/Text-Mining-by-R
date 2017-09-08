library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(RColorBrewer)
library(wordcloud)
library(tmcn)
setwd("F:\\学习\\2016泰迪杯\\C题：附件")
mydata1<- read.table(file="hh.csv", header = TRUE, sep = ',',  stringsAsFactors = FALSE)#导入数据#
diff<- read.table(file="词典.txt", header = TRUE, sep = ',',  stringsAsFactors = FALSE)#导入词典#
str(mydata1)
str(diff)
class(diff)
#需要将数据框格式的数据转化为向量格式
diff<- as.vector(diff[,1])
mydata1$name[1]
n=sum(is.na(mydata1$name))
n
insertWords(diff)
segword <- segmentCN(strwords = mydata1$name)
#查看第一条新闻分词结果
segword[[1]]

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
segword2 <- sapply(X = segword, FUN = removewords, mystopwords)
segword2[[1]]
#绘制文字图
word_freq <- getWordFreq(string = unlist(segword2))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#绘制出现频率最高的前50个词
wordcloud(words = word_freq$Word, freq = word_freq$Freq, max.words = 50, random.color = TRUE, colors = rainbow(n = 7))
par(opar)

v = table(unlist(segword2)) #计算每个单词的词频
v = sort(v, decreasing = T) #按降序排列
d = data.frame(segword2 = names(v), freq = v) #将词频矩阵转换为数据框格式
d$segword2=as.character(d$segword2) #将单词字段规整为字符串格式
rbind(d[nchar(d$segword2)==1,][1:10,],d[nchar(d$segword2)==2,][1:20,],d[nchar(d$segword2)>=3,][1:20,])->result_r #提取不同字数的单词中词频最高的TOP50单词，作为词云绘制的素材
write.table(result_r,file="E:/修改泰迪",sep=",",row.names = F,quote = F)
