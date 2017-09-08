library(NLP)
library(tm)
library(rJava)
library(Rwordseg)
library(RColorBrewer)
library(wordcloud)
library(tmcn)
setwd("F:\\ѧϰ\\2016̩�ϱ�\\C�⣺����")
mydata1<- read.table(file="hh.csv", header = TRUE, sep = ',',  stringsAsFactors = FALSE)#��������#
diff<- read.table(file="�ʵ�.txt", header = TRUE, sep = ',',  stringsAsFactors = FALSE)#����ʵ�#
str(mydata1)
str(diff)
class(diff)
#��Ҫ�����ݿ��ʽ������ת��Ϊ������ʽ
diff<- as.vector(diff[,1])
mydata1$name[1]
n=sum(is.na(mydata1$name))
n
insertWords(diff)
segword <- segmentCN(strwords = mydata1$name)
#�鿴��һ�����ŷִʽ��
segword[[1]]

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
segword2 <- sapply(X = segword, FUN = removewords, mystopwords)
segword2[[1]]
#��������ͼ
word_freq <- getWordFreq(string = unlist(segword2))
opar <- par(no.readonly = TRUE)
par(bg = 'black')
#���Ƴ���Ƶ����ߵ�ǰ50����
wordcloud(words = word_freq$Word, freq = word_freq$Freq, max.words = 50, random.color = TRUE, colors = rainbow(n = 7))
par(opar)

v = table(unlist(segword2)) #����ÿ�����ʵĴ�Ƶ
v = sort(v, decreasing = T) #����������
d = data.frame(segword2 = names(v), freq = v) #����Ƶ����ת��Ϊ���ݿ��ʽ
d$segword2=as.character(d$segword2) #�������ֶι���Ϊ�ַ�����ʽ
rbind(d[nchar(d$segword2)==1,][1:10,],d[nchar(d$segword2)==2,][1:20,],d[nchar(d$segword2)>=3,][1:20,])->result_r #��ȡ��ͬ�����ĵ����д�Ƶ��ߵ�TOP50���ʣ���Ϊ���ƻ��Ƶ��ز�
write.table(result_r,file="E:/�޸�̩��",sep=",",row.names = F,quote = F)