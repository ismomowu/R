install.packages("rJava")
library("rJava")

library(rvest)
library(plyr)
library(dplyr)
library(stringr)


#==============================for====================================

i=1
list<-list()  
content<-list()
head<-list()
date<-list()

CVS<-data_frame("標題"=NA,"內容+推文"=NA)
                #,"日期"=NA)
i=1

for (i in c(1:1671)) {

  surl = paste0("https://www.ptt.cc/bbs/CVS/index", i,".html")
  ptt = read_html(surl,encoding="UTF-8")
  ptt %>% iconv(from = "utf-8", to = "UTF-8")
  reply = ptt %>% html_nodes(".nrec")%>% html_text("hl f2") %>% iconv(from = "UTF-8", to = "UTF-8")
  #title= ptt %>% html_nodes(".title a")%>% html_text() %>% iconv(from = "UTF-8", to = "UTF-8")
  texturl=ptt%>% html_nodes(".title a")%>% html_attr("href")
  j=1
  for (j in c(1:20)){
    urllink <-read_html(paste0("https://www.ptt.cc",texturl[j]),encoding="UTF-8")
    title=urllink%>%html_nodes("#main-content")%>%html_nodes("div:nth-child(3)")%>%html_text("span.article-meta-value")%>%iconv(from = "UTF-8", to = "UTF-8")
    title=gsub("標題","",title)
    text = urllink%>%html_nodes("#main-container")%>%html_text("a")%>% iconv(from = "UTF-8", to = "UTF-8")
    StartName<- regexpr("時間", text)
    EndName<- regexpr("※ 發信站:", text)
    text=substr(text,StartName+28,EndName-6) 
    Push=str_c(head(urllink %>% html_nodes(".push")%>% html_nodes(".f3.push-content")%>% html_text() %>% iconv(from = "UTF-8", to = "UTF-8")),collapse = "")
    Push=gsub(":","",Push)
    content=paste0(text,Push)
    #Time=urllink%>%html_nodes("#main-content")%>%html_nodes("div:nth-child(4)")%>%html_text("span.article-meta-value")%>% iconv(from = "UTF-8", to = "UTF-8")
    CVS1<-data_frame("標題"=title,"內容+推文"=content)
    CVS<-rbind(CVS, CVS1)
    j<-j+1
    }
  i<-i+1
}

#all_text<-as.character(CVS[,2]) #content'
all_text<-as.character(CVS[,1]) #title'

#========================================清資料====================================
install.packages('tm')
library('tm')

doctxt<-Corpus(VectorSource(all_text)) 

doctxt=tm_map(doctxt,PlainTextDocument)
doctxt=tm_map(doctxt,stripWhitespace)# 去除空格
doctxt=tm_map(doctxt,tolower) #将内容转换成小写
doctxt=tm_map(doctxt, removeWords, stopwords("English"))#remove stopwords
doctxt=tm_map(doctxt, removePunctuation)
doctxt=tm_map(doctxt,stemDocument)
doctxt=tm_map(doctxt,stripWhitespace)
doctxt=tm_map(doctxt,removeNumbers)


#===========================斷字1==============================
#install.packages("Rwordseg", repos = "http://R-Forge.R-project.org", type = "source")
install.packages("~/Downloads/Rwordseg_0.2-1.tar", repos=NULL, type="source")  
library("Rwordseg")


segment.options(isNameRecognition=TRUE)
dm<-segmentCN(as.character(doctxt[[1]][1]))
fre_txt<-as.data.frame(table(dm[1]))


#========================================斷詞2====================================
#使用 jiebaR 作中文斷詞
library(jiebaR)


doc<-as.character(doctxt[[1]][1])
doc<-gsub("[0-9a-zA-Z]+?","",doc)###去除数字和英文




Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
cc = worker()
dtm<-cc[doc]
stopwords<-c('的','心得','好','是','嗎','re','便利','商店','便利商店','問題','啊','我們','我','cvs','討論','了','有','在','要','被','請問')
dtm<-as.character(filter_segment(dtm,stopwords))

ptt <-table(cc[dtm])
ptt<-data.frame(ptt)

head(ptt[order(ptt$Freq,decreasing = TRUE),])

setwd('~/Documents/R_Collection/ptt_CVS scrapy')
write.csv(ptt,file="pttfreq",fileEncoding = "UTF-8")


#===========================文字雲===============================

library(wordcloud)
library(RColorBrewer)


library(wordcloud)
library(RColorBrewer)


m1 <- as.matrix(ptt)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
par(family=("Heiti TC Light"))
pal2 <- brewer.pal(8,"Dark2")
wordcloud(ptt[[1]], ptt[[2]], scale=c(6.5,0.1),size=8,min.freq =500, random.order = F, ordered.colors = F,
          ,colors =brewer.pal(8,"Dark2"), gridSize=-1,ellipticity= 0.14)


'png("wordcloud_pttcvs.png", width=1280,height=800)
wordcloud(ptt[[1]], ptt[[2]], 
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)'



#================================贅字庫====================================


stopwords<-c('的','心得','好','是','嗎','re','便利','商店','便利商店','問題','啊','我們','我','cvs','討論','了','有','在','要','被','請問')



