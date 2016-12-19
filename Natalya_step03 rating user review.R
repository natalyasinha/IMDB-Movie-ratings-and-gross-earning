library(tidyverse)
library(magrittr)
library(dplyr)
library(rvest)
## Change it to your current location

My.Location <- ""
setwd(My.Location)

### Create two folders in the above location Foldernames should be Top10 and Bottom10

# Load data
sentiment.analysis.nonmissing <- read_csv('movie_color_nonmissing.csv')
sentiment.analysis <- read_csv('movie_color.csv')

head(sentiment.analysis.nonmissing$movie_imdb_link)

#################### Top 10 movies ##########################
top10_movies <- sentiment.analysis.nonmissing %>% arrange(desc(imdb_score)) %>% head(n = 10)
# top10_movies <- top10_movies[c(1:4,6:11),]
top10_movies$movie_title <- factor(top10_movies$movie_title, levels=unique(top10_movies$movie_title))
top10_movies$gross <- round(top10_movies$gross/1000000,digits=2)

top10_movies %>%
  ggplot(aes(x=movie_title,y=imdb_score,label=format(imdb_score, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=imdb_score), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="",
       title="Top 10 rated Movies in MM")


#################### Bottom 10 movies ##########################
bottom10_movies <- sentiment.analysis.nonmissing %>% arrange((imdb_score)) %>% head(n = 10) 
# bottom10_movies <- bottom10_movies[c(1:8,10:11),]
bottom10_movies$movie_title <- factor(bottom10_movies$movie_title, levels=unique(bottom10_movies$movie_title))
bottom10_movies$gross <- round(bottom10_movies$gross/1000,digits=2)

bottom10_movies %>%
  ggplot(aes(x=movie_title,y=imdb_score,label=format(imdb_score, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=imdb_score), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="",
       title="Bottom 10 rated Movies in MM")



top10_movies$IMDB.ID <- substr(top10_movies$movie_imdb_link, 29, 35)
bottom10_movies$IMDB.ID <- substr(bottom10_movies$movie_imdb_link, 29, 35)

## Remove last 8 special characters from the movie title ###
top10_movies$movie_title <- gsub('.{8}$', '', top10_movies$movie_title)
bottom10_movies$movie_title <- gsub('.{8}$', '', bottom10_movies$movie_title)
# for(i in 1:10){
#   URL <- paste0("http://www.imdb.com/title/tt", top10_movies[i,"IMDB.ID"], "/reviews?filter=prolific")
#   browseURL(URL)
# }
# 
# for(i in 1:10){
#   URL <- paste0("http://www.imdb.com/title/tt", bottom10_movies[i,"IMDB.ID"], "/reviews?filter=prolific")
#   browseURL(URL)
# }

top10_movies$no.of.pages.of.review <- c(429,475,321,224,283,508,301,243,367,79)
bottom10_movies$no.of.pages.of.review <- c(24,37,13,5,31,31,31,24,67,53)



# install.packages(c("tm","wordcloud","tidytext"))  ## only run once
library(tm)
library(tidytext)
library(wordcloud)
library(forcats)
library(gtools)
library(dplyr)


## Web scrapping of crtitic reviews for Top 10 movies##
for(i in 1:10){
  if(i==1){
    document.list <- NULL
    plot_list <- NULL
    document.df.top10 <- NULL
    rownames(document.df.top10) <- NULL
  }
  ID <- top10_movies[i,"IMDB.ID"]
  movie.name <- top10_movies[i,"movie_title"]
  No.of.pages <- as.numeric(top10_movies[i,"no.of.pages.of.review"])
  for(j in 1:No.of.pages){
    if (j==1){
      ex_review <- NULL
      URL <- paste0("http://www.imdb.com/title/tt", ID, "/reviews?filter=chrono")
      MOVIE_URL <- read_html(URL)
      ex_review <- MOVIE_URL %>% html_nodes("#pagecontent") %>%
        html_nodes("div+ p") %>%
        html_text()
    }
    else {
      URL <- paste0("http://www.imdb.com/title/tt", ID, "/reviews?filter=chrono;filter=chrono;start=", (j-1)*10)
      ex_review_additional <- MOVIE_URL %>% html_nodes("#pagecontent") %>%
        html_nodes("div+ p") %>%
        html_text()
      ex_review <- c(ex_review,ex_review_additional)
    }
  }
  
  document.list = list(movie.name, ex_review)
  document.df <- as.data.frame(document.list)
  colnames(document.df) <- c("movie.name","ex_review")
  
  document.df.top10 <- rbind(document.df.top10, document.df)
  
  text.c <- VCorpus(DataframeSource(dplyr::select(document.df,ex_review)))
  DTM <- DocumentTermMatrix(text.c,
                            control=list(removePunctuation=TRUE,
                                         wordLengths=c(3, Inf),
                                         stopwords=TRUE,
                                         stemming=TRUE,
                                         removeNumbers=TRUE
                            ))
  DTM <- removeSparseTerms(DTM,0.995)
  DTM <- tidy(DTM)
  
  term.count <- DTM %>%
    group_by(term) %>%
    summarize(n.total=sum(count)) %>%
    arrange(desc(n.total))
  
  
  mypath <- file.path(My.Location,"/Top10 user review",paste("frequency_plot_",i, ".jpg", sep = ""))
  
  jpeg(file=mypath)
  mytitle = paste("Most Frequent Terms for Movie - ", movie.name)
  p <- term.count %>%
    slice(1:30)
  p1 <- ggplot(data=p,aes(x=fct_reorder(term,n.total),y=n.total)) + geom_bar(stat='identity') +
    coord_flip() + xlab('Counts') + ylab('')+ggtitle(mytitle)
  print(p1)
  dev.off()
  
  term.count.pop <- term.count %>%
    slice(5:100)
  
  #wordcloud(term.count.pop$term, term.count.pop$n.total, scale=c(5,.5))
  
  mypath <- file.path(My.Location,"/Top10 user review",paste("wordcloud_",i, ".jpg", sep = ""))
  
  jpeg(file=mypath)
  mytitle = paste("WordCloud for Movie", movie.name)
  wordcloud(term.count.pop$term, term.count.pop$n.total,max.words =80,min.freq=3, scale=c(5,.5),
            random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
  dev.off()

}


meta.data.movie <- document.df.top10 %>%
  dplyr::select(movie.name) %>%
  rownames_to_column(var="document")

text.c.movie <- VCorpus(DataframeSource(dplyr::select(document.df.top10,ex_review)))
DTM <- DocumentTermMatrix(text.c.movie,
                          control=list(removePunctuation=TRUE,
                                       wordLengths=c(3, Inf),
                                       stopwords=TRUE,
                                       stemming=TRUE,
                                       removeNumbers=TRUE
                          ))

reviews.tidy <- tidy(removeSparseTerms(DTM,0.999))

str(reviews.tidy)
str(meta.data.movie)
term.movies <- reviews.tidy %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name) %>%
  summarize(n.reviews=sum(count))

## sentiments by movie 

bing <- get_sentiments("bing") 

movie.sentiment <- reviews.tidy %>%
  inner_join(bing,by=c("term"="word")) %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name,sentiment) %>%
  summarize(total=sum(count)) %>%
  inner_join(term.movies,by='movie.name') %>%
  mutate(relative.sentiment = total/n.reviews)

mypath <- file.path(My.Location,"/Top10 user review",paste("Sentiment Analysis Top 10.jpg", sep = ""))

jpeg(file=mypath)
p <- movie.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~movie.name)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
dev.off()

mypath <- file.path(My.Location,"/Top10 user review",paste("Net Positive Sentiment Analysis Top 10.jpg", sep = ""))
jpeg(file=mypath)
p <- movie.sentiment %>%
  dplyr::select(sentiment,relative.sentiment,movie.name) %>%
  spread(sentiment,relative.sentiment) %>%
  mutate(net.pos = positive-negative) %>%
  ggplot(aes(x=fct_reorder(movie.name,net.pos),y=net.pos*100)) + geom_point(size=4) + coord_flip()+
  ylab('Net Positive Sentiment')+xlab('Movies - Bottom 10')
print(p)
dev.off()


nrc <- get_sentiments("nrc")
movie.sentiment <- reviews.tidy %>%
  inner_join(nrc,by=c("term"="word")) %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name,sentiment) %>%
  summarize(total=sum(count)) %>%
  inner_join(term.movies,by='movie.name') %>%
  mutate(relative.sentiment = total/n.reviews)

mypath <- file.path(My.Location,"/Top10 user review",paste("Sentiment Analysis Top 10 v2.jpg", sep = ""))
jpeg(file=mypath)
p <- movie.sentiment %>%
  filter(!sentiment %in% c("disgust","fear")) %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~movie.name,ncol=3)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position="none")
print(p)
dev.off()

#############################################################################################
## Web scrapping of crtitic reviews for Bottom 10 movies##
for(i in 1:10){
  if(i==1){
    document.list <- NULL
    plot_list <- NULL
    document.df.bottom10 <- NULL
    rownames(document.df.bottom10) <- NULL
    }
  ID <- bottom10_movies[i,"IMDB.ID"]
  movie.name <- bottom10_movies[i,"movie_title"]
  No.of.pages <- as.numeric(bottom10_movies[i,"no.of.pages.of.review"])
  for(j in 1:No.of.pages){
    if (j==1){
      ex_review <- NULL
      URL <- paste0("http://www.imdb.com/title/tt", ID, "/reviews?filter=chrono")
      MOVIE_URL <- read_html(URL)
      ex_review <- MOVIE_URL %>% html_nodes("#pagecontent") %>%
        html_nodes("div+ p") %>%
        html_text()
    }
    else {
      URL <- paste0("http://www.imdb.com/title/tt", ID, "/reviews?filter=chrono;filter=chrono;start=", (j-1)*10)
      ex_review_additional <- MOVIE_URL %>% html_nodes("#pagecontent") %>%
        html_nodes("div+ p") %>%
        html_text()
      ex_review <- c(ex_review,ex_review_additional)
    }
  }
  
  document.list = list(movie.name, ex_review)
  document.df <- as.data.frame(document.list)
  colnames(document.df) <- c("movie.name","ex_review")
  
  document.df.bottom10 <- rbind(document.df.bottom10, document.df)
  
  text.c <- VCorpus(DataframeSource(dplyr::select(document.df,ex_review)))
  DTM <- DocumentTermMatrix(text.c,
                            control=list(removePunctuation=TRUE,
                                         wordLengths=c(3, Inf),
                                         stopwords=TRUE,
                                         stemming=TRUE,
                                         removeNumbers=TRUE
                            ))
  DTM <- removeSparseTerms(DTM,0.995)
  DTM <- tidy(DTM)
  
  term.count <- DTM %>%
    group_by(term) %>%
    summarize(n.total=sum(count)) %>%
    arrange(desc(n.total))
  
  
  mypath <- file.path(My.Location,"/Bottom10 user review",paste("frequency_plot_",i, ".jpg", sep = ""))
  
  jpeg(file=mypath)
  mytitle = paste("Most Frequent Terms for Movie - ", movie.name)
  p <- term.count %>%
    slice(1:30)
  p1 <- ggplot(data=p,aes(x=fct_reorder(term,n.total),y=n.total)) + geom_bar(stat='identity') +
    coord_flip() + xlab('Counts') + ylab('')+ggtitle(mytitle)
  print(p1)
  dev.off()
  
  term.count.pop <- term.count %>%
    slice(5:100)
  
  #wordcloud(term.count.pop$term, term.count.pop$n.total, scale=c(5,.5))
  
  mypath <- file.path(My.Location,"/Bottom10 user review",paste("wordcloud_",i,  ".jpg", sep = ""))
  
  jpeg(file=mypath)
  mytitle = paste("WordCloud for Movie", movie.name)
  wordcloud(term.count.pop$term, term.count.pop$n.total,max.words =100,min.freq=3, scale=c(5,.5),
            random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
  dev.off()
  
}


meta.data.movie <- document.df.bottom10 %>%
  dplyr::select(movie.name) %>%
  rownames_to_column(var="document")

text.c.movie <- VCorpus(DataframeSource(dplyr::select(document.df.bottom10,ex_review)))
DTM <- DocumentTermMatrix(text.c.movie,
                          control=list(removePunctuation=TRUE,
                                       wordLengths=c(3, Inf),
                                       stopwords=TRUE,
                                       stemming=TRUE,
                                       removeNumbers=TRUE
                          ))

reviews.tidy <- tidy(removeSparseTerms(DTM,0.999))

str(reviews.tidy)
str(meta.data.movie)
term.movies <- reviews.tidy %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name) %>%
  summarize(n.reviews=sum(count))

## sentiments by movie 

bing <- get_sentiments("bing") 

movie.sentiment <- reviews.tidy %>%
  inner_join(bing,by=c("term"="word")) %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name,sentiment) %>%
  summarize(total=sum(count)) %>%
  inner_join(term.movies,by='movie.name') %>%
  mutate(relative.sentiment = total/n.reviews)

mypath <- file.path(My.Location,"/Bottom10 user review",paste("Sentiment Analysis Bottom 10.jpg", sep = ""))

jpeg(file=mypath)
p <- movie.sentiment %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~movie.name)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p)
dev.off()

mypath <- file.path(My.Location,"/Bottom10 user review",paste("Net Positive Sentiment Analysis Bottom 10.jpg", sep = ""))
jpeg(file=mypath)
p <- movie.sentiment %>%
  dplyr::select(sentiment,relative.sentiment,movie.name) %>%
  spread(sentiment,relative.sentiment) %>%
  mutate(net.pos = positive-negative) %>%
  ggplot(aes(x=fct_reorder(movie.name,net.pos),y=net.pos*100)) + geom_point(size=4) + coord_flip()+
  ylab('Net Positive Sentiment')+xlab('Movies - Bottom 10')
print(p)
dev.off()


nrc <- get_sentiments("nrc")
movie.sentiment <- reviews.tidy %>%
  inner_join(nrc,by=c("term"="word")) %>%
  left_join(dplyr::select(meta.data.movie,movie.name,document),by='document') %>%
  group_by(movie.name,sentiment) %>%
  summarize(total=sum(count)) %>%
  inner_join(term.movies,by='movie.name') %>%
  mutate(relative.sentiment = total/n.reviews)
  
mypath <- file.path(My.Location,"/Bottom10 user review",paste("Sentiment Analysis Bottom 10 v2.jpg", sep = ""))
jpeg(file=mypath)
p <- movie.sentiment %>%
  filter(!sentiment %in% c("disgust","fear")) %>%
  ggplot(aes(x=sentiment,y=relative.sentiment,fill=sentiment)) + geom_bar(stat='identity') + 
  facet_wrap(~movie.name,ncol=3)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position="none")
print(p)
dev.off()


################################### End ##############################




