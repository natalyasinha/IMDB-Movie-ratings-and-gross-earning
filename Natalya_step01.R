# Load library
install.packages("plotly")
install.packages("ggplot2")
install.packages("formattable")
install.packages("dtplyr")
library(tidyverse)
library(ggplot2)
library(forcats)
library(plotly)
library(formattable)
library(dtplyr)
library(rvest)

## Change it to your current location

My.Location <- ""
setwd(My.Location)

# Load data
movie <- read_csv('movie_metadata.csv')
glimpse(movie)
head(movie)
str(movie)

spec(movie)
unique(movie$color)
unique(movie$title_year)


## What are the total number of movies reviewed by year?
attach(movie)
temp <- movie %>% select(movie_title,title_year)
temp <- temp %>% group_by(title_year) %>% summarise(n=n())
temp <- na.omit(temp)

temp %>% ggplot(aes(x=title_year,y=n)) + geom_point() + geom_line(linetype='dotted')

## What is the average IMDB rating by year?
temp <- movie %>% select(imdb_score,title_year)
temp <- temp %>% group_by(title_year)%>% summarise(score=mean(imdb_score))
temp <- na.omit(temp)
temp %>% ggplot(aes(x=title_year,y=score)) + geom_point() + geom_line(linetype='dotted')


## How do the average score change for each type of content rating?
temp <- movie %>% select(content_rating,imdb_score)
temp <- temp %>% group_by(content_rating)%>% summarise(score = mean(imdb_score))

p <- plot_ly(
  x = temp$content_rating,
  y = temp$score,
  name = "Avg score by Rating",
  type = "bar")
p

## the highest average score seems to be bagged by TV-MA category

## How do these scores vary by category?

temp <- movie %>% select(imdb_score,content_rating)
temp <- na.omit(temp)
plot_ly(temp, x = imdb_score, color = content_rating, type = "box")

# We see that the IQR of each distribution is above 5. The highest imdb_scores tend to be of the TV-MA content rating type. The R rated category has the largest number of outliers that range from a score of 1.9 to 4.2.

# Filter data, with missing gross values
movie.color.nogross <- movie %>%
  filter(!complete.cases(gross))

movie.color.nogross$columnid <- c(1:nrow(movie.color.nogross))
#Reading IMDB pages
urls<-movie.color.nogross$movie_imdb_link
#Retrieving movie summary from all web pages
movie_info.for.missing.gross<-NULL
#Loop through all the URL to extract information



# for(kk in 1:nrow(movie.color.nogross)){
#   page<-read_html(urls[kk])
# 
# #Web Scraping for getting movie name
# movie_nodes <-html_nodes(page,'.title_wrapper h1')
# movie_name <-trimws(html_text(movie_nodes))
# movie_name<-sapply(strsplit(movie_name,"\\("), '[', 1) # Remove '('
# 
# #Web Scraping for getting movie budget
# box_office_budget<-html_nodes(page,".subheading+ .txt-block")
# budget <- 0
# for(i in html_text(box_office_budget)){
#   b<-trimws(gsub("\n","",i))
#   if(is.na(pmatch("Budget",b))){
#     m<-1
#     
#   }
#   else{
#     budget<-trimws(gsub("Budget:","",b))
#     budget<-trimws(sapply(strsplit(budget,"\\("), '[', 1)) # Remove '('
#   }
# }

# #Web Scraping for getting movie gross income
# gross<-html_nodes(page,".txt-block")
# gross_budget <- 0
# for(i in html_text(gross)){
#   g<-trimws(gsub("\n","",i))
#   if(is.na(pmatch("Gross",g))){
#     m<-1
#     
#   }
#   else{
#     
#     gross_budget<-trimws(gsub("Gross:","",g))
#     gross_budget<-trimws(sapply(strsplit(gross_budget,"\\("), '[', 1)) # Remove '('
#   }
# }


# #Creating individual data frames to form a collective dataframe
# budget<-data.frame(Budget=budget)
# gross_budget<-data.frame(Gross=gross_budget)
# 
# #Bind all the dataframes and their data to a single dataframe movie_info
# movie_info.for.missing.gross<-rbind(movie_info.for.missing.gross,data.frame(budget,gross_budget,kk))
# }



# movie.color.nogross <- movie.color.nogross %>%
#   left_join(movie_info.for.missing.gross,by=c("columnid"="kk"))
# movie_info.for.missing.gross %>% filter(Gross != '0')
# movie.color.nogross$gross <- movie.color.nogross$Gross
# movie.color.nogross$gross <- as.numeric(gsub(",","",gsub("$", "", movie.color.nogross$Gross)))
# str(movie_info.for.missing.gross)
# str(movie.color.nogross)

################################

# Filter data, target:  movies after 1990
movie.color <- movie %>%
  filter(color == "Color") %>%
  filter(title_year > 1990) %>% 
  filter(complete.cases(gross)) %>%  filter(gross > 1000000)

quantile(movie.color$gross, c(.01,.05,.1,.15,.25, .50, .75,.90,.95, 1)) 

glimpse(movie.color)

for (i in 1: max(nrow(movie.color))){
  temp<-strsplit(as.character(movie.color$genres[i]), "|", fixed=TRUE)
  movie.color[i, "Genre.N"]<-length(temp[[1]])
  for (j in 1:length(temp[[1]])){
    movie.color[i,paste("Genre",j,sep=".")]<-temp[[1]][j]
  }
  
}

# unique(movie.color$Genre.1)

movie.color$Genre.1 <- if_else(movie.color$Genre.1 == "Sci-Fi","Sci.Fi",movie.color$Genre.1)
movie.color$Genre.2 <- if_else(movie.color$Genre.2 == "Sci-Fi","Sci.Fi",movie.color$Genre.2)
movie.color$Genre.3 <- if_else(movie.color$Genre.3 == "Sci-Fi","Sci.Fi",movie.color$Genre.3)
movie.color$Genre.4 <- if_else(movie.color$Genre.4 == "Sci-Fi","Sci.Fi",movie.color$Genre.4)
movie.color$Genre.5 <- if_else(movie.color$Genre.5 == "Sci-Fi","Sci.Fi",movie.color$Genre.5)
movie.color$Genre.6 <- if_else(movie.color$Genre.6 == "Sci-Fi","Sci.Fi",movie.color$Genre.6)
movie.color$Genre.7 <- if_else(movie.color$Genre.7 == "Sci-Fi","Sci.Fi",movie.color$Genre.7)
movie.color$Genre.8 <- if_else(movie.color$Genre.8 == "Sci-Fi","Sci.Fi",movie.color$Genre.8)

movie.color$Genre.1<-as.factor(gsub(" ","", movie.color$Genre.1))
movie.color$Genre.2<-as.factor(gsub(" ","", movie.color$Genre.2))
movie.color$Genre.3<-as.factor(gsub(" ","", movie.color$Genre.3))
movie.color$Genre.4<-as.factor(gsub(" ","", movie.color$Genre.4))
movie.color$Genre.5<-as.factor(gsub(" ","", movie.color$Genre.5))
movie.color$Genre.6<-as.factor(gsub(" ","", movie.color$Genre.6))
movie.color$Genre.7<-as.factor(gsub(" ","", movie.color$Genre.7))
movie.color$Genre.8<-as.factor(gsub(" ","", movie.color$Genre.8))


movie.color$Genre.1 <- as.factor(ifelse(is.na(movie.color$Genre.1)==T, "0",as.character(movie.color$Genre.1))) 
movie.color$Genre.2 <- as.factor(ifelse(is.na(movie.color$Genre.2)==T, "0",as.character(movie.color$Genre.2))) 
movie.color$Genre.3 <- as.factor(ifelse(is.na(movie.color$Genre.3)==T, "0",as.character(movie.color$Genre.3))) 
movie.color$Genre.4 <- as.factor(ifelse(is.na(movie.color$Genre.4)==T, "0",as.character(movie.color$Genre.4))) 
movie.color$Genre.5 <- as.factor(ifelse(is.na(movie.color$Genre.5)==T, "0",as.character(movie.color$Genre.5)))
movie.color$Genre.6 <- as.factor(ifelse(is.na(movie.color$Genre.6)==T, "0",as.character(movie.color$Genre.6))) 
movie.color$Genre.7 <- as.factor(ifelse(is.na(movie.color$Genre.7)==T, "0",as.character(movie.color$Genre.7))) 
movie.color$Genre.8 <- as.factor(ifelse(is.na(movie.color$Genre.8)==T, "0",as.character(movie.color$Genre.8)))

t<-unique(c(as.character(unique(movie.color["Genre.1"])$Genre.1), as.character(unique(movie.color["Genre.2"])$Genre.2),
            as.character(unique(movie.color["Genre.3"])$Genre.3),as.character(unique(movie.color["Genre.4"])$Genre.4),
            as.character(unique(movie.color["Genre.5"])$Genre.5),as.character(unique(movie.color["Genre.6"])$Genre.6),
            as.character(unique(movie.color["Genre.7"])$Genre.7),as.character(unique(movie.color["Genre.8"])$Genre.8)))
t<-t[-c(17)]
t[11] <- "Sci.Fi"

t<- make.names(t, unique=TRUE)

for (i in 1:length(t)){
  for (j in 1: nrow(movie.color)){
    
    if (movie.color[j,"Genre.1"]==t[i] | movie.color[j,"Genre.2"]==t[i] | movie.color[j,"Genre.3"]==t[i] | movie.color[j,"Genre.4"]==t[i] | movie.color[j,"Genre.5"]==t[i] | movie.color[j,"Genre.6"]==t[i] | movie.color[j,"Genre.7"]==t[i] | movie.color[j,"Genre.8"]==t[i]) 
      movie.color[j,paste(t[i],"","")]<-1
    else
      movie.color[j,paste(t[i],"","")]<-0
  }
}

# for (i in 1:nrow(movie.color)){
#   if (movie.color$Adventure[i]==1 | movie.color$Sci.Fi[i]==1 | movie.color$Action[i]==1)
#     movie.color$new.genre[i]<-'adventure'
#   else if (movie.color$Comedy[i]==1 | movie.color$Romance[i]==1 | movie.color$Family[i]==1 | movie.color$Music[i]==1 | movie.color$Musical[i]==1)
#     movie.color$new.genre[i]<-'light'
#   else if (movie.color$Drama[i]==1 | movie.color$Biography[i]==1 | movie.color$Crime[i]==1 | movie.color$Mystery[i]==1 | movie.color$Thriller[i]==1 | movie.color$War[i]==1)
#     movie.color$new.genre[i]<-'serious'
#   else
#     movie.color$new.genre[i]<-'other'
# }

colnames(movie.color) <- gsub(" ","",colnames(movie.color))
# a <- head(movie.color)[,c(2,10,12,38:59)]
# b <- movie.color[(movie.color$director_name == "Martin Scorsese"),][,c(2,4,9,10,12,38:59)]
# 
# c <- movie.color %>% arrange(desc(gross)) 
# d <- head(c,20)[,c(2,10,12,38:59)]
# e <- movie.color[(movie.color$`Animation  ` == 0 & movie.color$`Sci.Fi  ` == 0 & movie.color$`Fantasy  ` == 0 & movie.color$`Comedy  ` == 0 & movie.color$`Action  ` == 0 & movie.color$`Drama  ` == 0 & movie.color$`Horror  ` == 0 & movie.color$`Mystery  ` == 0 & movie.color$`Thriller  ` ==0 & movie.color$`Documentary  ` ==0) ,][,c(2,4,9,10,12,38:59)] %>% arrange(desc(gross)) 

# movie.color$new.genre <- ifelse(movie.color$Adventure==1 | movie.color$Sci.Fi==1 | movie.color$Action==1,'adventure',
#                                 ifelse(movie.color$Comedy==1 | movie.color$Romance==1 | movie.color$Family==1 | movie.color$Music==1 | movie.color$Musical==1,'light',
#                                        ifelse(movie.color$Drama==1 | movie.color$Biography==1 | movie.color$Crime==1 | movie.color$Mystery==1 | movie.color$Thriller==1 | movie.color$War==1,'serious','others')))



movie.color$short.genre<-  ifelse(movie.color$Animation==1,'Adventure',
                                  ifelse(movie.color$Sci.Fi==1, 'Sci_fi',
                                         ifelse(movie.color$Fantasy==1,'Fantasy',
                                                ifelse(movie.color$Comedy ==1,'Comedy',
                                                       ifelse(movie.color$Action==1,'Action',
                                                              ifelse(movie.color$Drama==1,'Drama',
                                                                     ifelse(movie.color$Horror==1 | movie.color$Mystery==1 | movie.color$Thriller==1,'Thriller',
                                                                            ifelse(movie.color$Documentary==1,'Documentary',
                                                                                   'Drama'))))))))
unique(movie.color$short.genre)
table(movie.color$short.genre)
unique(movie.color$aspect_ratio) 
table(movie.color$aspect_ratio,movie.color$aspect_ratio)

movie.color$aspect_ratio <- ifelse(movie.color$aspect_ratio == 1.85 , "a) 1.85:1",
                                   ifelse(movie.color$aspect_ratio == 2.35 , "b) 2.35:1","c) Others"))
table(movie.color$aspect_ratio)
movie.color$aspect_ratio <- as.factor(movie.color$aspect_ratio)

# e <- movie.color[(movie.color$short.genre =='Comedy') ,][,c(2,4,9,10,12)] %>% arrange(desc(gross)) 
# Check Missing Data 

# colnames(movie.color) <- make.names(t, unique=TRUE)
# t<- make.names(t, unique=TRUE)

x <<- 0

for(i in 2:length(colnames(movie.color))){
  x <- append(x,length(which(is.na(movie.color[,i]) == TRUE)))  
}

x1 <<- 0
for(i in 2:length(colnames(movie.color))){
  x1 <- append(x1,length(which(movie.color[,i] == 0)))  
}


missingvalues_record <- data.frame(column = colnames(movie.color),missing = x)
missingvalues_record[missingvalues_record[,2] > 0,]

values_equal_tozero_record <- data.frame(column = colnames(movie.color),zero = x1)
missingvalues_record[values_equal_tozero_record[,2] > 0,]


################## Add code here ###################

actor.deciles <- movie.color %>%
  group_by(actor_1_name) %>%
  summarize(average_rating = mean(imdb_score), number_movie = length(budget)) 


quantile(actor.deciles$number_movie, c(.25, .50, .75,.90,.93,.95, 1)) 
quantile(actor.deciles$average_rating, c(.25, .50, .75,.90,.95, 1)) 

attach(actor.deciles)
actor.deciles$actor1_rating <- ifelse(average_rating >= 6.5 & number_movie >= 5,"Class I actor",
                                     ifelse(average_rating >= 6.5 & number_movie >= 3,"Rising Stars",
                                           ifelse(average_rating >= 7 ,"One hit wonder",
                                                  ifelse(average_rating >= 6 ,"General actor",
                                                                 "Underdog"))))

table(actor.deciles$actor1_rating) 

director.deciles <- movie.color %>%
  group_by(director_name) %>%
  summarize(average_rating = mean(imdb_score), number_movie = length(budget)) 


quantile(director.deciles$number_movie, c(.25, .50, .75,.90,.95, 1)) 
quantile(director.deciles$average_rating, c(.25, .50, .75,.90,.95, 1)) 

attach(director.deciles)
director.deciles$director_rating <- ifelse(average_rating >= 7 & number_movie >= 7,"Class I director",
                                           ifelse(average_rating >= 7 & number_movie >= 3,"Rising Stars",
                                                  ifelse(average_rating >= 7 ,"One hit wonder",
                                                         ifelse(average_rating >= 6 ,"General director",
                                                                "Underdog"))))
table(director.deciles$director_rating) 

actor2.deciles <- movie.color %>%
  group_by(actor_2_name) %>%
  summarize(average_rating = mean(imdb_score), number_movie = length(budget)) 


quantile(actor2.deciles$number_movie, c(.25, .50, .75,.90,.95, 1)) 
quantile(actor2.deciles$average_rating, c(.25, .50, .75,.90,.95, 1)) 

attach(actor2.deciles)
actor2.deciles$actor2_rating <- ifelse(average_rating >= 6.5 & number_movie >= 5,"Class I actor",
                                       ifelse(average_rating >= 6.5 & number_movie >= 3,"Rising Stars",
                                              ifelse(average_rating >= 7 ,"One hit wonder",
                                                     ifelse(average_rating >= 6 ,"General actor",
                                                            "Underdog"))))

table(director.deciles$director_rating)
table(actor.deciles$actor1_rating)
table(actor2.deciles$actor2_rating)

#### Add Bar Charts  ------------
#### Top 5 under class 1 and rising stars  ------------

movie.color <- movie.color %>%
  left_join(director.deciles[,c(1,4)],by="director_name")
movie.color <- movie.color %>%
  left_join(actor.deciles[,c(1,4)],by="actor_1_name")
movie.color <- movie.color %>%
  left_join(actor2.deciles[,c(1,4)],by="actor_2_name")

table(movie.color$director_rating)
table(movie.color$actor1_rating)
table(movie.color$actor2_rating)

# movie.color <- movie.color[,-c(62:ncol(movie.color))]

#############################################################
#Are the number of facebook likes an indicator of the imdb_score? Lets check it out!
  
temp <- movie.color %>% select(movie_facebook_likes,imdb_score,content_rating)
plot_ly(temp, x = movie_facebook_likes, y = imdb_score,
        color =content_rating , mode = "markers",text=paste('Content:',content_rating)) 

##We divide this scatter plot by content-rating. We do not see any trend here. There seem to be movies that have high IMDB scores but low Facebook likes.

## How are the likes of the total cast related to budget? Do producers spend more on popular actors?
temp <- movie.color %>% select(cast_total_facebook_likes,budget,movie_title,content_rating)
plot_ly(temp, x = cast_total_facebook_likes, y = budget,
        color =content_rating , mode = "markers",text=paste('Content:',content_rating))

## We do not see much here either. The Legend of Ron Burgundy has the highest number of cast Facebook
#likes with a budget of just 26M!The Gladiator on the other hand has 6521 cast likes with a budget of 103M!


# Is the number of likes for a director tied to the IMDB score?

temp <- movie.color %>% select(director_facebook_likes,imdb_score,content_rating,movie_title)

plot_ly(temp, x = director_facebook_likes, y = imdb_score,
        color =content_rating , mode = "markers",text=paste('Content:',content_rating))

# As we see this is not true. There are directors with high IMDB scores with low Facebook 
# likes Upon scrolling over, we find that the movie Towering Inferno has an IMDB score of 9.5 with 
# director likes of 0. That implies data is not captured correctly


# Is the number of likes for an actor tied to the IMDB score?

temp <- movie.color %>% select(actor_1_facebook_likes,imdb_score,content_rating,gross)

plot_ly(temp, x = actor_1_facebook_likes, y = imdb_score,
        color =content_rating , mode = "markers",text=paste('Content:',content_rating))

plot_ly(temp, x = actor_1_facebook_likes, y = gross,
        color =content_rating , mode = "markers",text=paste('Content:',content_rating))

# As we see this is not true. There are directors with high IMDB scores with low Facebook 
# likes Upon scrolling over, we find that the movie Towering Inferno has an IMDB score of 9.5 with 
# director likes of 0. That implies data is not captured correctly


attach(movie.color)

pairs(~gross+director_facebook_likes+gross+actor_1_facebook_likes+actor_2_facebook_likes+actor_3_facebook_likes,data=movie.color, 
      main="Simple Scatterplot Matrix")


######################################################

str(movie.color)
# write.csv(head(movie.color),"chk_colnames.csv")
missing.records <- movie.color[!complete.cases(movie.color),]

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(movie.color,2,pMiss)
# apply(movie.color,1,pMiss)

# We see that gross is missing in almost 12% of the datapoints, content_rating almost 5.7% of the datapoints,
# budget almost 8.6% of the datapoints, aspect_ratio almost 6.7% of the datapoints
# therefore we might consider either dropping it from the analysis or gather more measurements. 
# The other variables are below the 5% threshold so we can keep them. As far as the samples are concerned, 
# missing just one feature leads to a 12% missing data per sample. 
# Samples that are missing 2 or more features (>50%), should be dropped if possible.


### Important variables 

# duration-39
# director_facebook_likes - 12
# actor_2_facebook_likes - 10
# actor_1_facebook_likes - 10
# genres - 522
# facenumber_in_poster - 16
# language - 15
# country - 2
# budget - 240
# title_year - 362
# movie_facebook_likes - 284
# content_rating - 240
# num_critic_for_reviews - 39 

# install.packages("mice")
# install.packages("VIM")
library(mice)
library(VIM)
pattern.missing.data <- md.pattern(movie.color)
aggr_plot <- aggr(movie.color, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))



## No Correlation found among the independent varaiables
##########################################################################
# Key statistics by directors, average_rating = average imdb score of the movies which are directed by the 
# director
director.stats <- movie.color %>%
  group_by(director_name) %>%
  summarize(average_rating = mean(imdb_score), number_movie = length(budget), 
            average_duration = mean(duration), 
            total_budget = sum(budget/1000000, na.rm = TRUE), 
            total_gross = sum(gross/1000000, na.rm = TRUE),
            average_gross = total_budget/number_movie) %>%
  filter(total_budget > 100) %>%
  filter(total_gross > 10)%>%
  mutate(profit_margin = (total_gross-total_budget)/total_budget) %>%
  mutate(Average.profit = (total_gross-total_budget)/number_movie) %>%
  filter(number_movie >3) %>%
  arrange(desc(profit_margin))

################ Profit Margin ######################
top10_directors <- head(director.stats,n = 10) 
top10_directors$director_name <- as.character(top10_directors$director_name)
#Then turn it back into an ordered factor
top10_directors$director_name <- factor(top10_directors$director_name, levels=unique(top10_directors$director_name))
top10_directors$profit_margin <- round(top10_directors$profit_margin,digits=2)

top10_directors %>%
  ggplot(aes(x=director_name,y=profit_margin,label=format(profit_margin, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=profit_margin), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="",
       title="Top 10 Profit Margins by Directors")

################ Average Gross ######################
top10_directors <- director.stats %>% arrange(desc(average_gross)) %>% head(.,n = 10)
top10_directors$director_name <- as.character(top10_directors$director_name)
#Then turn it back into an ordered factor
top10_directors$director_name <- factor(top10_directors$director_name, levels=unique(top10_directors$director_name))
top10_directors$average_gross <- round(top10_directors$average_gross,digits=2)

top10_directors %>% 
  ggplot(aes(x=director_name,y=average_gross,label=format(average_gross, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=average_gross), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="Avg Gross earnings in MM",
       title="Top 10 Average gross earning Directors")

################ Average Profit ######################
top10_directors <- director.stats %>% arrange(desc(Average.profit)) %>% head(.,n = 10)
top10_directors$director_name <- as.character(top10_directors$director_name)
#Then turn it back into an ordered factor
top10_directors$director_name <- factor(top10_directors$director_name, levels=unique(top10_directors$director_name))
top10_directors$Average.profit <- round(top10_directors$Average.profit,digits=2)

top10_directors %>% 
  ggplot(aes(x=director_name,y=Average.profit,label=format(Average.profit, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=Average.profit), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="Avg Gross profits in MM",
       title="Top 10 Average Profit earning Directors")

# Key statistics by staring actors, average_rating = average imdb score of the movies which are starring by the 
# the actor
actor.stats <- movie.color %>%
  group_by(actor_1_name) %>%
  summarize(average_rating = mean(imdb_score), number_movie = length(budget), 
            average_duration = mean(duration), 
            total_budget = sum(budget/1000000, na.rm = TRUE), 
            total_gross = sum(gross/1000000, na.rm = TRUE),
            average_gross = total_budget/number_movie) %>%
  filter(total_budget > 100) %>%
  filter(total_gross > 1)%>%
  mutate(profit_margin = (total_gross-total_budget)/total_budget) %>%
  mutate(Average.profit = (total_gross-total_budget)/number_movie) %>%
  filter(profit_margin >1) %>%
  arrange(desc(profit_margin))

################ Profit Margin ######################
top10_leadactor <- head(actor.stats,n = 10) 
top10_leadactor$actor_1_name <- as.character(top10_leadactor$actor_1_name)
#Then turn it back into an ordered factor
top10_leadactor$actor_1_name <- factor(top10_leadactor$actor_1_name, levels=unique(top10_leadactor$actor_1_name))
top10_leadactor$profit_margin <- round(top10_leadactor$profit_margin,digits=2)

top10_leadactor %>%
  ggplot(aes(x=actor_1_name,y=profit_margin,label=format(profit_margin, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=profit_margin), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="",
       title="Top 10 Profit Margins by leadactor")

################ Average Gross ######################
top10_leadactor <- actor.stats %>% arrange(desc(average_gross)) %>% head(.,n = 10)
top10_leadactor$actor_1_name <- as.character(top10_leadactor$actor_1_name)
#Then turn it back into an ordered factor
top10_leadactor$actor_1_name <- factor(top10_leadactor$actor_1_name, levels=unique(top10_leadactor$actor_1_name))
top10_leadactor$average_gross <- round(top10_leadactor$average_gross,digits=2)

top10_leadactor %>% 
  ggplot(aes(x=actor_1_name,y=average_gross,label=format(average_gross, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=average_gross), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="Avg Gross earnings in MM",
       title="Top 10 Average gross earning leadactor")

################ Average Profit ######################
top10_leadactor <- actor.stats %>% arrange(desc(Average.profit)) %>% head(.,n = 10)
top10_leadactor$actor_1_name <- as.character(top10_leadactor$actor_1_name)
#Then turn it back into an ordered factor
top10_leadactor$actor_1_name <- factor(top10_leadactor$actor_1_name, levels=unique(top10_leadactor$actor_1_name))
top10_leadactor$Average.profit <- round(top10_leadactor$Average.profit,digits=2)

top10_leadactor %>% 
  ggplot(aes(x=actor_1_name,y=Average.profit,label=format(Average.profit, digits=2))) + 
  geom_bar(stat='identity',fill="steelblue") + 
  geom_text(aes(label=Average.profit), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle = -90, vjust = 0.5, hjust=1)) +
  labs(x="",
       y="Avg Gross profits in MM",
       title="Top 10 Average Profit earning leadactor")

library("ggplot2")
library("RColorBrewer")
library("data.table")
# install.packages("corrgram")
library(corrgram)

color_scheme = brewer.pal(8, "Blues")

keep_var <- c('num_critic_for_reviews',
           'duration',
           'gross',
           'movie_title',
           'num_voted_users',
           'facenumber_in_poster',
           'movie_imdb_link',
           'num_user_for_reviews',
           'language',
           'country',
           'content_rating',
           'budget',
           'title_year',
           'imdb_score',
           'aspect_ratio',
           'Genre.N',
           'short.genre',
           'director_rating',
           'actor1_rating',
           'actor2_rating')

movie.color.nonmissing <- movie.color[complete.cases(movie.color[keep_var]),]


#################### Subset Directors #################
# Subset the directors from entire dataset
director = movie.color['director_name']
# Count how many times each director is in the dataset
director = data.frame(table(movie.color$director_name))
# Sort the dataset by the frequency each director appears
director = director[order(director$Freq,decreasing=TRUE),]
# Remove the row without a director name
director = director[-c(1),]
# Plot the top 20 directors with the most movies
# reorder(factor(director), Freq)

director$Var1 <- factor(director$Var1, levels=unique(director$Var1))

ggplot(director[1:20,], aes(x=Var1, y=Freq, alpha=Freq)) + 
  geom_bar(stat = "identity", fill=color_scheme[8]) + 
  xlab("Director") + 
  ylab("Number of Movies") + 
  ggtitle("Top 20 Directors with the most movies") + 
  coord_flip()

#################### Subset Facenumber #################
# Subset the posters
poster = movie.color['facenumber_in_poster']

# Count how many times each count of faces is in the dataset
poster = data.frame(table(movie.color$facenumber_in_poster))

# Sort the dataset by the frequency each face count appears
poster = poster[order(poster$Freq,decreasing=TRUE),]
poster$Var1 <- factor(poster$Var1, levels=unique(poster$Var1))

# Plot the face count occurences in posters
ggplot(poster, aes(x=Var1, y=Freq, alpha=Freq)) + 
  geom_bar(stat = "identity", fill=color_scheme[8]) + 
  xlab("Number of Faces on Movie Poster") + 
  ylab("Frequency") + 
  ggtitle("Distribution of the Number Faces on Movie Posters") + 
  coord_flip()


# There are some years that there was no data
year = movie.color['title_year']
year = data.frame(table(movie.color$title_year))
year = year[order(year$Freq,decreasing=TRUE),]
# year$Var1 <- factor(year$Var1, levels=unique(year$Var1))
# # Bar Graph
# ggplot(data=year, aes(x=Var1, y=Freq)) + 
#   geom_bar(colour = "black", fill = "blue", width = 0.8, stat="identity") + 
#   xlab("Year") +
#   ylab("Count") +
#   ggtitle("Number of Movies by Year") +
#   scale_x_discrete(breaks = seq(1916, 2016, 5)) 

# Line Graph
ggplot(data=year, aes(x=Var1, y=Freq, group = 1)) +
  geom_line(colour="red", linetype=1, size=1.5) +
  geom_point(colour="blue", size=4, shape=21, fill="blue") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Number of Movies by Year") +
  scale_x_discrete(breaks = seq(1916, 2016, 5)) 


# IMDB Score Averages by Director
imdb_scores = as.data.table(subset(movie.color, movie.color$director_name != ''))
imdb_scores = imdb_scores[, mean(imdb_score), by=director_name]
names(imdb_scores) = c("director", "Average_Score")
imdb_scores = imdb_scores[order(imdb_scores$Average_Score,decreasing=TRUE),]
imdb_scores

ggplot(imdb_scores[1:20,], aes(x=reorder(factor(director), Average_Score), y=Average_Score, alpha=Average_Score)) + 
  geom_bar(stat = "identity", fill=color_scheme[8]) + 
  xlab("Director") + 
  ylab("Average IMDB Score") + 
  ggtitle("Top 20 Director Average IMDB Scores") + 
  coord_flip()

# Distribution of IMDB Scores
imdb_score = as.data.table(subset(movie.color, movie.color$imdb_score >= 0 & !is.na(movie.color$imdb_score)))
ggplot(imdb_score, aes(x=imdb_score)) + 
  geom_histogram(aes(fill=..count..), binwidth = 0.1) + 
  xlab("IMDB Score") + 
  ylab("Frequency") + 
  ggtitle("Distribution of IMDB Scores") + 
  geom_vline(aes(xintercept=mean(imdb_score, na.rm=T)), color="black", linetype="dashed", size=1) +
  scale_fill_gradient("Frequency", low = "blue", high = "red") +
  scale_x_continuous(breaks = seq(0, 10, 0.5)) 

### Select only Numeric columns for correlation check ####
##########################################################################

movie.color.numeric <-movie.color[sapply(movie.color, is.numeric)]
movie.color.numeric <- movie.color.numeric[,1:16]
movie.color.filtered <- movie.color.numeric[,c(1,2,6:7,9:11,14,16)]

library(corrplot)
M <- cor(movie.color.numeric)

corrplot(M, method="circle")
corrplot(cor(movie.color.filtered), method="circle")


write.csv(movie.color.nonmissing,"movie_color_nonmissing.csv")
write.csv(movie.color,"movie_color.csv")













