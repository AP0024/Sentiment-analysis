
###############Load Libraries
library(tidyverse)
library(tm)
library(wordcloud)
library(syuzhet)
library(readr)

############read reviews############
"power_now_reviews.csv" %>%
  read_csv()  ->power_now_reviews

"reviews.csv" %>%
  read_csv()  ->secret

############Combining the reviews of two books############
secret %>%
  rbind(power_now_reviews) ->secret_now

############Glimpse###########
secret_now %>%
  glimpse()
############Filter Rating###########

secret_now %>%
  select(id,rating) %>%
    filter(rating >= 4) 



###########Creating Corpus#########
secret_now$text %>%
  iconv() -> encoded_text

sample(encoded_text,200)%>%
  VectorSource() %>%
    Corpus()-> Corpus_form

###########Inspecting Corpus #########

Corpus_form [1:3]%>%
  inspect()


###########Cleaning the Corpus########
 
Corpus_form %>%
  tm_map(tolower) -> Corpus_form


Corpus_form %>%
  tm_map(removePunctuation) -> Corpus_form

Corpus_form %>%
  tm_map(removeNumbers) -> Corpus_form


Corpus_form %>%
  tm_map(removeWords,stopwords("english")) -> Corpus_form

Corpus_form %>%
  tm_map(removeWords,c("book","read","life","become","must","asap")) -> Corpus_form

Corpus_form %>%
  tm_map(stripWhitespace) -> Final_Corpus_form

inspect(Final_Corpus_form[1:5])

############# Term DocumentMatrix##################

Final_Corpus_form %>%
              TermDocumentMatrix() %>%
  as.matrix() -> tdm 


tdm %>%
   rowSums() ->tdm_rsum 


##############Subsetting#######


tdm_rsum %>% 
  subset(tdm_rsum>5)


##############WordCLoud#######
tdm_rsum %>%
  sort(decreasing = TRUE) ->tdm_rsum_sort


set.seed(1234)


names(tdm_rsum_sort) %>%
  wordcloud(freq = tdm_rsum_sort,
            max.words = 105,
            random.order = T,
            min.freq = 2,
            colors = brewer.pal(5,"Dark2"),
            scale = c(3,0.3)
            
            )


###################SentimentScore########


get_nrc_sentiment(secret_now$text) ->sentiment

#########################Sampling Sentiments & Scoring###

sentiment[sample(1:nrow(sentiment),1000),] ->sentiment

sentiment %>% 
  mutate(Scoring_pn = sentiment$positive - sentiment$negative) -> sentiments

sentiments %>%
  colSums()  -> col_Sen
col_Sen/nrow(sentiment)*100 -> percentage_setniments
###barplot#####
percentage_setniments %>% barplot(col = rainbow(10))