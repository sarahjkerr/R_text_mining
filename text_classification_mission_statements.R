#Load libraries

library(dplyr)
library(RSocrata)
library(ggplot2)
library(tidytext)
library(tm)

#Load assets

mwbe <- read.socrata(
  "https://data.cityofnewyork.us/resource/63ss-e7x6.json",
  app_token = "YOUR TOKEN HERE"
)

#Define stopwords

my_stops <- c('background','includes','well','service','business','services','onto','around','in','by','over','through','provide','using',
'specializing','including','service','clients','new','york','city','businesses','related','provided','nyc','range','firm','products','program',
'programs','product','improvement','organization','organizations','led','market','equipment','supply','supplies','support','supports','supporting',
'offer','offers','offering','company','companies','servicing','management','managing','quality','effective','specialize','specializes','efficient',
'outstanding','fantastic','high','excellent','exceptional','improve','improves','preferred','superior','choice','exceeds','exceeding','bigger',
'greater','value','valuable','trusted','advisor', 'owner', 'llc', 'inc', 'gain', 'able', 'business', 'owned', 'work', 'works', 'working', 'full',
'rep', 'initiatives', 'initiative', 'embrace', 'embracing', 'dba', 'custom', 'customize', 'hand crafted', 'focused', 'focus', 'new', 'newly', 'engaged', 'engage', 'engagement', 'la', 'mode', 'allow', 'allows', 'allowing','content','specialized','develop', 'develops','development','developing', 'developed', 'contractor', 'contracting', 'contracts', 'licensed', 'busines', "business's","city's",
"firm's","firms","markets","organizational","offerings","offered","serviced","especialized","especially","specialist","specialization","program's",
"developer","developers","developingsustainability","company's","provides","provider","providers","owners","owner's","providing","ny","within","time","agencies","agency","agency's","certified","site", "sell","solutions","solution","system","systems","systematic","systemmatic","professional", "professionals","can","experience","master","general","large","bags","years","based","user","users","providing","onsite","c","1","2","3","4","5","6","7","8","9","10","corp","also","types","grow","growth","duty","cost","state","tri","control","driving","job","jobs","various","etc","include","wbe","mwbe","ebe","mbe","meet")

#-- this needs refactoring!

stop_words <- stopwords("en")
stop_words <- as.vector(rbind(my_stops, stop_words))
stop_words_df <- data.frame(stop_words)
colnames(stop_words_df)[colnames(stop_words_df)=="stop_words"] <- "word"

#Generate a df with a list of keywords from each business's mission statement (tidy format = 1 term per "document" per row)
#Each business will have n number of rows, each with 1 key word in it
#n depends on how many keywords appear in the statement

businesses <- data.frame(document = mwbe$vendor_formal_name, term = mwbe$business_description, stringsAsFactors = FALSE)

business_tokens <- unnest_tokens(businesses, word, term, token = "words", format = c("text"), to_lower = TRUE, drop = TRUE, collapse = FALSE)

business_word_counts <- business_tokens %>% 
  anti_join(stop_words_df) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

#Now cast to a document term matrix

business_dtm <- business_word_counts %>%
  cast_dtm(biz, word, n)

#There is a good way to determine the number of topics to include (https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-16-S13-S8)
#Here I went with 10 for exploratory analysis

mwbe_topic_model_10 <- LDA(business_dtm, k = 10, method = "VEM")

td_beta_10 <- tidy(mwbe_topic_model_10)

mwbe_top_terms_10 <- td_beta_10 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

mwbe_top_terms_10 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
