#Load Libraries

library(tidytext)
library(dplyr)
library(ggplot2)
library(tesseract)
library(pdftools)
library(tidyr)
library(ggthemes)


#Load & Preprocess Text Assets

pngfile <- pdf_convert('PDF FILEPATH HERE', dpi=600)

#Run the Tesseract OCR Function

text <- ocr(pngfile)

#Print the Resulting Text

cat(text)

#Calc word freq

text_df <- tibble(line = 1:length(text), text = text) %>%
  unnest_tokens(word, text)

data("stop_words")

tidied_text <- text_df %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidied_text %>%
  filter(n > 1) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Sentiment analysis

feelings_bing <- text_df %>%
  group_by(word) %>%
  tally() %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(mycolor = ifelse(sentiment == 'negative', "negative", "positive"))


feelings_bing %>%
  #filter(sentiment == 'negative') %>%
  ggplot(aes(sentiment, n, colour = word)) +
  geom_point() +
  theme_tufte()
