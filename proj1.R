library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(textreadr)
library(ggplot2)
data(stop_words)

#musician 1 = bieber
#musician 2 = rihanna
#musician 3 = lady gaga
#musician 4 = drake
#musician 5 = bruno mars


my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_data <- read_document(file="popular.txt")

my_df<-data_frame(line=1:19465,text=my_data)


tidy_df_nonstop_frequency <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  count(word, sort=TRUE)
print(tidy_df_nonstop_frequency )

#plotting the token frequencies:

freq_hist <-tidy_df_nonstop_frequency %>%
  count(word, sort=TRUE) %>%
  filter(n > 200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  coord_flip()
print(freq_hist)