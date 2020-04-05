library(tidytext)
library(dplyr)
library(stringr)
library(textreadr)
library(ggplot2)
data(stop_words)


#Custom Stop Words
cusstop <- tribble( ~word, ~lexicon,
                    "wanna", "CUSTOM",
                    "whoa" , "CUSTOM",
                    'gonna', "CUSTOM",
                    'gotta', "CUSTOM",
                    "bum"  , "CUSTOM",
                    "na"   , "CUSTOM",
                    "eh"   , "CUSTOM",
                    "ooh"  , "CUSTOM",
                    'hey'  , "CUSTOM",
                    'la'   , "CUSTOM",
                    'ah'   , "CUSTOM",
                    'uh'   , "CUSTOM",
                    'muh'  , "CUSTOM",
                    'yeah' , 'CUSTOM',
                    "em"   , "CUSTOM"
)
stop_words <- stop_words %>% bind_rows(cusstop)



#musician 1 = bieber
my_bieber <- read_document(file="bieber.txt")
my_df1<-data_frame(line=1:3715,text=my_bieber)

my_df1_non <- my_df1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


#musician 2 = rihanna
my_rihanna <- read_document(file="rihanna.txt")
my_df2<-data_frame(line=1:3895,text=my_rihanna)

my_df2_non <- my_df2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#musician 3 = lady gaga
my_lady <- read_document(file="ladygaga.txt")
my_df3<-data_frame(line=1:3807,text=my_lady)

my_df3_non <- my_df3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#musician 4 = drake
my_drake <- read_document(file="drake.txt")
my_df4<-data_frame(line=1:4773,text=my_drake)

my_df4_non <- my_df4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#musician 5 = bruno mars
my_brunomars <- read_document(file="brunomars.txt")
my_df5<-data_frame(line=1:3270,text=my_brunomars)

my_df5_non <- my_df5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)


library(tidyr)
frequency <- bind_rows(mutate(my_df1_non, musician="Justin Bieber"),
                       mutate(my_df2_non, musician= "Rihanna"),
                       mutate(my_df3_non, musician="Lady Gaga"),
                       mutate(my_df4_non, musician="Drake"),
                       mutate(my_df5_non, musician="Bruno Mars")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(musician, word) %>%
  group_by(musician) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(musician, proportion) %>%
  gather(musician, proportion, `Lady Gaga`, `Rihanna`,`Drake`,`Bruno Mars`)

print(frequency)


#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`Justin Bieber`, 
                      color = abs(`Justin Bieber`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~musician, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Justin Bieber", x=NULL)


cor.test(data=frequency[frequency$musician == "Lady Gaga",],
         ~proportion + `Justin Bieber`)

cor.test(data=frequency[frequency$musician == "Drake",],
         ~proportion + `Justin Bieber`)

cor.test(data=frequency[frequency$musician == "Bruno Mars",],
         ~proportion + `Justin Bieber`)

cor.test(data=frequency[frequency$musician == "Rihanna",],
         ~proportion + `Justin Bieber`)


##############################################
# Frequency of Total Data

word_counts <- bind_rows(mutate(my_df1, musician="Justin Bieber"),
                            mutate(my_df2, musician= "Rihanna"),
                            mutate(my_df3, musician="Lady Gaga"),
                            mutate(my_df4, musician="Drake"),
                            mutate(my_df5, musician="Bruno Mars")
)


tidy_pop <- word_counts %>%
  group_by(musician) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
tidy_pop
####### Table #########

#We can see who is more repeatetive and who used which words often
whousedwhat<-tidy_pop %>%
    filter(n>150)
whousedwhat
####### Table #########


word_co <- word_counts %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

word_counts2<-word_co%>%
  count(word) %>%
  filter(n>150)%>%
  arrange(desc(n))

word_counts <- word_co %>%
  count(word) %>%
  filter(n>200)%>%
  mutate(word2= fct_reorder(word, n))
  


##Review Word Counts BAR PLOT
ggplot(
  word_counts, aes(x = word2, y = n)
)+
  geom_col() +
  coord_flip() + ggtitle("Review Word Counts")


#See what sentiments doing
get_sentiments("bing") #negative-positive
get_sentiments("nrc") #feelings trust fear etc.
get_sentiments("afinn") #numbers


#positive-negative thing
library(tidyr)
popsentiment <- tidy_pop %>%
  inner_join(get_sentiments("bing")) %>%
  count(musician,index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Put them on a plot
library(ggplot2)
ggplot(popsentiment, aes(index, sentiment, fill = musician)) + geom_col(show.legend = FALSE) +
  facet_wrap(~musician, ncol = 2, scales = "free_x")


#################################################################3

### Drake
Drake <- tidy_pop %>%
  filter(musician == "Drake")
Drake

#afinn value
afinn <- Drake %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")
afinn
#bing and nrc
bing_and_nrc <- bind_rows(
  Drake %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  Drake %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
bing_and_nrc
#Comparrison afinn and bing&nrc
bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) + geom_col(show.legend = FALSE) + facet_wrap(~method, ncol = 1, scales = "free_y")

#Counting sentiments
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

bing_word_counts <- tidy_pop %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x=NULL)+ 
  coord_flip()
####################################################################################


####################################################################################

# Word Cloud just for Justin Bieber and Drake

word_c <- bind_rows(mutate(my_df1, musician="Justin Bieber"),
                         mutate(my_df4, musician="Drake")
                         
)


drake_justin <- word_c %>%
  group_by(musician) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
drake_justin


library(wordcloud)
drake_justin %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 80))

library(reshape2)
drake_justin %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray30", "gray70"),
                                                                          max.words = 80)






####################################
#Wordcloud for all the top 5 musician
library(wordcloud)
tidy_pop %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)
tidy_pop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("gray30", "gray70"),
                                                                          max.words = 80)



bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
wordcounts3 <- tidy_pop %>%
  group_by(musician) %>%
  summarize(words = n())
wordcounts3






