############
#Take a look at project Gutneberg
#http://www.gutenberg.org/wiki/Main_Page
###########
#We will use these 4 books by Wells:
# The Time machine ID:35
# The war of the worlds ID:36
#The invisible man ID:5230
# The island of Doctor Moreau ID: 159
############################################
install.packages("gutenbergr")
data(stop_words)
library(gutenbergr)

shakespeare() <- gutenberg_download(61225)

tidy_shakespeare <- shakespeare() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_shakespeare)
#counting frequencies for tokens
tidy_shakespeare %>%
  count(word, sort=TRUE)



# Pipe the tidy shakespeare data frame to the next line
tidy_shakespeare %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)

#####
'''Notice how the most common words in the data frame are words like
“the”, “and”, and “i” that have no sentiments associated with them.
In the next exercise, you will join the data with a lexicon to implement sentiment analysis.'''

#####################################################################################


shakespeare_sentiment <- tidy_shakespeare %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments('bing'))

shakespeare_sentiment %>%
  # Find how many positive/negative words each play has
  count(word,sentiment)

sentiment_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments('bing')) %>%
  # Count the number of words by title, type, and sentiment
  count(word, sentiment, sort=TRUE)

sentiment_counts %>%
  # Group by the titles of the plays
  group_by(word) %>%
  # Find the total number of words in each play
  mutate(total = sum(n),
         # Calculate the number of words divided by the total
         percent = n/total) %>%
  # Filter the results for only negative sentiment
  filter(sentiment=="negative") %>%
  arrange(percent)





##################################

word_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments('bing')) %>%
  # Count by word and sentiment
  count(word,sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()




###############################33

tidy_shakespeare %>%
  # Count by title and word
  count(title,word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments('afinn')) %>%
  # Filter to only examine the values for Macbeth that are negative
  filter(title=="The Tragedy of Macbeth",value<0)


sentiment_contributions <- tidy_shakespeare %>%
  # Count by title and word
  count(title, word, sort = TRUE) %>%
  # Implement sentiment analysis using the "afinn" lexicon
  inner_join(get_sentiments("afinn")) %>%
  # Group by title
  group_by(title) %>%
  # Calculate a contribution for each word in each title
  mutate(contribution = n*value/sum(n)) %>%
  ungroup()

sentiment_contributions




####Excellent! This is the first step in looking at narrative arcs.#####



tidy_shakespeare %>%
  # Implement sentiment analysis using "bing" lexicon
  inner_join(get_sentiments('bing')) %>%
  # Count using four arguments
  count(title, type, index = linenumber %/% 70, sentiment)


# Load the tidyr package
library(tidyr)

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  # Spread sentiment and n across multiple columns
  spread(sentiment, n, fill = 0) %>%
  # Use mutate to find net sentiment
  mutate(sentiment = positive - negative)



# Load the ggplot2 package
library(ggplot2)

tidy_shakespeare %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, type, index = linenumber %/% 70, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  # Put index on x-axis, sentiment on y-axis, and map comedy/tragedy to fill
  ggplot(aes(index, sentiment, fill = type)) +
  # Make a bar chart with geom_col()
  geom_col() +
  # Separate panels with facet_wrap()
  facet_wrap(~ title, scales = "free_x")