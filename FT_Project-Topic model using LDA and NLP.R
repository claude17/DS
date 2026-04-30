library(rvest)
library(dplyr)
library(stringr)

prodemo <- data.frame()

convert_day <- function(day) {
  words <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", 
             "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", 
             "Eighteen", "Nineteen", "Twenty", "Twenty-one", "Twenty-two", "Twenty-three", 
             "Twenty-four", "Twenty-five", "Twenty-six", "Twenty-seven", "Twenty-eight", 
             "Twenty-nine", "Thirty", "Thirty-one")
  return(words[as.numeric(day)])
}


convert_year <- function(year) {
  paste("Two-thousand-", 
        switch(substr(year, 3, 4),
               "20" = "twenty", "21" = "twenty-one", "22" = "twenty-two", 
               "23" = "twenty-three", "24" = "twenty-four"), sep = "")
}


for (page_number in 1:100) {
  
  link <- paste0("https://uk.trustpilot.com/review/www.crystaltravel.co.uk?page=", page_number)
  
  
  page <- read_html(link)
  
  
  reviewer_name <- page %>%
    html_nodes(".styles_consumerDetails__ZFieb .typography_appearance-default__AAY17") %>%
    html_text(trim = TRUE)
  
  review_title <- page %>%
    html_nodes(".link_notUnderlined__szqki .typography_appearance-default__AAY17") %>%
    html_text(trim = TRUE)
  
  rating <- page %>%
    html_nodes(".styles_reviewHeader__iU9Px img") %>%
    html_attr("alt") 
  
  Date_of_experience <- page %>%
    html_nodes(".styles_reviewContent__0Q2Tg p.typography_body-m__xgxZ_") %>%
    html_text(trim = TRUE)
  
  review_description <- page %>%
    html_nodes(".typography_color-black__5LYEn") %>%
    html_text(trim = TRUE)
  
  
  rating <- case_when(
    grepl("5 out of 5 stars", rating, ignore.case = TRUE) ~ "excellent",
    grepl("4 out of 5 stars", rating, ignore.case = TRUE) ~ "good",
    grepl("3 out of 5 stars", rating, ignore.case = TRUE) ~ "average",
    grepl("2 out of 5 stars", rating, ignore.case = TRUE) ~ "below average",
    grepl("1 out of 5 stars", rating, ignore.case = TRUE) ~ "bad",
    TRUE ~ NA_character_ 
  )
  
  
  max_length <- max(length(reviewer_name), length(review_title), length(rating), length(Date_of_experience), length(review_description))
  reviewer_name <- c(reviewer_name, rep(NA, max_length - length(reviewer_name)))
  review_title <- c(review_title, rep(NA, max_length - length(review_title)))
  rating <- c(rating, rep(NA, max_length - length(rating)))
  Date_of_experience <- c(Date_of_experience, rep(NA, max_length - length(Date_of_experience)))
  review_description <- c(review_description, rep(NA, max_length - length(review_description)))
  
  
  Crystal_Travel_Review <- data.frame(
    reviewer_name,
    review_title,
    rating,
    Date_of_experience,
    review_description,
    stringsAsFactors = FALSE
  )
  
  
  prodemo <- bind_rows(prodemo, Crystal_Travel_Review)
  
  
  print(paste("Page:", page_number, "scraped successfully."))
}


prodemo <- prodemo %>%
  mutate(
    Date_of_experience = str_replace(Date_of_experience, "Date of experience: ", ""),
    Date_of_experience = paste(
      sapply(str_extract(Date_of_experience, "^\\d+"), convert_day),
      str_extract(Date_of_experience, "[A-Za-z]+"),
      sapply(str_extract(Date_of_experience, "\\d{4}$"), convert_year)
    )
  )


prodemo <- prodemo %>%
  rename_with(~ str_to_title(.))


write.csv(prodemo, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", row.names = FALSE)


View(prodemo)


library(dplyr)
library(stringr)
library(textclean)


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


clean_text <- function(text) {
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)

View(data)

if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data_cleaned, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)


format_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  sentence_tokens <- tokenize_sentences(text)[[1]]
  
  
  formatted_sentences <- sapply(sentence_tokens, function(sentence) {
    word_tokens <- tokenize_words(sentence)[[1]]
    paste0("[", paste0("'", word_tokens, "'", collapse = ", "), "]")
  })
  
  
  return(paste(formatted_sentences, collapse = " "))
}


data_tokenized <- data_cleaned %>%
  mutate(across(everything(), ~ sapply(., format_tokens)))


write.csv(
  data_tokenized,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_tokenized.csv",
  row.names = FALSE
)


View(data_tokenized)


if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data_cleaned, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)


process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  word_tokens <- tokenize_words(text)[[1]]
  
  
  stop_words <- stopwords::stopwords("en")  
  filtered_tokens <- word_tokens[!(tolower(word_tokens) %in% stop_words)]
  
  
  formatted_tokens <- paste0("[", paste0("'", filtered_tokens, "'", collapse = ", "), "]")
  
  return(formatted_tokens)
}


data_filtered <- data_cleaned %>%
  mutate(across(everything(), ~ sapply(., process_tokens)))


write.csv(
  data_filtered,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_filtered.csv",
  row.names = FALSE
)


View(data_filtered)


if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("textstem")) install.packages("textstem")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)  
library(textstem)   


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data_cleaned, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)


process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  word_tokens <- tokenize_words(text)[[1]]
  
  
  stop_words <- stopwords::stopwords("en")  
  filtered_tokens <- word_tokens[!(tolower(word_tokens) %in% stop_words)]
  
  
  stemmed_tokens <- SnowballC::wordStem(filtered_tokens, language = "en")
  
  
  lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)
  
  
  formatted_tokens <- paste0("[", paste0("'", lemmatized_tokens, "'", collapse = ", "), "]")
  
  return(formatted_tokens)
}


data_processed <- data_cleaned %>%
  mutate(across(everything(), ~ sapply(., process_tokens)))


write.csv(
  data_processed,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_SandL.csv",
  row.names = FALSE
)

View(data_processed)


if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("textstem")) install.packages("textstem")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)  
library(textstem)   


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


contractions_dict <- list(
  "cant" = "cannot",
  "didnt" = "didnot",
  "dont" = "do not",
  "havent" = "havenot",
  "isnt" = "isnot",
  "arent" = "arenot",
  "willnt" = "willnot",
  "werent" = "werenot"
)


replace_contractions_dict <- function(tokens, contractions) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(NA)  
  }
  
  
  tokens <- sapply(tokens, function(token) {
    if (token %in% names(contractions)) {
      return(contractions[[token]])
    } else {
      return(token)
    }
  })
  
  return(tokens)
}


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data_cleaned, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)


process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  word_tokens <- tokenize_words(text)[[1]]
  
  
  stop_words <- stopwords::stopwords("en")  
  filtered_tokens <- word_tokens[!(tolower(word_tokens) %in% stop_words)]
  
  
  stemmed_tokens <- SnowballC::wordStem(filtered_tokens, language = "en")
  
  
  lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)
  
  return(lemmatized_tokens)
}


apply_contractions <- function(tokens, contractions) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(NA)  
  }
  
  
  tokens <- replace_contractions_dict(tokens, contractions)
  
  
  text <- paste(tokens, collapse = " ")
  
  
  formatted_text <- paste0("[", gsub("\\s+", " ", text), "]")
  
  return(formatted_text)
}


data_processed <- data_cleaned %>%
  mutate(across(everything(), ~ sapply(., function(x) {
    tokens <- process_tokens(x)  
    apply_contractions(tokens, contractions_dict) 
  })))


write.csv(
  data_processed,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_Contraction.csv",
  row.names = FALSE
)


View(data_processed)


if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("textstem")) install.packages("textstem")

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)  
library(textstem)   


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


contractions_dict <- list(
  "cant" = "cannot",
  "didnt" = "didnot",
  "dont" = "do not",
  "havent" = "havenot",
  "isnt" = "isnot",
  "arent" = "arenot",
  "willnt" = "willnot",
  "werent" = "werenot"
)


emoji_dict <- list(
  "ðŸ˜Š" = "smile",          
  "ðŸ‘ðŸ" = "thumbsup",     
  "â¤ï¸" = "heart",     
  "ðŸ˜­" = "sad",           
  "ðŸ‘ˆ" = "ok",             
  "ðŸ’¥" = "sparkles",       
  "ðŸ˜„" = "laugh",         
  "ðŸ‘ˆðŸ‘" = "hug",        
  "ðŸ˜" = "heart_eyes",     
  "ðŸ˜¡" = "surprise",       
  "ðŸ‘¬" = "star"           
)


replace_emojis_dict <- function(text, emojis) {
  if (is.na(text)) return(NA)  
  for (emoji in names(emojis)) {
    text <- str_replace_all(text, fixed(emoji), emojis[[emoji]])
  }
  return(text)
}


replace_contractions_dict <- function(tokens, contractions) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(NA)  
  }
  
  
  tokens <- sapply(tokens, function(token) {
    if (token %in% names(contractions)) {
      return(contractions[[token]])
    } else {
      return(token)
    }
  })
  
  return(tokens)
}


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


write.csv(data_cleaned, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_cleaned.csv", row.names = FALSE)


process_tokens <- function(text) {
  if (is.na(text) || !is.character(text) || nchar(text) == 0) {
    return(NA)  
  }
  
  
  word_tokens <- tokenize_words(text)[[1]]
  
  
  stop_words <- stopwords::stopwords("en")  
  filtered_tokens <- word_tokens[!(tolower(word_tokens) %in% stop_words)]
  
  
  stemmed_tokens <- SnowballC::wordStem(filtered_tokens, language = "en")
  
  
  lemmatized_tokens <- textstem::lemmatize_words(stemmed_tokens)
  
  return(lemmatized_tokens)
}


apply_contractions <- function(tokens, contractions) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(NA)  
  }
  
  
  tokens <- replace_contractions_dict(tokens, contractions)
  
  
  text <- paste(tokens, collapse = " ")
  
  return(text)
}


data_processed <- data_cleaned %>%
  mutate(across(everything(), ~ sapply(., function(x) {
    x <- replace_emojis_dict(x, emoji_dict)        
    tokens <- process_tokens(x)                 
    formatted_text <- apply_contractions(tokens, contractions_dict) 
    paste0("[", formatted_text, "]")          
  })))


write.csv(
  data_processed,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_Final_with_emoji.csv",
  row.names = FALSE
)


View(data_processed)

if (!require("tokenizers")) install.packages("tokenizers")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("textclean")) install.packages("textclean")
if (!require("stopwords")) install.packages("stopwords")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("textstem")) install.packages("textstem")
if (!require("hunspell")) install.packages("hunspell")  

library(tokenizers)
library(dplyr)
library(stringr)
library(textclean)
library(stopwords)
library(SnowballC)  
library(textstem)   
library(hunspell)   


data <- read.csv("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo.csv", stringsAsFactors = FALSE)


contractions_dict <- list(
  "cant" = "cannot",
  "didnt" = "didnot",
  "dont" = "do not",
  "havent" = "havenot",
  "isnt" = "isnot",
  "arent" = "arenot",
  "willnt" = "willnot",
  "werent" = "werenot"
)


emoji_dict <- list(
  "ðŸ˜Š" = "smile",          
  "ðŸ‘ðŸ" = "thumbsup",     
  "â¤ï¸" = "heart",        
  "ðŸ˜­" = "sad",            
  "ðŸ‘ˆ" = "ok",             
  "ðŸ’¥" = "sparkles",       
  "ðŸ˜„" = "laugh",         
  "ðŸ‘ˆðŸ‘" = "hug",       
  "ðŸ˜" = "heart_eyes",     
  "ðŸ˜¡" = "surprise",       
  "ðŸ‘¬" = "star"            
)


replace_emojis_dict <- function(text, emojis) {
  if (is.na(text)) return(NA)  
  for (emoji in names(emojis)) {
    text <- str_replace_all(text, fixed(emoji), emojis[[emoji]])
  }
  return(text)
}


replace_contractions_dict <- function(tokens, contractions) {
  if (is.null(tokens) || length(tokens) == 0) {
    return(NA)  
  }
  
  tokens <- sapply(tokens, function(token) {
    if (token %in% names(contractions)) {
      return(contractions[[token]])
    } else {
      return(token)
    }
  })
  
  return(tokens)
}


clean_text <- function(text) {
  if (is.na(text)) return(NA)  
  text <- replace_html(text) 
  text <- tolower(text) 
  text <- str_remove_all(text, "[^a-zA-Z\\s]") 
  text <- str_squish(text) 
  return(text)
}


spell_check_text <- function(text) {
  if (is.na(text) || !is.character(text)) {
    return(text)  
  }
  
  
  tokens <- unlist(strsplit(text, "\\s+"))
  corrected_tokens <- sapply(tokens, function(word) {
    suggestions <- hunspell_suggest(word)
    if (length(suggestions) > 0 && !hunspell_check(word)) {
      return(suggestions[[1]][1])  
    } else {
      return(word)  
    }
  })
  
  return(paste(corrected_tokens, collapse = " "))
}


data_cleaned <- data %>%
  mutate(across(everything(), ~ sapply(., clean_text)))


data_processed <- data_cleaned %>%
  mutate(
    Reviewer_name = sapply(Reviewer_name, function(x) {
      tokens <- process_tokens(x)
      formatted_text <- apply_contractions(tokens, contractions_dict)
      return(formatted_text)
    }),
    
    Rating = Rating,  
    
    Date_of_experience = Date_of_experience  
  )


data_processed <- data_processed %>%
  mutate(
    Review_title = sapply(Review_title, function(x) {
      spell_checked_text <- spell_check_text(x)
      return(spell_checked_text)
    }),
    
    Review_description = sapply(Review_description, function(x) {
      spell_checked_text <- spell_check_text(x)
      return(spell_checked_text)
    })
  )


write.csv(
  data_processed,
  "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/New folder/Project2demo_Final_With_All_NLP.csv",
  row.names = FALSE
)

View(data_processed)






library(tm)
library(tidytext)
library(topicmodels)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(wordcloud)


file_path <- "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/DataScience_Mid_Project/Project2demo_Final_With_All_NLP.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)
View(data)

text_data <- data$Review_description

corpus <- Corpus(VectorSource(text_data))

corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)


dtm <- DocumentTermMatrix(corpus)

dtm_matrix <- as.matrix(dtm)
print("DTM as a matrix:")
print(dtm_matrix)
view(dtm_matrix)

row_totals <- rowSums(as.matrix(dtm))  
dtm <- dtm[row_totals > 0, ]

dtm_tfidf <- weightTfIdf(dtm)
inspect(dtm_tfidf)

dtm_tfidf_Matrix <- as.matrix(dtm_tfidf)
print("DTM_TFIDF as a matrix:")
print(dtm_tfidf_Matrix)
view(dtm_tfidf_Matrix)

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing = TRUE)
freq[ord]
write.csv(freq[ord], "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/DataScience_FT_Project/word_freq.csv")


sorted_freq <- freq[ord][1:10]

freq_df <- data.frame(
  Word = names(sorted_freq),
  Frequency = sorted_freq
)

ggplot(freq_df, aes(x = reorder(Word, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Word Frequency Bar Plot",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



log_likelihoods <- c()
k_values <- seq(4, 10, by = 1)  

for (k in k_values) {
  lda_model <- LDA(dtm, k = k, control = list(seed = 12345))
  log_likelihoods <- c(log_likelihoods, logLik(lda_model))
}


log_likelihood_df <- data.frame(k = k_values, log_likelihood = log_likelihoods)
ggplot(log_likelihood_df, aes(x = k, y = log_likelihood)) +
  geom_line() +
  geom_point() +
  labs(title = "Log-Likelihood vs Number of Topics",
       x = "Number of Topics (k)", y = "Log-Likelihood") +
  theme_minimal()


num_topics <- k_values[which.max(log_likelihoods)]  
cat("Optimal number of topics (k):", num_topics, "\n")


lda_model <- LDA(dtm, k = num_topics, control = list(seed = 12345))


terms <- terms(lda_model, 10)  
print("Top Terms for Each Topic:")
print(terms)

write.csv(terms, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/DataScience_FT_Project/Topic_Modeling_Terms.csv", row.names = FALSE)


topic_proportions <- as.data.frame(posterior(lda_model)$topics)
print("Topic Proportions for Each Document:")
print(head(topic_proportions))


non_empty_rows <- row_totals > 0  


filtered_data <- data[non_empty_rows, ]


data_with_topics <- cbind(filtered_data, topic_proportions)


write.csv(data_with_topics, "E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/DataScience_FT_Project/Topic_Modeling_Proportions.csv", row.names = FALSE)


topic_terms <- tidy(lda_model, matrix = "beta")
top_terms <- topic_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic",
       x = NULL, y = "Beta") +
  theme_minimal()


topic_terms <- tidy(lda_model, matrix = "beta")
top_terms <- topic_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 50) %>%  
  ungroup()


for (i in 1:num_topics) {
  cat("Generating Word Cloud for Topic", i, "\n")
  
  topic_data <- top_terms %>% filter(topic == i)
  

  wordcloud(words = topic_data$term, 
            freq = topic_data$beta, 
            max.words = 50, 
            random.order = FALSE, 
            colors = brewer.pal(8, "Dark2"),
            scale = c(3, 0.5))
}