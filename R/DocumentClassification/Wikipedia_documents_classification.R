suppressPackageStartupMessages({
  if (!require(rvest)){ install.packages("rvest")}; library(rvest)
})
suppressPackageStartupMessages({
  if (!require(tm)){ install.packages("tm")}; library(tm)
})
suppressPackageStartupMessages({
  if (!require(tokenizers)){ install.packages("tokenizers")}; library(tokenizers)
})
suppressPackageStartupMessages({
  if (!require(tidyverse)){ install.packages("tidyverse")}; library(tidyverse)
})
suppressPackageStartupMessages({
  if (!require(tidytext)){ install.packages("tidytext")}; library(tidytext)
})
suppressPackageStartupMessages({
  if (!require(stringr)){ install.packages("stringr")}; library(stringr)
})
suppressPackageStartupMessages({
  if (!require(topicmodels)){ install.packages("topicmodels")}; library(topicmodels)
})

suppressPackageStartupMessages({
  if (!require(ggplot2)){ install.packages("ggplot2")}; library(ggplot2)
})


getSentencesFrmWikiPages <- function(url)
{
  pagetext <- vector()
  wikipagetext <- read_html(url)
  paragraphs <- wikipagetext %>% html_nodes(xpath = "//p")
  
  for(paragraph in paragraphs)
  {
    p.text <- paragraph %>% html_text()
    pagetext<- c(pagetext,p.text)
    
  }
  pagetext <- gsub("[^a-zA-Z\\.']+", " ", pagetext )
  sentences <- tokenize_sentences(pagetext)
  return (sentences)
}

wikipage1 <- "https://en.wikipedia.org/wiki/Mahatma_Gandhi"
wikipage2 <- "https://en.wikipedia.org/wiki/Apple_Inc."
wikipage3 <- "https://en.wikipedia.org/wiki/Roger_Federer"
wikipage4 <- "https://en.wikipedia.org/wiki/Singapore"

wikipages <- c(wikipage1, wikipage2, wikipage3, wikipage4)

wikiMahatmaGandhi <- unlist(getSentencesFrmWikiPages(wikipage1), recursive = TRUE, use.names = TRUE)
wikiApple <- unlist(getSentencesFrmWikiPages(wikipage2), recursive = TRUE, use.names = TRUE)
wikiRogerFederer <- unlist(getSentencesFrmWikiPages(wikipage3), recursive = TRUE, use.names = TRUE)
wikiSingapore <- unlist(getSentencesFrmWikiPages(wikipage4), recursive = TRUE, use.names = TRUE)
wikiList <- list(wikiMahatmaGandhi, wikiApple, wikiRogerFederer, wikiSingapore)

distScale <- list(c(0,0,0,8),c(0,0,4,4),c(0,0,8,0),c(0,2,2,4),c(0,2,4,2),c(0,4,0,4),c(0,4,2,2),c(0,4,4,0),c(0,8,0,0),c(2,0,2,4),c(2,0,4,2),c(2,2,0,4),
               c(2,2,2,2),c(2,2,4,0),c(2,4,0,2),c(2,4,2,0),c(4,0,0,4),c(4,0,2,2),c(4,0,4,0),c(4,2,0,2),c(4,2,2,0),c(4,4,0,0),c(8,0,0,0))


dfdocs <- data.frame(matrix(ncol = 9, nrow = 0), stringsAsFactors = F)
dfforLTM <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
colnames(dfforLTM) <- c("id", "text", "title")
docNames <- c("MahatmaGandhi", "Apple", "RogerFederer",  "Singapore")
colnames(dfdocs) <- c("DocNum", "MahatmaGandhi", "MGPorp", "Apple", "AppPorp",  "RogerFederer","RFPorp",  "Singapore", "SingPorp")
temp <- list()
for(i in 1:75)
{
  sumcount <- 0
  
  sentcsel <- list()
  randompicks <- unlist(sample(distScale, 1, replace = T))
  for(z in 1:4)
  {
    rselwikilist <- list()
      if(randompicks[z] > 0)
      {
        rselwikilist <- sample(wikiList[[z]], randompicks[z], replace = F)
        wikiList[[z]] <- setdiff(wikiList[[z]], rselwikilist)
        rselwikilist <- paste(rselwikilist, collapse = " ")
      }
      else
      {
        rselwikilist <- ""
      }
    sentcsel <- list(sentcsel, rselwikilist, randompicks[z])
    dfforLTM[nrow(dfforLTM)+1,] <- c(i, rselwikilist, paste(docNames[z],i,sep="_"))#docNames[z])
  }
  
  dfdocs[nrow(dfdocs)+1,] <- c(paste("Doc",i,sep="_"), unlist(sentcsel, recursive = TRUE))
  icount <- icount + 1
}

#dfforLTM1 <- dfforLTM %>% group_by(title)

doctibble <- as_tibble(dfforLTM)


# split into words
by_documents_word <- doctibble %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_documents_word %>%
  anti_join(stop_words) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

word_counts    # A tibble: 104,722 x 3. cols = {doc, word, n}

# now cast into dtm
documents_dtm <- word_counts %>%
  cast_dtm(title, word, n)

documents_dtm   # DocumentTermMatrix (documents: 193, terms: 18215)


documents_lda <- LDA(documents_dtm, k = 4, control = list(seed = 1234))

documents_lda

documents_topics <- tidy(documents_lda, matrix = "beta")
documents_topics    # tibble: 72,860 x 3

# use dplyr's top_n() to find the top 5 terms within each topic.
top_terms <- documents_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

docs_gamma <- tidy(documents_lda, matrix = "gamma")
docs_gamma    # A tibble: 772 x 3

# re-separate the document name into title and chapter
docs_gamma <- docs_gamma %>%
  separate(document, c("title", "DocNum"), sep = "_", convert = TRUE)

docs_gamma 


docs_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

document_classifications <- docs_gamma %>%
  group_by(title, DocNum) %>%
  top_n(1, gamma) %>%
  ungroup()

document_classifications

docs_topics <- document_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

docs_topics    # A tibble: 4 x 2

# find 'em mismatches now.
document_classifications %>%
  inner_join(docs_topics, by = "topic") %>%
  filter(title != consensus)

document_classifications   

assignments <- augment(documents_lda, data = documents_dtm)
assignments    # tibble: 104,721 x 4. cols={doc, term, count, .topic}

# combine assignments table with consensus book titles to find incorrectly classified words.

assignments <- assignments %>%
  separate(document, c("title", "DocNum"), sep = "_", convert = TRUE) %>%
  inner_join(docs_topics, by = c(".topic" = "topic"))

assignments

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n))%>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red") +  # , label = percent_format()
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Pages words were assigned to",
       y = "Pages words came from",
       fill = "% of assignments")

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words