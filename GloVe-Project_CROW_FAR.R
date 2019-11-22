# Word Embedding Project Tim Runck, Massimo Graae Losinno, Matt Loftis
# November 2019
# Aarhus University


#load in data from archive


library(keras)
library(stringr)
library(purrr)
library(tm)

#preprocess data
out <- combo_set[combo_set$referat != "", ]

wordcounts <- out$referat %>% 
  map(function(x) str_count(x, boundary("word"))) %>%
  unlist()

out$referat = removeWords(out$referat, stopwords("danish"))
out$referat = removeNumbers(out$referat)
out$referat <- gsub("\\s*(?<!\\S)[a-zA-Z]{1,2}(?!\\S)", "", out$referat, perl=T)
out$referat <- gsub("[^\\w\\s]", "", out$referat, perl=T)

#Glove embeddings

library(text2vec)


# Create iterator over tokens
tokens <- space_tokenizer(out$referat)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
#glove$fit(tcm, n_iter = 20)
wv_main = fit_transform(tcm, glove, n_iter = 20)


wv_context = glove$components
dim(wv_context)


#find_similar_words("", word_vectors)

library(dplyr)
library(text2vec)

find_similar_words <- function(word, wv_main, n = 5) {
  similarities <- wv_main[word, , drop = FALSE] %>%
    sim2(wv_main, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

find_similar_words("digitalisering", wv_main, n=50)

word_vectors = wv_main + t(wv_context)


library(data.table)

responsiveness <- c("vælger", "klage", "protest", "borger", "forælder", "pendlere", "modtager", "rettigheder", "forpligtelse", "sagsbehandler")

dfs <- list()
for (i in responsiveness) {
  
  cosinesimilar <- tryCatch(as.data.frame(find_similar_words(c(i), wv_main, n=20), error = function(e) print(NA)))
  key <- i
  df <- data.frame(consinesimilar = cosinesimilar, key = key, stringsAsFactors = F)
  
  dfs[[length(dfs) + 1]] <- df
}

dfcosine <- do.call(rbind, setNames(dfs, NULL))
dfcosine$similar <- rownames(dfcosine)
dfcosine$similar <- removeNumbers(dfcosine$similar)
responsivenessdf <- unique(dfcosine$similar)

dfs2 <- list()
for(i in uncertainty){
  cosinesimilar <- tryCatch((find_similar_words(i, wv_main, n=20)),
                            error = function(e) print(NA))
  key <- i
  df2 <- data.frame(consinesimilar = cosinesimilar, key = key, stringsAsFactors = F)
  
  dfs2[[length(dfs2) + 1]] <- df2
}


dfcosine2 <- do.call(rbind, setNames(dfs2, NULL))
dfcosine2$similar <- rownames(dfcosine2)
dfcosine2$similar <- removeNumbers(dfcosine2$similar)
uncertaintydf <- unique(dfcosine2$similar)

################################################################
#Plot vectors of interest


### VISUALIZATION ###


library(ggplot2)
library(ggridges)
library(ggpointdensity)
library(viridis)
library(Rtsne)
library(ggplot2)
library(hrbrthemes)
library(plotly)

# HRBR Themes prep ######################################
hrbrthemes::import_roboto_condensed()
d <- read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)
d[grepl("Light", d$FontName),]$FamilyName <- font_rc_light
write.csv(d,extrafont:::fonttable_file(), row.names = FALSE)
extrafont::loadfonts()
#########################################################

# SIMPLE-TSNE

lookup <- c(dfcosine$similar)

responsevectors <- wv_main[row.names(wv_main) %in% lookup, ]
tsne <- Rtsne(responsevectors, perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(responsevectors)) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3, alpha = .6) +
  theme_ipsum_rc()
tsne_plot

# TSNE with density


tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(uncertvec)) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3, alpha = .6) +
  geom_pointdensity() +
  scale_color_viridis() +
  theme_ipsum_rc()

tsne_plot

# CONVEX HULL

#most outer points
convexhull <- chull(tsne$Y)

#plot
plot(tsne$Y, cex = 0.5)
hpts <- chull(tsne$Y)
hpts <- c(hpts, hpts[1])
lines(tsne$Y[hpts, ])

