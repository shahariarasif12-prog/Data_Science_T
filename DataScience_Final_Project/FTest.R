# ======= INSTALL PACKAGES =======
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("dbscan")
install.packages("factoextra")
install.packages("qdapDictionaries")
install.packages("textclean")
install.packages("stringr")
install.packages("tokenizers")
install.packages("hunspell")
install.packages("stopwords")
install.packages("textstem")
install.packages("stringi")
install.packages("knitr")
install.packages("Matrix")
install.packages("irlba")

# ===== LIBRARIES =====
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(ggplot2)
library(dbscan)
library(factoextra)
library(qdapDictionaries)
library(textclean)
library(stringr)
library(tokenizers)
library(hunspell)
library(stopwords)
library(textstem)
library(stringi)
library(parallel)
library(knitr)
library(Matrix)
library(irlba)

cat("All packages loaded successfully!\n")


#-------------------- Load Dataset -----------------------

mydata <- read.csv("C:/Users/My PC/Downloads/DataScience_Final_Project/pdataset.csv")

mydata <- mydata[1:5611, ]    # select[row:row,column:column] ; "(r=empty || c=empty)=Select ALL(row || column)"


# ------------------- Data Exploration --------------------

dim(mydata)          #Show Total number of rows and columns
names(mydata)        #Show Column names
summary(mydata)      #Show Dataset Summary

#Missing Value summary
#--------------------------
missing_summary <- sapply(mydata, function(col) sum(is.na(col) | trimws(as.character(col)) == ""))
print("Number of missing values per column:")
print(missing_summary)

#missing value in composite table view
#------------------------------------------
  missing_table <- mydata %>%
  mutate(across(everything(), as.character)) %>%  # make all columns character
  mutate(row_id = row_number()) %>%
  pivot_longer(-row_id, names_to = "column", values_to = "value") %>%
  filter(is.na(value) | trimws(value) == "")
  print(missing_table)

# Missing Values removal
#------------------------------
  colSums(is.na(mydata))                  # Show total missing values per column
mydata[mydata == ""] <- NA                # Convert blank strings ("") to NA (if present)
mydata <- na.omit(mydata)                 # Remove all rows with any NA values
sum(mydata$helpful == 0, na.rm = TRUE)
mydata <- mydata[mydata$helpful != 0, ]   # Remove rows where helpful is 0


# ==================== Outlier Detection &Removal ====================
  
  # Select numeric columns
  numeric_cols <- names(mydata)[sapply(mydata, is.numeric)]
# Initialize empty outlier table
outliers_table <- data.frame(Column=character(),
                             Row=integer(),
                             Value=numeric(),
                             Condition=character(),
                             stringsAsFactors=FALSE)

# Loop through numeric columns to detect outliers
for (colname in numeric_cols) {
  col <- mydata[[colname]]
  #rating column condition
  if (colname == "rating") {
    outlier_idx <- which(col < 1 | col > 5)
    if (length(outlier_idx) > 0) {
      outliers_table <- rbind(outliers_table,data.frame(Column=colname,Row=outlier_idx,Value=col[outlier_idx], Condition="Rating must be between 1 and 5"))
    }
  }
  #helpful column condition
  else if (colname == "helpful") {
    outlier_idx <- which(col < 1 | col > 30)
    if (length(outlier_idx) > 0) {
      outliers_table <- rbind(outliers_table,data.frame(Column=colname,Row=outlier_idx,Value=col[outlier_idx],Condition="Helpful votes must be between 1 and 30"))
    }
  }
  #IQR for other columns
  else {
    Q1 <- quantile(col, 0.25, na.rm=TRUE)
    Q3 <- quantile(col, 0.75, na.rm=TRUE)
    IQR_val <- Q3 - Q1
    lower <- Q1 - 1.5*IQR_val
    upper <- Q3 + 1.5*IQR_val
    
    outlier_idx <- which(col < lower | col > upper)
    if (length(outlier_idx) > 0) {
      outliers_table <- rbind(outliers_table,data.frame(Column=colname,Row=outlier_idx,Value=col[outlier_idx],Condition=paste0("Outside [", round(lower,2), ", ", round(upper,2), "]")))
    }
  }
}

#Show Outliers
if (nrow(outliers_table) > 0) {
  cat("Detected outliers (temporary dataset 'outliers_table'):\n")
  print(outliers_table)
} else {
  cat("No numeric outliers detected.\n")
}

#Remove outliers
if (nrow(outliers_table) > 0) {
  mydata <- mydata[-unique(outliers_table$Row), ]
  cat("\nOutliers containing rows removed.\n")
}


# Implement a local contraction replacer to avoid dependency/version issues
replace_contraction <- function(text) {
  if (is.na(text) || text == "") return(text)
  txt <- tolower(text)
  dict <- c(
    "can't"="cannot","won't"="will not","don't"="do not","didn't"="did not",
    "i'm"="i am","i've"="i have","i'll"="i will","i'd"="i would",
    "you're"="you are","you've"="you have","you'll"="you will","you'd"="you would",
    "he's"="he is","she's"="she is","it's"="it is","we're"="we are","we've"="we have",
    "they're"="they are","that's"="that is","there's"="there is","there're"="there are",
    "couldn't"="could not","shouldn't"="should not","wouldn't"="would not",
    "isn't"="is not","aren't"="are not","wasn't"="was not","weren't"="were not",
    "haven't"="have not","hasn't"="has not","hadn't"="had not","let's"="let us",
    "who's"="who is","what's"="what is","how's"="how is","where's"="where is",
    "y'all"="you all","o'clock"="of the clock","shan't"="shall not"
  )
  # replace each contraction using word boundaries (perl regex)
  for (k in names(dict)) {
    pattern <- paste0("\\b", gsub("([\\W])", "\\\\\\1", k), "\\b") # escape non-word chars
    txt <- gsub(pattern, dict[[k]], txt, perl = TRUE, ignore.case = TRUE)
  }
  txt
}

# -------------------- Step 1: Handle Contractions --------------------

corpus <- mydata$review_text
corpus_contraction <- vapply(corpus, replace_contraction, FUN.VALUE = character(1), USE.NAMES = FALSE)
changed_rows_contraction <- which(corpus != corpus_contraction)  # Detect affected rows

affected_rows_contraction <- data.frame(
  review_id = mydata$review_id[changed_rows_contraction],         # Review ID
  original_review_text = corpus[changed_rows_contraction],        # Original text
  after_contraction = corpus_contraction[changed_rows_contraction], # After contraction replacement
  stringsAsFactors = FALSE
)
cat("Rows affected by contraction replacement:\n")
print(affected_rows_contraction)


# -------------------- Step 2: Remove Emojis & Emoticons --------------------

# remove non-ascii characters (this removes emoji and many other symbols)
corpus_no_emoji <- iconv(corpus_contraction, from = "UTF-8", to = "ASCII", sub = "")

# remove common ASCII emoticons
emoticon_pattern <- "[:;=8][\\-^']?[)DPOp3(/|]"
corpus_no_emoji <- gsub(emoticon_pattern, "", corpus_no_emoji, perl = TRUE)

# -------------------- Step 3: Spell Checking (hunspell) --------------------

# Prepare a cleaned version for word splitting (keep letters and apostrophes)
pre_spell_text <- tolower(corpus_no_emoji)
pre_spell_text <- gsub("[^\\p{L}\\s']", " ", pre_spell_text, perl = TRUE)
all_words <- unique(unlist(strsplit(pre_spell_text, "\\s+")))
all_words <- all_words[nzchar(all_words)]

# Use parallel spell-correction if possible, otherwise fallback to lapply
num_cores <- max(1, detectCores(logical = TRUE) - 1)
cat("Using", num_cores, "core(s) for spell checking.\n")

# wrapper to guarantee a single string returned for each word
spell_correct_one <- function(word) {
  if (substr(word, 1, 1) == "@") return(word)   # keep handles intact
  if (hunspell_check(word)) return(word)
  sug <- hunspell_suggest(word)
  if (length(sug) > 0 && length(sug[[1]]) > 0) return(sug[[1]][1])
  return(word)
}

if (num_cores > 1) {
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, library(hunspell))
  clusterExport(cl, c("all_words", "spell_correct_one"), envir = environment())
  corrections_list <- parLapply(cl, all_words, spell_correct_one)
  stopCluster(cl)
} else {
  corrections_list <- lapply(all_words, spell_correct_one)
}

corrections_vec <- unlist(corrections_list, use.names = FALSE)
names(corrections_vec) <- all_words

# Map corrected words back into documents
corpus_spell <- vapply(pre_spell_text, function(txt) {
  words <- unlist(strsplit(txt, "\\s+"))
  words <- words[nzchar(words)]
  if (length(words) == 0) return("")
  corrected <- corrections_vec[words]
  # if some words are not found in corrections_vec, keep original
  na_idx <- which(is.na(corrected))
  if (length(na_idx) > 0) corrected[na_idx] <- words[na_idx]
  paste(corrected, collapse = " ")
}, FUN.VALUE = character(1), USE.NAMES = FALSE)

# -------------------- Step 4: Clean Text (remove punctuation/numbers/non-alpha) --------------------

clean_text <- function(text) {
  text <- tolower(text)
  text <- gsub("<.*?>", " ", text)
  text <- gsub("[[:punct:]]+", " ", text)
  text <- gsub("[0-9]+", " ", text)
  # keep only ascii letters and spaces
  text <- gsub("[^a-z\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}
corpus_clean <- sapply(corpus_spell, clean_text)                     # Apply cleaning
changed_rows_clean <- which(corpus_spell != corpus_clean)            # Detect changes

affected_rows_clean <- data.frame(
  review_id = mydata$review_id[changed_rows_clean],                  # Review ID
  original_review_text = corpus_spell[changed_rows_clean],           # Before cleaning
  after_cleaning = corpus_clean[changed_rows_clean],                 # After cleaning
  stringsAsFactors = FALSE
)
cat("Rows affected by text cleaning:\n")
print(affected_rows_clean)

# -------------------- Step 5: Tokenization (Cleaned Corpus) --------------------

tokens_list <- tokenize_words(corpus_clean, lowercase = TRUE)
tokens_df <- data.frame(
  review_id = mydata$review_id,
  tokens = sapply(tokens_list, function(x) paste(x, collapse = ", ")),
  stringsAsFactors = FALSE
)
head(tokens_df, 10)  # Preview


# -------------------- Step 6: Remove Stopwords --------------------

stopwords_set <- tolower(stopwords::stopwords("en"))
tokens_no_stop <- lapply(tokens_list, function(x) x[!x %in% stopwords_set])

tokens_no_stop_df <- data.frame(
  review_id = mydata$review_id,
  tokens_no_stop = sapply(tokens_no_stop, function(x) paste(x, collapse = ", ")),
  stringsAsFactors = FALSE
)

head(tokens_no_stop_df, 20)
View(tokens_no_stop_df)   # Optionally view in RStudio viewer


# -------------------- Step 7: Lemmatization --------------------

tokens_lemmatized <- lapply(tokens_no_stop, lemmatize_words)

tokens_lemmatized_df <- data.frame(
  review_id = mydata$review_id,
  tokens_lemmatized = sapply(tokens_lemmatized, function(x) paste(x, collapse = " ")),
  stringsAsFactors = FALSE
)
head(tokens_lemmatized_df, 20)
View(tokens_lemmatized_df)

# -------------------- Step 8: Create TF-IDF Matrix --------------------
tokens_tidy <- tokens_lemmatized_df %>%
  unnest_tokens(word, tokens_lemmatized, token = "words") %>%
  filter(word != "")

term_freq <- tokens_tidy %>%
  count(review_id, word, sort = TRUE)

# TF-IDF calculation
tf_idf <- term_freq %>%
  bind_tf_idf(word, review_id, n) %>%
  mutate(tf_idf = as.numeric(tf_idf))

# Create sparse matrix
dtm_tfidf <- tf_idf %>%
  cast_sparse(review_id, word, tf_idf)
dim(dtm_tfidf)  # Check matrix size
tf_idf_df <- tf_idf %>%
  select(review_id, word, tf_idf) %>%  # Keep only relevant columns
  arrange(review_id, desc(tf_idf))      # Sort by document and TF-IDF score

# View first 20 rows in data frame
head(tf_idf_df, 20)
# Optionally, view as a full data frame in RStudio viewer
View(tf_idf_df)

#-------------wide tf_idf--------
tf_idf_wide <- tf_idf_df %>%
  pivot_wider(
    names_from = word,
    values_from = tf_idf,
    values_fill = 0
  )
# Check dimensions and preview
dim(tf_idf_wide)
head(tf_idf_wide[, 1:10])  # Show first 10 word columns for preview
View(tf_idf_wide)

# Remove any zero rows
row_sums <- Matrix::rowSums(dtm_tfidf)
dtm_tfidf <- dtm_tfidf[row_sums > 0, , drop = FALSE]
row_ids <- rownames(dtm_tfidf)

# -------------------- Step 9: PCA + Clustering --------------------

n_components <- min(50, ncol(dtm_tfidf), nrow(dtm_tfidf) - 1)
if (n_components < 2) stop("Not enough data/terms to compute SVD. Reduce requirements or check preprocessing.")

cat("Computing truncated SVD with", n_components, "components (irlba)...\n")
svd_res <- irlba::irlba(dtm_tfidf, nv = n_components, maxit = 1000)

# Document coordinates in reduced space:
pca_data <- svd_res$u %*% diag(svd_res$d)
rownames(pca_data) <- row_ids
colnames(pca_data) <- paste0("PC", seq_len(n_components))

# ==================== K-Means CLUSTERING ====================
set.seed(123)
k <- 5
kmeans_res <- kmeans(pca_data, centers = k, nstart = 25)

pca_df <- data.frame(
  PC1 = pca_data[, 1],
  PC2 = pca_data[, 2],
  cluster = factor(kmeans_res$cluster),
  review_id = rownames(pca_data),
  stringsAsFactors = FALSE
)

centers <- data.frame(
  PC1 = kmeans_res$centers[, 1],
  PC2 = kmeans_res$centers[, 2]
)

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_point(data = centers, aes(x = PC1, y = PC2), color = "black", size = 3, shape = 16) +
  labs(title = "K-Means Clusters (first 2 components)", x = "PC1", y = "PC2") +
  theme_minimal()

# ==================== DBSCAN CLUSTERING ====================
set.seed(123)
dbscan_res <- dbscan::dbscan(pca_data[, 1:2], eps = 1.5, minPts = 5)
dbscan_df <- data.frame(
  PC1 = pca_data[, 1],
  PC2 = pca_data[, 2],
  cluster = factor(dbscan_res$cluster)
)
ggplot(dbscan_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "DBSCAN Clusters (first 2 components)", x = "PC1", y = "PC2") +
  theme_minimal()

# ====================  HIERARCHICAL CLUSTERING ====================
hc_dims <- min(10, n_components)
dist_matrix <- dist(pca_data[, 1:hc_dims])
hc_res <- hclust(dist_matrix, method = "ward.D2")
hc_clusters <- cutree(hc_res, k = k)
hc_df <- data.frame(
  PC1 = pca_data[, 1],
  PC2 = pca_data[, 2],
  cluster = factor(hc_clusters)
)
ggplot(hc_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Hierarchical Clusters (first 2 components)", x = "PC1", y = "PC2") +
  theme_minimal()


# -------------------- Step 10: Interpretation --------------------

cat("\n===== INTERPRETATION OF RESULTS =====\n")
cat("1. K-Means formed", k, "clusters (see K-Means plot). Each cluster groups reviews with similar word usage.\n")
cat("2. DBSCAN identified dense regions and treats sparse/noisy items as cluster 0 (noise).\n")
cat("3. Hierarchical clustering gives a tree-like perspective on similarity.\n")
cat("Notes:\n")
cat("- Contractions handled by a local mapping function (robust across package versions).\n")
cat("- Emojis removed via iconv() to avoid fragile \\U escapes.\n")
cat("- Truncated SVD (irlba) used to avoid converting sparse matrices to dense (memory-friendly).\n")
