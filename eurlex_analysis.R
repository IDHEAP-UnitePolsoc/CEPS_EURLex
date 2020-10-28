##############################################################
# EURLEX - Further pre-processing with Quanteda, then analysis
##############################################################

library("readtext")
    library("quanteda")
    library("topicmodels")
    theme_set(theme_minimal())

setwd("/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/NCCR Project/CEPS_EURLex")

# Reading in data
data <- readtext("eurlex_reduced.csv", text_field = "act_raw_text")
    names(data)
    
# Constructing corpus
eurlex_corp <- corpus(data)

# Renaming doc-IDs
docid <- paste(eurlex_corp$year,
               eurlex_corp$CELEX,
               eurlex_corp$Act_type)

docnames(eurlex_corp) <- docid
    
# Document variables
head(docvars(eurlex_corp))

# Creating tokens <- this takes long!
toks <- tokens(eurlex_corp)
    rm(data,docid) # <- free up memory

# keep only English-language words <- !! THIS IS CONSEQUENTIAL !!
toks <- tokens_select(toks,names(data_int_syllables))
    toks <- tokens_select(toks, pattern = stopwords('en'), selection = 'remove') # irrelevant words remained
    # toks <- tokens_select(toks, pattern = c('shall','may')) <-  think about it
    toks <- tokens_keep(toks,min_nchar=3) # removes single letters and stuff like 'it' or 'oj'
    toks <- tokens_wordstem(toks) # stemming; consider if this makes sense; pior LDA suggests yes

# Creating document-feature matrix
dfmat <- dfm(toks)
    rm(toks,eurlex_corp) # <- remove more clutter
    print(dfmat,max_ndoc=2)    
    ndoc(dfmat) # <- slightly lower
    nfeat(dfmat)
    head(dfmat, nf=5)

# First inspection
topfeatures(dfmat,n=100)
    
# Preparation for LDA <- keeping most frequent features that appear in less than 10% of documents
# based on: https://tutorials.quanteda.io/machine-learning/topicmodel/

dfmat_trim <- dfm_trim(dfmat, min_termfreq = 0.95, termfreq_type = "quantile",
                       max_docfreq = 0.10, docfreq_type = "prop")
dfmat_trim <- dfmat_trim[ntoken(dfmat_trim)>0,]

topfeatures(dfmat_trim,n=100) # <- seems much is still related to agriculture and product markets

# Wordcloud
set.seed(12345)
sample <- dfm_sample(dfmat_trim,size = 10,margin = "documents")
    pdf("wordcloud_eurlex.pdf")
    textplot_wordcloud(sample, max_words = 300,min_size=1.2,max_size = 5)
    dev.off()
    rm(sample)
    
# LDA estimation
################
lda <- LDA(dfmat_trim,k=5) # <- number of topics=10?; takes long!  
    terms(lda, 20)


# K-means estimation
###################
dfmat_idf <- dfm_tfidf(dfmat_trim,base = 2)    
    k <- 3
    
set.seed(42)    
km_out <- stats::kmeans(dfmat_idf,centers=k,nstart = 50)
    km_out$iter
    summary(km_out)
    km_out$size

    
    