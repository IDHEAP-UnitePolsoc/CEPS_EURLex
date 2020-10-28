
# EURLex Selection on reduced dataset
#####################################

setwd("/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/NCCR Project/CEPS_EURLex") # <- adapt; also download full CEPS dataset

# <- install if necessary
library("tm")
    library("tidyverse")
    library("stringr")
    library("textclean")
    library("udpipe")
    library("xtable")


# Sources: 
# https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

data <- read.csv("eurlex_notext.csv", sep = ";", stringsAsFactors = F, encoding = "Latin-1")

# Create vocabulary of EUROVOC terms used
#########################################

# Subsetting
data <- data[c("CELEX","Subject_matter")]

# Clean up misformatted special characters
data$Subject_matter <- replace_non_ascii(data$Subject_matter)

# Remove white space after ';'
data$Subject_matter <- str_replace_all(data$Subject_matter,"; ",";")

# Calculate frequencies   
dtm <- document_term_frequencies(x=data$Subject_matter,document = data$CELEX, split = ";")
    dtm <- document_term_matrix(dtm)
    length(colnames(dtm)) # <- we use "Subject_matter" because EURVOC results in ~8500 unique categories; not useful
    colnames(dtm)

# Turn to data.frame and sort    
count <-  as.data.frame(dtm_colsums(dtm))
    count$phrase <- rownames(count)
    names(count)[names(count)=="dtm_colsums(dtm)"] <- "freq"
    count <- count[order(-count$freq),]
        rm(dtm)
# cleaning    
count <- count[ !(count$phrase %in% c("NA","character(0)")),] # <-  removes missings
    count$id <- 1:nrow(count)
    row.names(count) <- count$id
    count$id <- NULL

# Export to table
table <- xtable(count, caption = "Frequencies of subject matters in EURLex Dataset")
    align(table) <- xalign(table)
    digits(table) <- xdigits(table)
    display(table) <- xdisplay(table)
    print(table, hline.after=c(-1,0), tabular.environment = "longtable", file = "freq.tex")
    

    
data <- read.csv("eurlex_notext.csv", sep = ";", stringsAsFactors = F, encoding = "Latin-1")
# More elaborate analysis
data <- data[c("CELEX","Act_name","Act_type","EUROVOC","Subject_matter","Date_document","Date_publication")]

# Create year variable
data$date <- as.Date(data$Date_document)
    data$year <- format(data$date, '/%Y')
    data$year <- as.numeric(gsub("/","",data$year))

# Create variable to count key words 
data$migr <- as.integer(grepl(pattern = "[A-z]ommunity worker",x=data$EUROVOC) | 
                            grepl(pattern = "[A-z]ree movement of workers",x=data$EUROVOC) |
                            grepl(pattern = "[A-z]iscrimination against foreigners", x=data$EUROVOC))
    data$social <- as.integer((grepl(pattern = "[A-z]ocial",x=data$EUROVOC) |
                              grepl(pattern = "national insurance", x=data$EUROVOC)) &
                              !grepl(pattern = "socialist",x=data$EUROVOC))
    data$ref <- as.integer(grepl(pattern="[A-z]efugee", x=data$EUROVOC))
    
# Create matrix/df of word frequencies per year
migr <- aggregate(data$migr,by=list(data$year),FUN=mean,na.rm=T)
        migr <- migr %>% rename(year = Group.1, migr=x)
    social <- aggregate(data$social,by=list(data$year),FUN=mean,na.rm=T)
        social <- social %>% rename(year = Group.1, social=x)
    ref <- aggregate(data$ref, by=list(data$year),FUN=mean,na.rm=T)
        ref <- ref %>% rename(year=Group.1,ref=x)
    
cond <- merge(migr,social, by="year")
    #cond <- merge(cond,socdis, by="year")
    cond <- merge(cond,ref, by="year")
    rm(migr,social,ref)
    
   
# Graph
ggplot(cond, aes(x=year)) + 
    geom_line(aes(y=migr, colour="migr",linetype="migr")) +
    geom_line(aes(y=social, colour="social",linetype="social")) +
    geom_line(aes(y=ref,colour="ref",linetype="ref")) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name="Share of all EU legislation") + 
    theme(legend.position = "bottom") + 
    #guides(colour=guide_legend(nrow = 2),linetype=guide_legend(nrow=2)) +
        scale_colour_manual("",
                            breaks=c("migr","social","ref"),
                            values = c("migr"="black","social"="gray","ref"="darkgreen"),
                            labels=c("Free movement","Social policy","Refugees")) +
        scale_linetype_manual("",
                              breaks=c("migr","social","ref"),
                              values = c("migr"="solid","social"="longdash","ref"="solid"),
                              labels=c("Free movement","Social policy","Refugees"))
    ggsave("eurlex_legisshares.pdf")

 #   annotation_custom(grob=textGrob("Based on EUROVOC keywords as follows: Free movement ('community worker','free movement of workers','discrimination against foreigners); Social policy ('social','national insurance', but excl. 'socialist'); Refugees ('Refugee')."),)
    
    
        
        
# Select obs with reference to social policy, employment, migration, refugees
sub <- data[grepl("migra",data[["EUROVOC"]]) | grepl("refugee",data[["EUROVOC"]]) |
            grepl("social",data[["EUROVOC"]]) | grepl("employment",data[["EUROVOC"]]),]
    rm(data)

# Create corpus
corpus <- SimpleCorpus(VectorSource(data$EUROVOC))
    inspect(corpus)
    
# Remove stopwords
corpus <- tm_map(corpus, removeWords, 
                 c(stopwords("english")))

# Create document-term matrix
dtm <- DocumentTermMatrix(corpus)
    inspect(dtm)

# Create word cloud    
sums <- as.data.frame(colSums(as.matrix(dtm)))
    sums <- rownames_to_column(sums)
    colnames(sums) <- c("term","count")
    sums <- arrange(sums, desc(count))
