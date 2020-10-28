###############################################
# CEPS-EURLex Data on EU legislation, 1952-2019
###############################################

options(stringsAsFactors = FALSE)
setwd("/Users/carloknotz/Dropbox (IDHEAP)/NCCR_WelfareSolidarity/NCCR Project/CEPS_EURLex") # <- adapt; also download full CEPS dataset

# <- install if necessary
library("tm")
    library("tidyverse")
    library("vroom")
    library("textclean")
    library("ISOcodes")

# Sources: 
# https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
# https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf


# Reading in data <- THIS CAN TAKE SOME TIME
data <- vroom("eurlex_all.csv",
              col_types = list(CELEX="c",Act_name="c", Act_type="c", 
                               Status="c", EUROVOC="c", Subject_matter="c", 
                               Treaty="c", Legal_basis_celex="c", Authors="c",
                               Procedure_number="c", Date_document="D",Date_publication="D",
                               First_entry_into_force="c",Temporal_status="c",
                               Act_cites="c",Cites_links="c",Act_ammends="c",
                               Ammends_links="c",Eurlex_link="c",ELI_link="c",
                               Proposal_link="c",Oeil_link="c",Additional_info="c",
                               act_raw_text="c"))

# subsetting
data <- data[c("CELEX","Act_name","Treaty","Act_type","Subject_matter","Date_document","Date_publication","act_raw_text")]


# # Having regard to the Treaty on the Functioning of the European Union, and in particular Article 48
# data$art48tfeu <- as.integer(
#   grepl(
#     pattern ="Having regard to the Treaty on the Functioning of the European Union, and in particular Article[[:print:]]{,20}48",
#     x=data$act_raw_text)) # <- 27 hits
# 
# data$art51eec <- as.integer(
#   grepl(
#     pattern="Having regard to the Treaty establishing the European Economic Community, and in particular Article[[:print:]]{,20}51",
#     x=data$act_raw_text)) # <- 24 hits
# 
# 
# data$art48eec <- as.integer(
#   grepl(
#     pattern="Having regard to the Treaty establishing the European Economic Community, and in particular Article[[:print:]]{,20}48",
#     x=data$act_raw_text)) # <- 2 hits 
# 


# generate case ID
data$id <- 1:nrow(data)


# Subsetting according to eurlex_reduced.R results
key <- c("health|employment|social affairs|social protection|
         education|insurance|family|social framework")

data$keep <- as.integer(grepl(pattern = key,x=data$Subject_matter))
  sum(as.numeric(data$keep),na.rm = T)
  

# Subsetting, retaining all laws as identified with keywords
data <- subset(data,keep==1)
  

# Define additional terms to be excluded
remove <- c("Article","Parliament","Regulation","Decision","Member states","EU","THE","OF","For","The",
              "Union","European","Commission","Economic and Social Committee","Committee of the Regions",
              "Journal","Official","Council","Paragraph","President","January","February","March","April","May","June","July","August",
              "September","October","November","December","EUR","EC","EN","No ","DECISION","HAVE ADOPTED THIS",
              "EUROPEAN","PARLIAMENT","AND","COUNCIL","COMMISSION","UNION"," p. "," OJ ","Strasbourg",
              "HAS ADOPTED THIS","ANNEX","Annex","IMPLEMENTING", "AND", "THIS","This"," C ", " L ", " X ",
              "(C)","(R.O.C.)","Yes","yes","YES") # " p. "," L "

ccodes <- ISO_3166_1$Alpha_2 # <- ISO country codes to be removed from text

bullets <- c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)","(i)","(j)","(k)","(l)","(m)","(n)","(o)","(p)",
             "(q)","(r)","(s)","(t)","(u)","(v)","(w)","(x)","(y)","(z)")

roman <- c(" I "," II "," III "," IV "," V "," VI "," VII "," VIII "," IX "," X "," XI "," XII "," XIII ",
           " XV "," XIV "," XVI "," XVII "," XVIII")
             
# Clean text
############
data$act_raw_text <- replace_non_ascii(data$act_raw_text) # <- some non-ASCII chars remain, cleaned with this
  detach("package:textclean",unload=T)

# Strip irrelevant words
for(i in 1:nrow(data)){
  data$act_raw_text[i] <- removeWords(data$act_raw_text[i],c(stopwords("english"),remove,ccodes,bullets,roman))
  data$act_raw_text[i] <- removeNumbers(data$act_raw_text[i])
  data$act_raw_text[i] <- removePunctuation(data$act_raw_text[i],preserve_intra_word_contractions = TRUE, preserve_intra_word_dashes = TRUE)
  data$act_raw_text[i] <- stripWhitespace(data$act_raw_text[i])
}

data$act_raw_text[1234]

# Create year variable
data$date <- as.Date(data$Date_document)
  data$year <- format(data$date, '/%Y')
  data$year <- as.numeric(gsub("/","",data$year))

write.csv(data,file="eurlex_reduced.csv")  
