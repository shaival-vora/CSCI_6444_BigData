# installing the required packages
install.packages('tm')
install.packages('dplyr')
install.packages('tidytext')
install.packages('ggplot2')
install.packages('wordcloud')

# loading the installed packages
library('tm')
library('dplyr')
library('tidytext')
library('ggplot2')
library('wordcloud')

# setting the directory
setwd("C:/Users/anith/OneDrive/Desktop/GW/sem 2/Big Daata/project 3")
getwd()
# loading the text file
Book <- readLines('TarzanOfTheApes.txt')
Book

# separating the chapters from 1 to 15 into seperate files and identifying the indices
index_chp1 <- which(Book == "Chapter I", arr.ind=TRUE)
index_chp2 <- which(Book == "Chapter II", arr.ind=TRUE)
index_chp3 <- which(Book == "Chapter III", arr.ind=TRUE)
index_chp4 <- which(Book == "Chapter IV", arr.ind=TRUE)
index_chp5 <- which(Book == "Chapter V", arr.ind=TRUE)
index_chp6 <- which(Book == "Chapter VI", arr.ind=TRUE)
index_chp7 <- which(Book == "Chapter VII", arr.ind=TRUE)
index_chp8 <- which(Book == "Chapter VIII", arr.ind=TRUE)
index_chp9 <- which(Book == "Chapter XI", arr.ind=TRUE)
index_chp10 <- which(Book == "Chapter X", arr.ind=TRUE)
index_chp11 <- which(Book == "Chapter XI", arr.ind=TRUE)
index_chp12 <- which(Book == "Chapter XII", arr.ind=TRUE)
index_chp13 <- which(Book == "Chapter XIII", arr.ind=TRUE)
index_chp14 <- which(Book == "Chapter XIV", arr.ind=TRUE)
index_chp15 <- which(Book == "Chapter XV", arr.ind=TRUE)
index_chp16 <- which(Book == "Chapter XVI", arr.ind=TRUE)

index_chp1
# extracting the text from chapters
chp1 <- Book[(index_chp1+1):(index_chp2-1)]
chp2 <- Book[(index_chp2+1):(index_chp3-1)]
chp3 <- Book[(index_chp3+1):(index_chp4-1)]
chp4 <- Book[(index_chp4+1):(index_chp5-1)]
chp5 <- Book[(index_chp5+1):(index_chp6-1)]
chp6 <- Book[(index_chp6+1):(index_chp7-1)]
chp7 <- Book[(index_chp7+1):(index_chp8-1)]
chp8 <- Book[(index_chp8+1):(index_chp9-1)]
chp9 <- Book[(index_chp9+1):(index_chp10-1)]
chp10 <- Book[(index_chp10+1):(index_chp11-1)]
chp11 <- Book[(index_chp11+1):(index_chp12-1)]
chp12 <- Book[(index_chp12+1):(index_chp13-1)]
chp13 <- Book[(index_chp13+1):(index_chp14-1)]
chp14 <- Book[(index_chp14+1):(index_chp15-1)]
chp15 <- Book[(index_chp15+1):(index_chp16-1)]

chp1

# creating a directory for the chapters 
dir.create('Chapters')

# writing each chapter to a text file
write.table(chp1, file = 'Chapters/chp1.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp2, file = 'Chapters/chp2.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp3, file = 'Chapters/chp3.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp4, file = 'Chapters/chp4.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp5, file = 'Chapters/chp5.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp6, file = 'Chapters/chp6.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp7, file = 'Chapters/chp7.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp8, file = 'Chapters/chp8.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp9, file = 'Chapters/chp9.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp10, file = 'Chapters/chp10.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp11, file = 'Chapters/chp11.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp12, file = 'Chapters/chp12.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp13, file = 'Chapters/chp13.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp14, file = 'Chapters/chp14.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chp15, file = 'Chapters/chp15.txt', sep = '\t', row.names = FALSE, col.names = FALSE, quote = FALSE)

# creating a VCorpus on the text files
CORP <- VCorpus(DirSource('./Chapters', ignore.case = TRUE, mode = 'text'))
str(CORP)
CORP

# extracting text from VCorpus
vtext <- CORP[[1]]
vtext
#retrieving the content
vtext[1]

# creating a Document Term Matrix
CORP_DTM <- DocumentTermMatrix(CORP)
CORP_DTM
inspect(CORP_DTM)
str(CORP_DTM)

# creating a Term Document Matrix
CORP_TDM <- TermDocumentMatrix(CORP)
CORP_TDM
inspect(CORP_TDM)
str(CORP_TDM)

# converting the corpus file to tidy corpus file
tidyCORP <- tidy(CORP)
tidyCORP

# finding the 10 longest words from all the chapters
CORPWords<-tidyCORP %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords

# finding the 10 longest sentences from all the chapters
CORPSentences<-tidyCORP %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences

# finding the 10 longest words and sentences from each chapter

#chapter1
tidyCORP_chp1 <- tidy(CORP[1])
tidyCORP_chp1
CORPWords_chp1<-tidyCORP_chp1 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp1
CORPSentences_chp1<-tidyCORP_chp1 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp1

#chapter2
tidyCORP_chp2 <- tidy(CORP[8])
tidyCORP_chp2
CORPWords_chp2<-tidyCORP_chp2 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp2
CORPSentences_chp2<-tidyCORP_chp2 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp2

#chapter3
tidyCORP_chp3 <- tidy(CORP[9])
tidyCORP_chp3
CORPWords_chp3<-tidyCORP_chp3 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp3
CORPSentences_chp3<-tidyCORP_chp3 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp3

#chapter4
tidyCORP_chp4 <- tidy(CORP[10])
tidyCORP_chp4
CORPWords_chp4<-tidyCORP_chp4 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp4
CORPSentences_chp4<-tidyCORP_chp4 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp4

#chapter5
tidyCORP_chp5 <- tidy(CORP[11])
tidyCORP_chp5
CORPWords_chp5<-tidyCORP_chp5 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp5
CORPSentences_chp5<-tidyCORP_chp5 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp5

#chapter6
tidyCORP_chp6 <- tidy(CORP[12])
tidyCORP_chp6
CORPWords_chp6<-tidyCORP_chp6 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp6
CORPSentences_chp6<-tidyCORP_chp6 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp6

#chapter7
tidyCORP_chp7 <- tidy(CORP[13])
tidyCORP_chp7
CORPWords_chp7<-tidyCORP_chp7 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp7
CORPSentences_chp7<-tidyCORP_chp7 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp7

#chapter8
tidyCORP_chp8 <- tidy(CORP[14])
tidyCORP_chp8
CORPWords_chp8<-tidyCORP_chp8 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp8
CORPSentences_chp8<-tidyCORP_chp8 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp8

#chapter9
tidyCORP_chp9 <- tidy(CORP[15])
tidyCORP_chp9
CORPWords_chp9<-tidyCORP_chp9 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp9
CORPSentences_chp9<-tidyCORP_chp9 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp9

#chapter10
tidyCORP_chp10 <- tidy(CORP[2])
tidyCORP_chp10
CORPWords_chp10<-tidyCORP_chp10 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp10
CORPSentences_chp10<-tidyCORP_chp10 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp10

#chapter11
tidyCORP_chp11 <- tidy(CORP[3])
tidyCORP_chp11
CORPWords_chp11<-tidyCORP_chp11 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp11
CORPSentences_chp11<-tidyCORP_chp11 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp11

#chapter12
tidyCORP_chp12 <- tidy(CORP[4])
tidyCORP_chp12
CORPWords_chp12<-tidyCORP_chp12 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp12
CORPSentences_chp12<-tidyCORP_chp12 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp12

#chapter13
tidyCORP_chp13 <- tidy(CORP[5])
tidyCORP_chp13
CORPWords_chp13<-tidyCORP_chp13 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp13
CORPSentences_chp13<-tidyCORP_chp13 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp13

#chapter14
tidyCORP_chp14 <- tidy(CORP[6])
tidyCORP_chp14
CORPWords_chp14<-tidyCORP_chp14 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp14
CORPSentences_chp14<-tidyCORP_chp14 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp14

#chapter15
tidyCORP_chp15 <- tidy(CORP[7])
tidyCORP_chp15
CORPWords_chp15<-tidyCORP_chp15 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
CORPWords_chp15
CORPSentences_chp15<-tidyCORP_chp15 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
CORPSentences_chp15

# cleaning the corpus
# removing quotes from the corpus
CORP<-tm_map(CORP, content_transformer(gsub), pattern="'", replacement="")

#removing numbers and punctuates
Remove_Num_Punct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
CORP_clean <- tm::tm_map(CORP, content_transformer(Remove_Num_Punct))
CORP_clean
str(CORP_clean)
inspect(CORP_clean)
inspect(CORP)

#make sure that the content is in lowercase
CORP_low<-tm_map(CORP_clean, tm::content_transformer(tolower))
CORP_low
str(CORP_low)
inspect(CORP_low)

#Computing the Document Term Matrix to the cleansed corpus
CORP_DTM<-DocumentTermMatrix(CORP_low)
CORP_DTM
str(CORP_DTM)
inspect(CORP_DTM)

#removing stop words from the corpus
StopWords<-c(tm::stopwords("english"))
StopWords
CORP_StopWords<-tm::tm_map(CORP_low,tm::removeWords,StopWords)
tm::inspect(CORP_StopWords[[1]])

#creating the term document matrix to the cleansed corpus with no stop words
CORP_StopWords_TDM<-tm::TermDocumentMatrix(CORP_StopWords)
CORP_StopWords_TDM

#finding frequent terms with low frequency as 5
Freq_Terms<-tm::findFreqTerms(CORP_StopWords_TDM, lowfreq = 5)
Freq_Terms

#Number of frequent terms with low frequency as 5
length(Freq_Terms)
#Finding length of a random frequent term
Freq_Terms[15]
nchar(Freq_Terms[15])

#computing term frequencies for each term in the documents
tfList<-list()
for(i in seq_along(CORP_StopWords)){
  tfList[[i]]<-termFreq(CORP_StopWords[[i]])
}
print(tfList[[1]])
print(tfList[[2]])
print(tfList[[3]])
print(tfList[[4]])
print(tfList[[5]])
print(tfList[[6]])
print(tfList[[7]])
print(tfList[[8]])
print(tfList[[9]])
print(tfList[[10]])
print(tfList[[11]])
print(tfList[[12]])
print(tfList[[13]])
print(tfList[[14]])
print(tfList[[15]])

#inspecting the term document matrix for corpus with no stop words 
tm::inspect(CORP_StopWords_TDM)

#generating the dendogram
CORP_df <- as.data.frame(as.matrix(CORP_StopWords_TDM))
CORP_dist <- dist(CORP_df)
CORP_dg <- hclust(CORP_dist, method ='ward.D2')
str(CORP_dg)

#plotting the dendogram
plot(CORP_dg)

#removing more sparsity
CORP_StopWords_TDM <- removeSparseTerms(CORP_StopWords_TDM, sparse = 0.2)

#generating the new dendogram
CORP_df <- as.data.frame(as.matrix(CORP_StopWords_TDM))
CORP_dist <- dist(CORP_df)
CORP_dg <- hclust(CORP_dist, method ='ward.D2')
str(CORP_dg)
tm::inspect((CORP_StopWords_TDM))

#plotting the new dendogram
plot(CORP_dg)

#creating wordcloud
tf_comb <- unlist(tfList)
tf_summ <- tapply(tf_comb, names(tf_comb), sum)
Words <- names(tf_summ)
Words

#making the wordcloud
pal <- brewer.pal(9,'BuGn')
str(pal)
CORP_WC <- wordcloud(Words, tf_summ, colors = pal[-(1:4)])
str(CORP_WC)

#setting the min.freq =30
CORP_WC <- wordcloud(Words, tf_summ, min.freq = 30, colors = pal[-(1:4)])

#using a different pallet
pal2 <- brewer.pal(9,'Spectral')
CORP_WC <- wordcloud(Words, tf_summ, colors = pal2)
str(CORP_WC)

#applying functions on the corpus
install.packages('quanteda')
library('quanteda')

#Printing first ten lines of first document
CORP_text <- CORP_clean[[1]]
CORP_text$content[1:10]

#tokenization
CORP_tokens <-lapply(CORP_clean,function(x) quanteda::tokens(x$content))
CORP_tokens

#applying kwic on the corpus
kwic(CORP_tokens[[1]], pattern = "great")
kwic(CORP_tokens[[2]], pattern = "great")
kwic(CORP_tokens[[3]], pattern = "great")

CORP_DFM_Chp1 <- quanteda::dfm(CORP_tokens[[1]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp2 <- quanteda::dfm(CORP_tokens[[2]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp3 <- quanteda::dfm(CORP_tokens[[3]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp4 <- quanteda::dfm(CORP_tokens[[4]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp5 <- quanteda::dfm(CORP_tokens[[5]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp6 <- quanteda::dfm(CORP_tokens[[6]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp7 <- quanteda::dfm(CORP_tokens[[7]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp8 <- quanteda::dfm(CORP_tokens[[8]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp9 <- quanteda::dfm(CORP_tokens[[9]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp10 <- quanteda::dfm(CORP_tokens[[10]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp11 <- quanteda::dfm(CORP_tokens[[11]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp12 <- quanteda::dfm(CORP_tokens[[12]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp13 <- quanteda::dfm(CORP_tokens[[13]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp14 <- quanteda::dfm(CORP_tokens[[14]]) %>% dfm_remove(StopWords)
CORP_DFM_Chp15 <- quanteda::dfm(CORP_tokens[[15]]) %>% dfm_remove(StopWords)

str(CORP_DFM_Chp1)
str(CORP_DFM_Chp6)
str(CORP_DFM_Chp12)

#getting frequency of terms from the DFM
CORP_Freq_Chp1 <- quanteda::docfreq(CORP_DFM_Chp1)
CORP_Freq_Chp2 <- quanteda::docfreq(CORP_DFM_Chp2)
CORP_Freq_Chp3 <- quanteda::docfreq(CORP_DFM_Chp3)
CORP_Freq_Chp4 <- quanteda::docfreq(CORP_DFM_Chp4)
CORP_Freq_Chp5 <- quanteda::docfreq(CORP_DFM_Chp5)
CORP_Freq_Chp6 <- quanteda::docfreq(CORP_DFM_Chp6)
CORP_Freq_Chp7 <- quanteda::docfreq(CORP_DFM_Chp7)
CORP_Freq_Chp8 <- quanteda::docfreq(CORP_DFM_Chp8)
CORP_Freq_Chp9 <- quanteda::docfreq(CORP_DFM_Chp9)
CORP_Freq_Chp10 <- quanteda::docfreq(CORP_DFM_Chp10)
CORP_Freq_Chp11 <- quanteda::docfreq(CORP_DFM_Chp11)
CORP_Freq_Chp12 <- quanteda::docfreq(CORP_DFM_Chp12)
CORP_Freq_Chp13 <- quanteda::docfreq(CORP_DFM_Chp13)
CORP_Freq_Chp14 <- quanteda::docfreq(CORP_DFM_Chp14)
CORP_Freq_Chp15 <- quanteda::docfreq(CORP_DFM_Chp15)

str(CORP_Freq_Chp3)
CORP_Freq_Chp3

#assigning weights to the words
CORP_Weight_Chp1 <- quanteda::dfm_weight(CORP_DFM_Chp1)
CORP_Weight_Chp2 <- quanteda::dfm_weight(CORP_DFM_Chp2)
CORP_Weight_Chp3 <- quanteda::dfm_weight(CORP_DFM_Chp3)
CORP_Weight_Chp4 <- quanteda::dfm_weight(CORP_DFM_Chp4)
CORP_Weight_Chp5 <- quanteda::dfm_weight(CORP_DFM_Chp5)
CORP_Weight_Chp6 <- quanteda::dfm_weight(CORP_DFM_Chp6)
CORP_Weight_Chp7 <- quanteda::dfm_weight(CORP_DFM_Chp7)
CORP_Weight_Chp8 <- quanteda::dfm_weight(CORP_DFM_Chp8)
CORP_Weight_Chp9 <- quanteda::dfm_weight(CORP_DFM_Chp9)
CORP_Weight_Chp10 <- quanteda::dfm_weight(CORP_DFM_Chp10)
CORP_Weight_Chp11 <- quanteda::dfm_weight(CORP_DFM_Chp11)
CORP_Weight_Chp12 <- quanteda::dfm_weight(CORP_DFM_Chp12)
CORP_Weight_Chp13 <- quanteda::dfm_weight(CORP_DFM_Chp13)
CORP_Weight_Chp14 <- quanteda::dfm_weight(CORP_DFM_Chp14)
CORP_Weight_Chp15 <- quanteda::dfm_weight(CORP_DFM_Chp15)
str(CORP_Weight_Chp3)
CORP_Weight_Chp3

#computing tf-idf scores
CORP_TFIDF_Chp1 <- quanteda::dfm_tfidf(CORP_DFM_Chp1,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp2 <- quanteda::dfm_tfidf(CORP_DFM_Chp2,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp3 <- quanteda::dfm_tfidf(CORP_DFM_Chp3,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp4 <- quanteda::dfm_tfidf(CORP_DFM_Chp4,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp5 <- quanteda::dfm_tfidf(CORP_DFM_Chp5,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp6 <- quanteda::dfm_tfidf(CORP_DFM_Chp6,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp7 <- quanteda::dfm_tfidf(CORP_DFM_Chp7,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp8 <- quanteda::dfm_tfidf(CORP_DFM_Chp8,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp9 <- quanteda::dfm_tfidf(CORP_DFM_Chp9,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp10 <- quanteda::dfm_tfidf(CORP_DFM_Chp10,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp11 <- quanteda::dfm_tfidf(CORP_DFM_Chp11,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp12 <- quanteda::dfm_tfidf(CORP_DFM_Chp12,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp13 <- quanteda::dfm_tfidf(CORP_DFM_Chp13,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp14 <- quanteda::dfm_tfidf(CORP_DFM_Chp14,scheme_tf = "count", scheme_df="inverse")
CORP_TFIDF_Chp15 <- quanteda::dfm_tfidf(CORP_DFM_Chp15,scheme_tf = "count", scheme_df="inverse")
str(CORP_TFIDF_Chp3)
CORP_TFIDF_Chp3

install.packages('syuzhet')
library('syuzhet')

CORP_df <- as.data.frame(CORP_text$content)
CORP_df

#read the text document as a string
CORP_asString <- get_text_as_string("C:/Users/anith/OneDrive/Desktop/GW/sem 2/Big Daata/project 3/TarzanOfTheApes.txt")
CORP_asString

#getting sentences from the string
CORP_sentences <- get_sentences(CORP_asString)
CORP_sentences[1:50]
str(CORP_sentences)

#analyzing the sentiment using syuzhet method
CORP_syuzhet <- get_sentiment(CORP_sentences,"syuzhet")
CORP_syuzhet

#analyzing the sentiment using nrc method
CORP_nrc <- get_sentiment(CORP_sentences,"nrc")
CORP_nrc

#analyzing the sentiment using bing method
CORP_bing <- get_sentiment(CORP_sentences,"bing")
CORP_bing

#analyzing the sentiment using afinn method
CORP_afinn <- get_sentiment(CORP_sentences,"afinn")
CORP_afinn

#sentiment dictionary for syuzhet method
CORP_Dictionary_syuzhet <- get_sentiment_dictionary()
CORP_Dictionary_syuzhet

#sentiment dictionary for nrc method
CORP_Dictionary_nrc <- get_sentiment_dictionary("nrc")
CORP_Dictionary_nrc

#sentiment dictionary for bing method
CORP_Dictionary_bing <- get_sentiment_dictionary("bing")
CORP_Dictionary_bing

#sentiment dictionary for afinn method
CORP_Dictionary_afinn <- get_sentiment_dictionary("afinn")
CORP_Dictionary_afinn

#sum of sentiment score for syuzhet method
CORP_Sum_syuzhet <- sum(CORP_syuzhet)
CORP_Sum_syuzhet

#sum of sentiment score for nrc method
CORP_Sum_nrc <- sum(CORP_nrc)
CORP_Sum_nrc

#sum of sentiment score for bing method
CORP_Sum_bing <- sum(CORP_bing)
CORP_Sum_bing

#sum of sentiment score for afinn method
CORP_Sum_afinn <- sum(CORP_afinn)
CORP_Sum_afinn

#mean of sentiment score for syuzhet method
CORP_Mean_syuzhet <- mean(CORP_syuzhet)
CORP_Mean_syuzhet

#mean of sentiment score for nrc method
CORP_Mean_nrc <- mean(CORP_nrc)
CORP_Mean_nrc

#mean of sentiment score for bing method
CORP_Mean_bing <- mean(CORP_bing)
CORP_Mean_bing

#mean of sentiment score for afinn method
CORP_Mean_afinn <- mean(CORP_afinn)
CORP_Mean_afinn

#summaries of the sentiments generated
summary(CORP_syuzhet)
summary(CORP_nrc)
summary(CORP_bing)
summary(CORP_afinn)

#plotting the sentiments
plot(CORP_syuzhet, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(CORP_nrc, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(CORP_bing, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(CORP_afinn, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")

#plotting the distribution of emotional valence, grouped by bins
CORP_SentimentPctValue <- get_percentage_values(CORP_syuzhet , bins=10)
structure(CORP_SentimentPctValue)
plot(CORP_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="red")

CORP_SentimentPctValue <- get_percentage_values(CORP_syuzhet , bins=20)
structure(CORP_SentimentPctValue)
plot(CORP_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="green")

CORP_SentimentPctValue <- get_percentage_values(CORP_syuzhet , bins=30)
structure(CORP_SentimentPctValue)
plot(CORP_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="blue")
