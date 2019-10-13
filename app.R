
library(shiny)
library(stringr)
library(ggplot2)
library(tm)
library(NLP)
library(ngram)

#Generating file ids for each corpus in R
fid_tw<-file("final\\en_US\\en_US.twitter.txt",open="r")
fid_bl<-file("final\\en_US\\en_US.blogs.txt",open="r")
fid_ne<-file("final\\en_US\\en_US.news.txt",open="r")

#Reading the entire twitter corpus into R as a charcater vector
tw_corpus<-readLines(fid_tw)

# Create a vector that only has 5% of the corpus, to reduce computation times, this will be the corpus we use for text
# prediction
set.seed(1345)
tw_corpus_rand<-tw_corpus[sample(length(tw_corpus),0.05*length(tw_corpus))]
# convert eachs string to lower case
tw_corpus_rand<-tolower(tw_corpus_rand)


#Convert vector to a format that the natural language processing package "tm" can use
tw_tm<-VCorpus(VectorSource(tw_corpus_rand))


#tw_tm_filt<-tm_map(tw_tm,removeWords,stopwords(kind="en"))
#Remove punctuation and extra whitespace from corpus
tw_tm_filt<-tm_map(tw_tm,removePunctuation)
tw_tm_filt<-tm_map(tw_tm_filt,stripWhitespace)

#Create another corpus that also has stopwords removed
tw_tm_sw<-tm_map(tw_tm_filt,removeWords,stopwords(kind="en"))

#Create a term document matrix (this matrix has gives the freqeuncey of each term in the corpus for each document (each
# tweet is a considered a document) for both corpuses
dtm_tw<-TermDocumentMatrix(tw_tm_filt)
dtm_tw_sw<-TermDocumentMatrix(tw_tm_sw)


#This creates a function that toeknizes each document into bigrams, this function is then used 
# by termdocumentmatrix to create a matrix that gives the frequency of each bigram in each document
#Analyzing bigrams
BigramTokenizer <-
  function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}

# Create a term document matrix for bigrams
dtm_tw2 <- TermDocumentMatrix(tw_tm_filt, control = list(tokenize = BigramTokenizer))
dtm_tw2_sw <- TermDocumentMatrix(tw_tm_sw, control = list(tokenize = BigramTokenizer))


TrigramTokenizer <-
  function(x){
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}

dtm_tw3 <- TermDocumentMatrix(tw_tm_filt, control = list(tokenize = TrigramTokenizer))
dtm_tw3_sw <- TermDocumentMatrix(tw_tm_sw, control = list(tokenize = TrigramTokenizer))






#As creating term document matrices can be computationally expensive, we have loaded, unigram, bigrams and trigram
# term document matries as RDS files, so they can be used immediately. We load both the term documents with and without stop words

dtm_tw<-readRDS("dtm_tw_sm.rds")
dtm_tw_sw<-readRDS("dtm_tw_sm_sw.rds")

#List each the term in the corpus and how many times they appear in the corpus (for all the tweets)
mft_tw<-findMostFreqTerms(dtm_tw,n=dim(dtm_tw)[1],INDEX=rep(1,each=dim(dtm_tw)[2]))
mft_tw_sw<-findMostFreqTerms(dtm_tw_sw,n=dim(dtm_tw_sw)[1],INDEX=rep(1,each=dim(dtm_tw_sw)[2]))

dtm_tw2 <- readRDS("dtm_tw2_sm.rds")
dtm_tw2_sw <- readRDS("dtm_tw2_sm_sw.rds")

#List each the bigram in the corpus and how many times they appear in the corpus (for all the tweets)
mft_tw2<-findMostFreqTerms(dtm_tw2,n=dim(dtm_tw2)[1],INDEX=rep(1,each=118007))
mft_tw2_sw<-findMostFreqTerms(dtm_tw2_sw,n=dim(dtm_tw2_sw)[1],INDEX=rep(1,each=118007))



#Determine Katz counts
#Katz backoff allows us to account for unseen n-grams , by shifting the probability mass from count i+1 to count i,
# n- grams that have high counts(>5) are considered accurate and are left unchanged

kcount<-c()

for(i in 1:5){
  rstar<-((i+1)*length(mft_tw2[[1]][mft_tw2[[1]]==(i+1)]))/(length(mft_tw2[[1]][mft_tw2[[1]]==i]))
  print(rstar)
  nki<-6*(length(mft_tw2[[1]][mft_tw2[[1]]==(6)]))/(length(mft_tw2[[1]][mft_tw2[[1]]==1]))
  dr<-((rstar/i)-nki)/(1-nki)
  print(dr)
  kcount<-c(kcount,dr*i)
}


kcount_sw<-c()

for(i in 1:5){
  rstar<-((i+1)*length(mft_tw2_sw[[1]][mft_tw2_sw[[1]]==(i+1)]))/(length(mft_tw2_sw[[1]][mft_tw2_sw[[1]]==i]))
  print(rstar)
  nki<-6*(length(mft_tw2_sw[[1]][mft_tw2_sw[[1]]==(6)]))/(length(mft_tw2_sw[[1]][mft_tw2_sw[[1]]==1]))
  dr<-((rstar/i)-nki)/(1-nki)
  print(dr)
  kcount_sw<-c(kcount_sw,dr*i)
}

#Update the previous frequencies to the katz freqencies 

mft_tw_katz<- mft_tw2[[1]]

mft_tw_katz_sw<-mft_tw2_sw[[1]]

for(i in 1:5){
  mft_tw_katz[(1:length(mft_tw_katz))[mft_tw_katz==i]]<-kcount[i]
}

for(i in 1:5){
  mft_tw_katz_sw[(1:length(mft_tw_katz_sw))[mft_tw_katz_sw==i]]<-kcount_sw[i]
}



dtm_tw3 <-readRDS("dtm_tw3_sm.rds")
dtm_tw3_sw <-readRDS("dtm_tw3_sm_sw.rds")

#List each the trigram in the corpus and how many times they appear in the corpus (for all the tweets)
mft_tw3<-findMostFreqTerms(dtm_tw3,n=dim(dtm_tw3)[1],INDEX=rep(1,each=118007))   
mft_tw3_sw<-findMostFreqTerms(dtm_tw3_sw,n=dim(dtm_tw3_sw)[1],INDEX=rep(1,each=118007))

kcount3<-c()

for(i in 1:5){
  rstar<-((i+1)*length(mft_tw3[[1]][mft_tw3[[1]]==(i+1)]))/(length(mft_tw3[[1]][mft_tw3[[1]]==i]))
  print(rstar)
  nki<-6*(length(mft_tw3[[1]][mft_tw3[[1]]==(6)]))/(length(mft_tw3[[1]][mft_tw3[[1]]==1]))
  dr<-((rstar/i)-nki)/(1-nki)
  print(dr)
  kcount3<-c(kcount3,dr*i)
}

kcount3_sw<-c()

for(i in 1:5){
  rstar<-((i+1)*length(mft_tw3_sw[[1]][mft_tw3_sw[[1]]==(i+1)]))/(length(mft_tw3_sw[[1]][mft_tw3_sw[[1]]==i]))
  print(rstar)
  nki<-6*(length(mft_tw3_sw[[1]][mft_tw3_sw[[1]]==(6)]))/(length(mft_tw3_sw[[1]][mft_tw3_sw[[1]]==1]))
  dr<-((rstar/i)-nki)/(1-nki)
  print(dr)
  kcount3_sw<-c(kcount3_sw,dr*i)
}



mft_tw_katz3<- mft_tw3[[1]]

for(i in 1:5){
  mft_tw_katz3[(1:length(mft_tw_katz3))[mft_tw_katz3==i]]<-kcount3[i]
}  

mft_tw_katz3_sw<- mft_tw3_sw[[1]]

for(i in 1:5){
  mft_tw_katz3_sw[(1:length(mft_tw_katz3_sw))[mft_tw_katz3_sw==i]]<-kcount3_sw[i]
}  

#The algorithm predict the next word based on the previous two words based the most likely occuring trigram. If no trigram
# exists witht he previous two words we backoff to the bigram, and to the unigram if nesscary. The count mass shifted from
# trigram to bigram and bigram to unigram are based off the Katz counts. This algorithm provides the prediction with and without
#stopwords

tri_pred_nw<-function(input_string){
  
# Take the input string and convert to lowercase and a tm corpus object
  ips<-input_string
  ips<-tolower(ips)
  ips_tm<-VCorpus(VectorSource(ips))
  
  ips_tm_filt<-tm_map(ips_tm,removePunctuation)
  ips_tm_filt<-tm_map(ips_tm_filt,stripWhitespace)

#Tokenize the string, so we can extract the last two words
  
  ips_chr<-as.character(ips_tm_filt[[1]])
  ips_tok<-words(ips_chr)
  

  
  # Get the counts of all trigrams containing the last two words of the string
  ctri<-mft_tw_katz3[grep(paste("^",paste(ips_tok[length(ips_tok)-1],ips_tok[length(ips_tok)]),sep=""),names(mft_tw3[[1]]),value = F)]
  
  # Get the counts of all bigrams containing the word of the string
  ck<-mft_tw_katz[grep(paste("^",ips_tok[length(ips_tok)],sep=""),names(mft_tw2[[1]]))]
  
  # If the bigrams and trigrams of the string are unseen, the algorithm justs simply predicts the most common word 
  # of the corpus
  cw<-mft_tw[[1]]
  
  #Return the 5 most probable (highest counts) trigrams and their final words ( the final words are our predctions)
  tri<-sort(ctri,decreasing=T)[1:5]
  tri_name<-names(tri)
  tri_wds<-words(tri_name)
  tri_ret<-tri_wds[seq(3,15,3)]
  
  #Return the 5 most probable (highest counts) bigrams and their final words ( the final words are our predctions)
  bi<-sort(ck,decreasing=T)[1:5]
  bi_name<-names(bi)
  bi_wds<-words(bi_name)
  bi_ret<-bi_wds[seq(2,10,2)]
  
  #Return the 5 most probable (highest counts) unigrams and their final words ( the final words are our predctions)
  m_ret<-names(sort(cw,decreasing=T)[1:5])
  
  
#If the list of trigrams (top 5 likiliest trigrams) is empty then return the list of bigrams, if that list is empty 
  # return the 5 most common words
  if(length(ctri)!=0){
    
    return (tri_ret)
  }else if(length(ck)!=5){
    return(bi_ret)} else{
      return(m_ret)}}

#Same algorithm but stopwords are removed
tri_pred_nw_sw<-function(input_string){
  ips<-input_string
  ips<-tolower(ips)
  ips_tm<-VCorpus(VectorSource(ips))
  
  
  ips_tm_filt<-tm_map(ips_tm,removePunctuation)
  ips_tm_filt<-tm_map(ips_tm_filt,stripWhitespace)
  ips_tm_filt<-tm_map(ips_tm_filt,removeWords,stopwords(kind="en"))
  
  ips_chr<-as.character(ips_tm_filt[[1]])
  ips_tok<-words(ips_chr)
  
  #ig3<-paste(ips_tok[length(ips_tok)-1],ips_tok[length(ips_tok)])
  
  #tind<- words(big3) %in% names(mft_tw[[1]])
  
  #if (sum(tind)!=length(tind)){
  # for (i in length(tind)){
  #  if(tind[i]==F){
  #   print(paste(words(big3)[i], "is not in the dictonary used, please use a synonym"))
  #}}
  #return()}
  # else{
  
  ctri<-mft_tw_katz3_sw[grep(paste("^",paste(ips_tok[length(ips_tok)-1],ips_tok[length(ips_tok)]),sep=""),names(mft_tw3_sw[[1]]),value = F)]
  
  
  ck<-mft_tw_katz_sw[grep(paste("^",ips_tok[length(ips_tok)],sep=""),names(mft_tw2_sw[[1]]))]
  
  
  cw<-mft_tw_sw[[1]]
  
  
  tri<-sort(ctri,decreasing=T)[1:5]
  tri_name<-names(tri)
  tri_wds<-words(tri_name)
  tri_ret<-tri_wds[seq(3,15,3)]
  
  bi<-sort(ck,decreasing=T)[1:5]
  bi_name<-names(bi)
  bi_wds<-words(bi_name)
  bi_ret<-bi_wds[seq(2,10,2)]
  
  m_ret<-names(sort(cw,decreasing=T)[1:5])
  
  if(length(ctri)!=0){
    
    return (tri_ret)
  }else if(length(ck)!=0){
    return(bi_ret)} else{
      return(m_ret)}}

#The Shiny interface the user interacts with, here they eneter the string they want to predict the next word for
# and the 5 likliest words are displayed
ui<-fluidPage(
  titlePanel("Next word prediction algoirthm"),
  mainPanel(
    
    h3("This app gives the 5 most likley next words given the sentence you entered. If the remove stopwords box is ticked, the corpus 
      which the algorithm uses to predict the next word will not include stopwords"),
    #Where the user enters the string
    textInput("sentence","Enter setence",NULL),
    #User clicks predict
    actionButton("predict", "Predict"),
    #toggle with and without stopwords
    checkboxInput("Remove_stopwords", "Remove Stopwords", value = FALSE),
    h3("5 likliest next words (is descending order):"),
    #Return list of predictions
    conditionalPanel("input.Remove_stopwords == 0", h3(textOutput("textnw1")),h3(textOutput("textnw2")),h3(textOutput("textnw3"))
                     ,h3(textOutput("textnw4")),h3(textOutput("textnw5"))),
    conditionalPanel("input.Remove_stopwords == 1", h3(textOutput("textsw1")),h3(textOutput("textsw2")),h3(textOutput("textsw3"))
                     ,h3(textOutput("textsw4")),h3(textOutput("textsw5")))
    
  
  )
)

#Shiny server to perform prediction calculations
server<-function(input, output){
# Reactive variables, that are only calculated when predict button is pressed, then prediction function 
#is called
  nwr<-eventReactive(input$predict,tri_pred_nw(input$sentence))
  swr<-eventReactive(input$predict,tri_pred_nw_sw(input$sentence))

#Assign results of predction function call to outputs which will be displayed in shiny user interface    
output$textnw1<-renderText({if(input$Remove_stopwords==0){nwr()[1]}else{""}})
output$textnw2<-renderText({if(input$Remove_stopwords==0){nwr()[2]}else{""}})
output$textnw3<-renderText({if(input$Remove_stopwords==0){nwr()[3]}else{""}})
output$textnw4<-renderText({if(input$Remove_stopwords==0){nwr()[4]}else{""}})
output$textnw5<-renderText({if(input$Remove_stopwords==0){nwr()[5]}else{""}})
  
output$textsw1<-renderText({if(input$Remove_stopwords==1){swr()[1]}else{""}})
output$textsw2<-renderText({if(input$Remove_stopwords==1){swr()[2]}else{""}})
output$textsw3<-renderText({if(input$Remove_stopwords==1){swr()[3]}else{""}})
output$textsw4<-renderText({if(input$Remove_stopwords==1){swr()[4]}else{""}})
output$textsw5<-renderText({if(input$Remove_stopwords==1){swr()[5]}else{""}})
  

 
  

}


shinyApp(ui = ui, server = server)