# Deploying-an-online-text-prediction-algorithm
A web based text predction algorithm using Shiny

Contained is this repository is R code to deploy a Shiny app that allows the user to enter a string and the app will predict the next likliest word in the string.
The Shiny app is deployed on the Shiny Server provided by R Studios. This project demonstates understanding of natural language processing,
smoothing associated with natural language processing (such as Katz back-off models) and implementing online data products, tthat utilize
user interfaces and servers for computation. It also demonstartes proficiency in natural language processing packages in R such as tm, ngram and NLP.

The app can be used at the following link: [https://rojay16.shinyapps.io/text_pred3/](https://rojay16.shinyapps.io/text_pred3/)

The word predcition algorithm used in this app utilizes n-gram modelling.

In n-gram modelling the next word is predcted based on the previous n-1 terms. In our app tri-gram modelling is used 
to predict the next word. This means given a sentence the last two words are used to predict the next word. 

In order to use tri-gram modelling you must have already have a database of three word sequences (tri-grams). 
In our app the database of tri-grams in generated from the Twitter corpus (a file containing 2.3 million tweets). 
We only used 5% of the Twitter corupus to decrease the size of app and to reduce computation time. The R package tm allows us the generate all tri-grams within the Twitter corpus (all three word squences in the in the corpus). Hence the twitter corpus to our algorithm is considered representative of the English language.

When the tm package finds the tri-grams from the corpus it also gives the frequency of each tri-gram in the corpus. Given the last two words of the sentence the user enters we can form a list of possible tri-grams containing the last
two words of the sentence (from the database of tri-grams created by tm). We then select the highest frequencey tri-gram from the list. The final word of the selected tri-gram is the predicted word our algorithms gives.

The app allows you the filter stopwords from the corpus used to generate the tri-grams. Stopwords are words that generally don't contribute to the actual content of the corpus such as "the", "I" "my" ect. Thus when stopwords are not removed the next predicted word can often be "a" or "the". Removing stopwords allows the user to make sure the next word is related to the content and context of the sentence. This app provides the 5 most likely next words ( the 5 most frequent tri-grams given the final two words of the entered sentence.)

If the last two words of the sentence are not found in the corpus the algorithm uses (the sequence of the last two words is an unseen tri-gram), we then just take the last word of the setence and find all the bi-grams containing the last word in the corpus. We then select the bi-gram with the highest frequency, and the last word in that bi-gram is predicted as the next likliest word. Similarly if there are no bi-grams containing the final word of the sentence, the next likeliest word is simply the most common word in the corpus. This is a similar to the technique used to account for unseen n-grams know as Katz Backoff.
