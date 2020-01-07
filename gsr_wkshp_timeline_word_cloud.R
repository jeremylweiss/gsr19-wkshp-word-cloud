

#  This code produces a word-cloud figure based on participant
#  comments recorded on growing season timelines during activities
#  of the Verde Valley and Southeastern Arizona 'Growing Season in
#  Review' workshops for Arizona winegrape growers in October 
#  2019, as used in the workshop synthesis report:
#  <URL>

#  Code adapted from:
#  http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

#  Author:
#  Jeremy Weiss, Climate and Geospatial Extension Scientist
#  School of Natural Resources and the Environment
#  University of Arizona
#  520-626-8063, jlweiss@email.arizona.edu


#####  SETUP


#  Load the needed packages.
library( tm )  #  for text mining
library( RColorBrewer )  #  color palettes
#library( SnowballC )  #  for text stemming
library( wordcloud )  #  word-cloud generator


#####  IMPORT TEXT DATA

  
#  Import the text file containing entries from the growing season
#  timeline.
text <- readLines( "gsr_wkshp_timeline_text.txt" )


#####  TRANSFORM THE TEXT


#  Transform the text data into a corpus, which is a list of a 
#  document. The 'Corpus()' function is from the text mining 
#  ( tm ) package. The 'VectorSource()' function creates a corpus 
#  of character vectors. In this case, each workshop participant
#  entry on the growing season timeline is an element in the 
#  vector and the vector length is the total number of entries.
#  
#  Note: See: https://stackoverflow.com/questions/51942767/r-tm-error-of-transformation-drops-documents
text_corpus <- VCorpus( VectorSource( text ) )

#  Inspect the content of the document.
#
#  Note: See: https://stackoverflow.com/questions/30435054/how-to-show-corpus-text-in-r-tm-package
#inspect( text_corpus )

#  Create a content transformer function that we will use to
#  substitute a space for special characters in the text.
toSpace <- content_transformer( function( x, pattern ) gsub( pattern, " ", x ) )

#  Replace special characters in the text with a 'space' using the
#  'tm_map()' function, which applies transformation functions, or
#  mappings, to corpora.
text_corpus <- tm_map( text_corpus, toSpace, "\\(" )
text_corpus <- tm_map( text_corpus, toSpace, ")" )
text_corpus <- tm_map( text_corpus, toSpace, "-" )
text_corpus <- tm_map( text_corpus, toSpace, "/" )
text_corpus <- tm_map( text_corpus, toSpace, "~" )
text_corpus <- tm_map( text_corpus, toSpace, "=" )


##  CLEAN THE TEXT


#  Convert all text to lower-case characters.
text_corpus <- tm_map( text_corpus, content_transformer( tolower ) )

#  Remove numbers
#text_corpus <- tm_map( text_corpus, removeNumbers )

#  Remove common stopwords in English like 'the' and 'this' from 
#  the text.
text_corpus <- tm_map( text_corpus, removeWords, stopwords( kind = "english" ) )

#  Specify and remove your own stop words from the text.
#text_corpus <- tm_map( text_corpus, removeWords, c( "blabla1", "blabla2" ) ) 

#  Remove punctuation marks from the text.
text_corpus <- tm_map( text_corpus, removePunctuation )

#  Eliminate extra white spaces from, or collaspe multiple 
#  whitespace in, the text.
text_corpus <- tm_map( text_corpus, stripWhitespace )


##  BUILD WORD FREQUENCY TABLE


#  Construct a table representing term-document occurrences. 
tdm <- TermDocumentMatrix( text_corpus )

#  Convert this table into a matrix in which rows are words, or
#  'Terms', and columns are individual participant entries, or
#  'Documents'.
m <- as.matrix( tdm )

#  Sort this matrix from high to low word frequency, based on the
#  total number of word, or 'Term', occurences across all 
#  participant entries, or 'Documents'. Output is a vector with
#  words as names and total occurrences as elements.
v <- sort( rowSums( m ), decreasing = TRUE )

#  Convert the sorted word-occurrence sums into a dataframe.
d <- data.frame( word = names( v ), freq = v )


##  GENERATE THE WORD CLOUD


#  Regarding the stemmed version...the 'stemDocument' function
#  leads to strange results, like 'earli' for 'early' and 
#  'earlier'. Thus, it is not used in this case.

#  Set up the destination file for the word cloud.
png( filename = "gsr-wkshp-timeline-word-cloud.png",
     width = 6,
     height = 6,
     units = "in",
     pointsize = 12,
     bg = "white",
     res = 600,
     type = c( "windows", "cairo", "cairo-png" ),
     antialias = "cleartype" )

wordcloud( words = d$word,
           freq = d$freq,
           scale = c( 4, 0.4 ),
           min.freq = 2,
           max.words = 250,
           random.order = FALSE,
           rot.per = 0.25,
           colors = brewer.pal( 8, "Dark2" ) )

dev.off()


#####

