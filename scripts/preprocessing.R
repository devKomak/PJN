#w?aczanie bibliotek
library(tm)
library(hunspell)
library(stringr)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\Konrad\\Desktop\\PJN\\TextMining"
setwd(workDir)

#definicja katalogu projektu
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzenie katalogu wyjsciowego
dir.create(outputDir, showWarnings = TRUE)
dir.create(workspaceDir, showWarnings = TRUE)

#utworzenie korpusu dokument?w
corpusDir <- paste(
  inputDir, 
  "\\",
  "Literatura - streszczenia - oryginał",
  sep = ""
  )
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
    ),
  readerControl = list(
    language="pl_PL"
    )
)

#usuniecie z tekstow podzialu na akapity
pasteParagraphs <- content_transformer(function(text, char) paste(text, collapse = char))
corpus <- tm_map(corpus, " ")

#wst?pne przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplistFile <- paste(
  inputDir, 
  "\\",
  "stopwords_pl.txt",
  sep = ""
)
stoplist <- readLines(
  stoplistFile,
  encoding = "UTF-8"
)
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

removeChar <- content_transformer(
  function(x, pattern, replacement)
  gsub(pattern, replacement, x)
)

#usuni?cie "em dash" i 3/4 znak?w
corpus <- tm_map(corpus, removeChar, intToUtf8(8722), "")
corpus <- tm_map(corpus, removeChar, intToUtf8(190), "")

#lematyzacha - sprowadzanie do formy podstawowej
polish <- dictionary(lang = "pl_PL")

lemmatize <- function(text) {
  simpleText <- str_trim(as.character(text[1])) 
  parsedText <- strsplit(simpleText, split = " ")
  newTtextVec <- hunspell_stem(parsedText[[1]], dict = polish)
  for (i in 1:length(newTtextVec)){
    if (length(newTtextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
    if (length(newTtextVec[[i]]) > 1) newTtextVec[i] <- newTextVec[[i]][1]
  }
  new_text <- paste(new_text_vec, collapse = " ")
  return(new_text)
}

corpus <- tm_map(corpus, content_transformer(lemmatize))

#usuni?cie rozszerze? z nazw dokument?w
cutExtensions <- function(document) {
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cutExtensions)

#eksport korpusu przetowrzonego do plik?w tesktowych
preprocessedDir <- paste(
  inputDir, 
  "\\",
  "Literatura - streszczenia - przetworzone",
  sep = ""
)
dir.create(preprocessedDir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessedDir)


wirteLines(as.character(corpus[[1]]))
