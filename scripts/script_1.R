#w³aczanie bibliotek
library(tm)
library(hunspell)
library(stringr)

#zmiana katalogu roboczego
workDir <- "D:\\km193873\\pjn"
setwd(workDir)

#definicja katalogu projektu
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzenie katalogu wyjsciowego
dir.create(outputDir, showWarnings = TRUE)
dir.create(workspaceDir, showWarnings = TRUE)

#utworzenie korpusu dokumentów
corpusDir <- paste(
  inputDir, 
  "\\",
  "Literatura - streszczenia - orygina³",
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

#wstêpne przetwarzanie
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

remove_char <- content_transformer(
  function(x, pattern, replacement)
  gsub(pattern, replacement, x)
)

#usuniêcie "em dash" i 3/4 znaków
corpus <- tm_map(corpus, remove_char, intToUtf8(8722), "")
corpus <- tm_map(corpus, remove_char, intToUtf8(190), "")

#lematyzacha - sprowadzanie do formy podstawowej
polish <- dictionary(lang = "pl_PL")

lemmatize <- function(text) {
  simple_text <- str_trim(as.character(text[1]))
  parsed_text <- strsplit(simple_text, split = " ")
  new_text_vec <- hunspell_stem(parsed_text[[1]], dict = polish)
  for (i in 1:length(new_text_vec)){
    if (length(new_text_vec[[i]]) == 0) new_text_vec[i] <- parsed_text[[1]][i]
    if (length(new_text_vec[[i]]) > 1) new_text_vec[i] <- new_text_vec[[i]][1]
  }
  new_text <- paste(new_text_vec, collapse = " ")
  return(new_text)
}

corpus <- tm_map(corpus, content_transformer(lemmatize))

#usuniêcie rozszerzeñ z nazw dokumentów
cut_extension <- function(document) {
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cut_extension)

#eksport korpusu przetowrzonego do plików tesktowych
preprocessed_dir <- paste(
  inputDir, 
  "\\",
  "Literatura - streszczenia - przetworzone",
  sep = ""
)
dir.create(preprocessed_dir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessed_dir)


wirteLines(as.character(corpus[[1]]))
