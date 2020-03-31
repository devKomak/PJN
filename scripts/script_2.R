#w?aczanie bibliotek
library(tm)
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
  "Literatura - streszczenia - przetworzone",
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

#usuni?cie rozszerze? z nazw dokument?w
cutExtension <- function(document) {
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cutExtensions);

#utworzenie macierzy czestosci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus,
  control = list(
    bounds = list(
      global = c(2, 16) #testowac rozne wartosci tego bounds dla swojego korpusu
    )
  )
)
tdmTfidfBounds<- TermDocumentMatrix(
  corpus,
  control = list(   
    weighting = weightBin,
    bounds = list(
      global = c(2, 16)
    )
  )
)

#konwersja macierzy rzadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)

#eksport macierzy do pliku .csv
#matrixFile <- paste(
#  outputDir, 
#  "\\",
#  "tdmTfidfBounds.csv",
#  sep = ""
#)
#write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

#skalowanie wielowymiarowe
d <- dist(dtmTfidfBoundsMAtrix)
fit <- cmdscale(d,eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(dtmTfidfBoundsMAtrix), cex=.7)