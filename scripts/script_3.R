#wlaczenie bibliotek

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir, 
  "\\",
  "script_2.R",
  sep = ""
)
source(sourceFile)