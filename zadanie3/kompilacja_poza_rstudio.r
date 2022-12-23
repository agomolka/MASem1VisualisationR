rm(list=ls())
library(knitr)
#library(pander)
library(markdown)
library(rmarkdown)

## wygenerowanie pliku *.md
knit(input="Raport.Rmd", output="tmp.md") 

## Konwersja na zwykly dokument html
markdownToHTML(file="tmp.md", output="Raport.html", stylesheet='markdown.css')

## Konwersja na dokument html - slajdy
#system("pandoc -t slidy -s tmp.md -o Raport_slajd.html")

## Konwersja na dokument pdf
#system("pandoc tmp.md -o Raport.pdf")

unlink("tmp.md")
