library(knitr)
setwd("~/Documents/Classes/Dissertation/Defense/")


knit("index.Rmd")
system("pandoc -s --mathjax -H css_and_js.html -t dzslides  index.md -o index.html")

