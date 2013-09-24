---
layout: post
title : "The SQAwR Package!"
categories: [update]
comments: true
---

I've just uploaded my first R package to [CRAN](http://cran.r-project.org/). It's not available yet, 
but if you'd like to check it out yourself, you can install it from github, if you've installed `devtools`.

    library(devtools)
    install_github("SQAwR", username = "JoFrhwld", branch = "master")
    library(SQAwR)
    help(package="SQAwR")

You'll see that there's not much to it, just the data sets that I'll be referencing in
the texbook I'm writing called "Introduction to Quantitative Analysis with R."