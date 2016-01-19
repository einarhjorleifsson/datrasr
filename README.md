# a datras package and course
Einar Hjörleifsson  
18 January 2016  

### Preamble

Note: Only version 7 available as non-compiled directory structure

### Downloading

Following was run prior to the git seeding:


```r
tfil <- tempfile()
download.file("https://datras.googlecode.com/files/datrasR_7.0.tar.gz", tfil)
untar(tfil)
# remove big objects
system("rm datrasR/data/balticBathy.RData")
system("rm datrasR/data/grandbanksBathy.RData")
system("rm datrasR/data/northBathy.RData")
system("rm datrasR/data/nseaBathy.RData")
system("rm datrasR/data/nwestBathy.RData")
system("rm datrasR/data/southBathy.RData")
system("rm datrasR/data/swestBathy.RData")
system("mv datrasR/* .")
system("rmdir datrasR")

system("mkdir inst")
system("mkdir inst/practical")
download.file("https://datras.googlecode.com/files/Practical01.r", "inst/practical/Practical01.R")
download.file("https://datras.googlecode.com/files/Practical02.r", "inst/practical/Practical02.R")
download.file("https://datras.googlecode.com/files/Practical03.r", "inst/practical/Practical03.R")
download.file("https://datras.googlecode.com/files/Practical04.r", "inst/practical/Practical04.R")
download.file("https://datras.googlecode.com/files/Practical05.r", "inst/practical/Practical05.R")
```

