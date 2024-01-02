#install.packages("tidyverse")
library("tidyverse")

### Read in files from current directory
### if they are there, dftable and dfresponse are made.
###
filelist <- list.files()
dfresponse <- NULL
dftable <- NULL
responsefound <- FALSE
tablefound <- FALSE
for (i in 1:length(filelist)) {
  filename <- filelist[i]
  if (filename == "response.csv") {
    print("response.csv found, loading...")
    dfresponse <- read_csv(filename, skip_empty_rows = TRUE)
    responsefound <- TRUE
  } else if (grepl("Table.csv" , filename)) {
    print(paste("Table file found",filename))
    dftable <- read_csv(filename, skip_empty_rows = TRUE)
    tablefound <- TRUE
  }
}
if (responsefound == TRUE & tablefound == TRUE) {
  print("Files found and loaded response.csv and Table.csv file")
} else {
  print("ERROR: Files not found. Need response.csv and *Table.csv file")
  stop()
}

###
### Filter out stuff from dftable, remove blank rows and 
###   rows with Label with START, END
###
dftable <- dftable %>% filter(!Label=="NA")
dftable <- dftable %>% filter(!str_detect(Label, "^START"))
### need end times to know when to stop the pressure protocols
#dftable <- dftable %>% filter(!str_detect(Label, "^END"))

###
### Create a function so we can call it once before the loop
### and for each time in the loop...
###

addNewColumn <- function(starttime, endtime, newcolnum, labelname) {
  dfresponse[,newcolnum] <<- 0
  names(dfresponse)[newcolnum] <<- labelname
  inx <- which(dfresponse$Time > starttime & dfresponse$Time < endtime)
  mininx <- min(inx)
  maxinx <- max(inx)
  print(paste(starttime,endtime,labelname,mininx, maxinx))
  if (mininx > numrows | maxinx > numrows) {
    print("out of range times...ending early")
    break
  }
  dfresponse[mininx:maxinx,newcolnum] <<- dfresponse[mininx:maxinx,"Outer Diameter"]
}
###
### For each time range in dftable, copy the response Outer Diameter values
### into a new column with name same as Label
###
numranges <- dim(dftable)[1]
numrows <- dim(dfresponse)[1]
starttime <- 0
endtime <- as.numeric(dftable[1,1])
addNewColumn(starttime=0, endtime=endtime, newcolnum=3, labelname="startwash")

starttime <- endtime
newcolnum <- 4
for (i in 1:(numranges)) {
  # add new column, name it with Label name
  labelname <- toString(dftable[i,"Label"])
  endtime <- as.numeric(dftable[i+1,"Time (s)"])
  ### if row i starts with END, then it is an endtime, skip it.
  if (!grepl("END",dftable[i,"Label"])) {
    addNewColumn(starttime=starttime, endtime=endtime, newcolnum=newcolnum, labelname=labelname)
    newcolnum <- newcolnum + 1
  }
  starttime <- endtime
}
print("data processed and will be written to output.csv")
write_csv(dfresponse, "output.csv")