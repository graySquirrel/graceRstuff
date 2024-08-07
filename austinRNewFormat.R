#install.packages("tidyverse")
library("tidyverse")
if (dir.exists("C:\\Users\\harra")) {
  setwd("\\\\AS6704T-1035\\Home\\Myography Data\\austin")
}
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
  if (grepl("^[0-9]{8}_Exp[0-9]{2}.csv",filename)) {
    print(paste("response file found, loading...",filename))
    dfresponse <- read_csv(filename, skip_empty_rows = TRUE)
    responsefound <- TRUE
  } else if (grepl("table.csv" , filename)) {
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
#dftable <- dftable %>% filter(!str_detect(Label, "^START"))
### need end times to know when to stop the pressure protocols
#dftable <- dftable %>% filter(!str_detect(Label, "^END"))

## New format times are in hour:min:sec - convert to seconds
# and put in first column so it isn't cropped.
v <- dftable %>% pull(Time)
v1 <- strptime(v, format='%H:%M:%S')
timesSecond <- v1$hour * 3600 + v1$min * 60 + v1$sec
dftable[1] <- timesSecond
names(dftable)[names(dftable) == '#'] <- 'Time (s)'

# Preprocess dftable to remove bits that I dont want to copy
# Add a column to dftable put StartCount, which counts the number of Starts it's seen
dft1 <- dftable[,c(1,2,3,5)]
dft1["StartCount"] = 0
dft1["EndCount"] = 0
# loop through dftable and increment StartCount every time we see a START Label
StartCount <- 0
EndCount <- 0
numranges <- dim(dft1)[1]
for (i in 1:(numranges)) {
  if (grepl("START",dft1[i,"Label"])) {
    StartCount <- StartCount + 1
  }
  if (grepl("END",dft1[i,"Label"])) {
    EndCount <- EndCount + 1
  }
  dft1[i,"StartCount"] <- StartCount
  dft1[i,"EndCount"] <- EndCount
}
## now we got start and end count.  Start removing rows we dont care about
# remove all stuff before first START
#dft1 <- dft1[dft1$StartCount != 0,]
# remove all stuff when start and end counts are same that is not a Yoda
dft1 <- dft1 %>% filter(!(StartCount > 0 & StartCount == EndCount
                          & !str_detect(Label, "Yoda")
                          & !str_detect(Label, "END")))
dft1 %>% print(n=Inf)

# put it back into dftable
dftable <- dft1
###
### Create a function so we can call it once before the loop
### and for each time in the loop...
###

addNewColumn <- function(starttime1, endtime1, starttime2, endtime2, newcolnum, labelname) {
  if (length(starttime1) == 0) {starttime1 <- starttime2}
  if (length(endtime1) == 0) {endtime1 <- endtime2}
  dfresponse[,newcolnum] <<- NA
  names(dfresponse)[newcolnum] <<- labelname
  print(paste("addnewcolumn", "st1",starttime1,"et1",endtime1,"st2",starttime2, "et2",endtime2, labelname))
  ## Only copy the data if its a real data (mmHg)
  #if (grepl("mmHg",labelname)) {
  #inx1 <- which(dfresponse$Time > starttime1 & dfresponse$Time < endtime1)
  inx1 <- which(dfresponse$'Time (s)' >= starttime1 & dfresponse$'Time (s)' < endtime1)
  mininx1 <- min(inx1)
  maxinx1 <- max(inx1)
  #inx2 <- which(dfresponse$Time > starttime2 & dfresponse$Time < endtime2)
  inx2 <- which(dfresponse$'Time (s)' >= starttime2 & dfresponse$'Time (s)' < endtime2)
  mininx2 <- min(inx2)
  maxinx2 <- max(inx2)
  inxdiff1 <- maxinx1 - mininx1
  inxdiff2 <- maxinx2 - mininx2
  print(paste(starttime1,endtime1,starttime2, endtime2, labelname,mininx1, maxinx1, inxdiff1, inxdiff2))
  if (mininx2 > numrows | maxinx2 > numrows) {
    print("out of range times...ending early")
    break
  }
  dfresponse[mininx1:maxinx1,newcolnum] <<- dfresponse[mininx2:(mininx2+inxdiff1),"Inner Diameter"] 
  dfresponse[which(dfresponse[newcolnum] > 200 | dfresponse[newcolnum] < 1), newcolnum] <<- NA
  #}
}
###
### For each time range in dftable, copy the response Outer Diameter values
### into a new column with name same as Label
###
numranges <- dim(dftable)[1]
numrows <- dim(dfresponse)[1]
starttime <- 0
endtime <- as.numeric(dftable[1,1])
addNewColumn(starttime1=0, endtime1=endtime, starttime2=0, endtime2=endtime, 
             newcolnum=4, labelname="Tone")

#offset <- as.numeric(dftable[1,"Time (s)"])
#starttime <- offset
starttime <- endtime
newcolnum <- 5
#newcolnum <- 3
for (i in 1:(numranges)) {
  print(i)
  # add new column, name it with Label name
  if (!grepl("END",dftable[i,"Label"]) & !grepl("START",dftable[i,"Label"])) {
    labelname <- toString(dftable[i,"Label"])
    endtime <- as.numeric(dftable[i+1,"Time (s)"])
    if (is.na(endtime)) {
      endtime <- as.integer(dfresponse[numrows,"Time (s)"])
    }
    ### if row i starts with END, then it is an endtime, skip it.
    #if (!grepl("END",dftable[i,"Label"]) & !grepl("START",dftable[i,"Label"])) {
    if (dftable[i, "StartCount"] <= 1) {
      addNewColumn(starttime1=starttime, endtime1=endtime, 
                   starttime2=starttime, endtime2=endtime,
                   newcolnum=newcolnum, labelname=labelname)
      newcolnum <- newcolnum + 1
    }
    else {
      firstinx <- which(dftable$StartCount==1 & dftable$Label==labelname)
      starttime1 <- dftable$`Time (s)`[firstinx]
      endtime1 <- dftable$`Time (s)`[firstinx+1]
      addNewColumn(starttime1=starttime1, endtime1=endtime1, 
                   starttime2=starttime, endtime2=endtime,
                   newcolnum=newcolnum, labelname=labelname)
      newcolnum <- newcolnum + 1
    }
    starttime <- endtime
  }
}
#names(dfresponse)[2] <- "Tone"
print("data processed and will be written to output.csv")
write_csv(dfresponse, "output.csv", na = "")