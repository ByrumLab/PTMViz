readScaffold = function(fileName){
  
  # modified split function addressing lines ending with "\t" (strsplit will remove last missing entry)
  my.split = function(x){
    ch = substr(x,nchar(x),nchar(x))
    if(ch == "\t"){
      return(c(strsplit(x,split='\t')[[1]],""))
    } else{
      return(strsplit(x,split='\t')[[1]])
    }
  }
  
  # read file line by line
  myConnection = file(fileName,open="r")
  myLines = readLines(myConnection)
  close(myConnection)

  # finding true beginning of file
  i = 1
  while(myLines[i] != "") i = i + 1
  myHeader = unlist(strsplit(myLines[i + 1], "\t"))
  startOfData = i = i + 2
  
  # finding end of file
  while(myLines[i] != "END OF FILE") i = i + 1
  endOfFile = i - 1
  
  # split each line along at tabs
  dataLines = myLines[startOfData:endOfFile]
  dataMatrix = t(sapply(dataLines,FUN=my.split))
  dataMatrix[dataMatrix == ""] = NA
  colnames(dataMatrix) = myHeader
  
  return(data.frame(dataMatrix, check.names = F))
}