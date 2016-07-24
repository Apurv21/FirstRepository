failedStudents <- function(){
  studentData <- read.csv("StudentMarks.csv");
  
  failedStudentCount <- 0
  totalCount <- length(studentData$RollNo)
  for(i in 1:totalCount){
    if(stud1[i,"Marks"] >= 35 && studentData[i,"Year"] == 1){
      failedStudentCount = failedStudentCount + 1 
    }
  }
  
  print(failedStudentCount)
}


StudentsByClassOptimized <- function(){
  studentData <- read.csv("StudentMarks.csv")
  
  DistinctionCount <- length(subset(studentData,Marks >= 75 & Year == 1)$RollNo)
  FirstClassCount <- length(subset(studentData,Marks < 75 & Marks >=60 & Year == 1)$RollNo)
  SecondClassCount <- length(subset(studentData,Marks < 60 & Marks >=50 & Year == 1)$RollNo)
  PassClassCount <- length(subset(studentData,Marks < 50 & Marks >= 35 & Year == 1)$RollNo)
  FailCount <- length(subset(studentData,Marks < 35 & Year == 1)$RollNo)
  
  result <- list(c("Distinction", DistinctionCount), c("First Class", FirstClassCount), c("Second Class",SecondClassCount), c("Pass Class", PassClassCount),c("Failed Class", FailCount))
  
  print(result)
}

StudentsByClass <- function(){
  studentData <- read.csv("StudentMarks.csv")
  
  distinctionCount <- 0
  firstClassCount <- 0
  secondClassCount <- 0
  passClassCount <- 0
  FailedCount <- 0
  totalCount <- length(studentData$RollNo)
  
  for(i in 1:totalCount){
      tempMarks <- studentData[i,"Marks"]
      year <- studentData[i,"Year"]
      
      if(tempMarks >= 75 && year == 1)
      {
        distinctionCount <- distinctionCount + 1
      }
      else if(tempMarks < 75 && tempMarks >= 60 && year == 1)
      {
        firstClassCount <- firstClassCount + 1
      }
      else if(tempMarks < 60 && tempMarks >= 50 && year == 1)
      {
        secondClassCount <- secondClassCount + 1
      }
      else if(tempMarks < 50 && tempMarks >= 35 && year == 1)
      {
        passClassCount <- passClassCount + 1
      }
      else if(tempMarks < 35 && year == 1)
      {
        FailedCount <- FailedCount + 1
      }
  
  }
  
  result <- list(c("Distinction", distinctionCount), c("First Class", firstClassCount), c("Second Class",secondClassCount), c("Pass Class", passClassCount),c("Failed Class", FailedCount))
  
  print(result)
  
}

PlotStudentGraph <- function(roll_no){
  studentData <- read.csv("StudentMarks.csv")
  totalCount <- length(studentData$RollNo)
  
  firstYearMarks <- NA
  secondYearMarks <- NA
  thirdYearMarks <- NA
  
  for(i in 1:totalCount)
  {
    tempMarks <- studentData[i,"Marks"]
    year <- studentData[i,"Year"]
    rollNo <- studentData[i,"RollNo"]
    
    if(roll_no == rollNo && year == 1)
    {
      firstYearMarks = tempMarks
    }
    else if(roll_no == rollNo && year == 2)
    {
      secondYearMarks = tempMarks
    }
    else if(roll_no == rollNo && year == 3)
    {
      thirdYearMarks = tempMarks
    }
    
  }
  yearColumn <- c("First","Second","Third")
  marksColumn <- c(firstYearMarks, secondYearMarks, thirdYearMarks)
  plotList <- data.frame(yearColumn, marksColumn)
  
  plot(x=plotList$yearColumn,y = plotList$marksColumn, xlab="Year", ylab = "Marks", type="l")
}

GetFirstRankedStudent <- function(year_val, division_val)
{
  studentData <- read.csv("StudentMarks.csv")
  
  studentYearSubset <- subset(studentData, Year == year_val & Division == division_val)
  sortedStudentSet <- studentYearSubset[order(studentYearSubset$Marks, decreasing = TRUE),]
  print(sortedStudentSet[1,1:ncol(sortedStudentSet)])
}