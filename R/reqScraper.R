## reqScraper SVSU Catalog Requisite Scraper
##
## This function produces a list of courses that require a course from the specified department.
## @param coursePrefix The department courses for which you are looking for requisites in other department courses
## @param catURL The url for the SVSU Catalog. Defaults to "http://catalog.svsu.edu/preview_course_nopop.php?catoid=39&coid="
## @param programURL The url for the program overview page in the catalog. Defaults to "http://catalog.svsu.edu/preview_program.php?catoid=39&poid="
## @param startID The courseID to start searching Defaults to 62669
## @param endID The courseID to end with Defaults to 64140
## @param startPID The programID to start searching Defaults to 4977
## @param endPID The programID to end with Defaults to 5121
## @param fullCat Boolean to return full catalog. Defaults to False to return only courses that have coursePrefix as requisite (outside of coursePrefix department)
## @return a tibble with catalog course ID, course prefix, course number, course description, course requisites, and flag for dept of interest
## @keywords SVSU Catalog Requisite
## @export
## @examples
## reqScraper("PSYC")

# Load Packages
require(tidyverse)
require(rvest)

reqScraper <- function(coursePrefix,catURL="http://catalog.svsu.edu/preview_course_nopop.php?catoid=39&coid=", startID = 62669, endID = 64140,fullCat=FALSE){

  #Create Course Search Sequence
  courseID <- seq(startID,endID)

  #Create holder variables
  courseDept <- rep(NA,length(courseID))
  courseNumber <- rep(NA,length(courseID))
  courseDescript <- rep(NA,length(courseID))
  courseReqs <- rep(NA,length(courseID))
  reqDept <- rep(FALSE,length(courseID))

  #Create tibble
  catReq <- tibble(courseID,courseDept,courseNumber,courseDescript,courseReqs,reqDept)

  #Counters for Progress
  j <- 1
  n <- nrow(catReq)

  for(i in courseID){

    #Load Course Preview Page (if it exists, skip to next courseID if not)
    skip_to_next <- FALSE
    tryCatch(catEnt <- xml2::read_html(paste(catURL,i,sep="")), error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }

    #Extract title from node
    catTitle <- catEnt %>% rvest::html_nodes("#course_preview_title") %>% html_text()

    #If No Title is Found, Go to next CourseID
    if(length(catTitle) == 0){
      next
    }

    #Select just Dept and Course Number
    courseInfo <- str_split(regmatches(catTitle,gregexpr("[A-Z]+\\s[0-9]{3}",catTitle))," ",n=2)

    #Save information to tibble
    catReq$courseDept[catReq$courseID == i] <- courseInfo[[1]][1]
    catReq$courseNumber[catReq$courseID == i] <- courseInfo[[1]][2]

    #Show Progress
    cat('\014')
    cat(paste0(round(j / n * 100), '% completed: Currently in the ',catReq$courseDept[catReq$courseID == i]," Department"))

    #Get full body of page
    catBody <- catEnt %>% rvest::html_nodes('body')

    #Find Course Description
    descrip <- regmatches(catBody,regexpr("(?x)(?<=<hr>)(.*)(?=<br><br><strong>)|(?<=</span>)(.*)(?=<br><br><strong>)",catBody,perl = TRUE))

    #Save it to tibble (if it exists)
    if(length(descrip)>0){
      catReq$courseDescript[catReq$courseID == i] <- str_split(descrip,"<br>")[[1]][1]
    }

    #Find Requisites Section
    result <- trimws(regmatches(catBody,regexpr("(?x)(?<=<strong>Requisites:</strong>)(.*)(?=\\()",catBody,perl = TRUE)))

    #Save Requisites to tibble
    if (length(result)>0){
      catReq$courseReqs[catReq$courseID == i] <- regmatches(result,gregexpr("[A-Z]+\\s[0-9]{3}",result))
    }

    if (j == n) cat(': Done')

    j <- j+1
  }

  if(fullCat == FALSE){
    catReq$reqDept[grep(coursePrefix,catReq$courseReqs)] <- TRUE
    catReq <- catReq[catReq$reqDept == TRUE & catReq$courseDept != coursePrefix,]
  }

  return(catReq)
}

reqProgramScraper <- function(coursePrefix,programURL="http://catalog.svsu.edu/preview_program.php?catoid=39&poid=", startPID = 4977, endPID = 5121){

  #Create Program Search Sequence
  programID <- seq(startPID,endPID)

  #Create holder variables
  program <- rep(NA,length(programID))
  reqDept <- rep(FALSE,length(programID))

  #Create tibble
  programReq <- tibble(programID,program,reqDept)

  #Counters for Progress
  j <- 1
  n <- nrow(programReq)

  for(i in programID){

    #Load Course Preview Page (if it exists, skip to next courseID if not)
    skip_to_next <- FALSE
    tryCatch(catEnt <- xml2::read_html(paste(programURL,i,sep="")), error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }

    #Extract title from node
    progTitle <- catEnt %>% rvest::html_nodes("h1") %>% html_text()
    cat(progTitle)

    #If No Title is Found, Go to next CourseID
    if(length(progTitle) == 0){
      next
    }

    #Save information to tibble
    programReq$program[programReq$programID == i] <- progTitle

    #Show Progress
    cat('\014')
    cat(paste0(round(j / n * 100), '% completed: Currently in the ',programReq$program[programReq$programID == i]))

    #Get full body of page
    catBody <- catEnt %>% rvest::html_nodes('body')

    #Search Body for coursePrefix
    if(length(grep(coursePrefix,catBody))>0){
      programReq$reqDept[programReq$programID == i] <- TRUE
    }

    if (j == n) cat(': Done')

    j <- j+1
  }

  programReq <- programReq[programReq$reqDept == TRUE,]

  return(programReq)
}
