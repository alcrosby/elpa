#' @importFrom dplyr %>% case_when bind_rows select
#' @importFrom rlang .data :=
#' @importFrom ggalluvial geom_alluvium
NULL
#> NULL

######################################################
#
# R functions for analyzing multiple years of
# ELPA21 Scores
#
######################################################
# Author: Albert Crosby (acrosby@sdale.org)
# Last Update: 8 June 2022
######################################################
# Abstract:
#   These are functions for analyzing ELPA data with R
#   as used by Springdale Public Schools.
######################################################

######################################################
# FUNCTION findnewestfile
#   pattern
#     a grep-style file name pattern (*not* DOS/WINDOWS)
#   read.data
#     T/F to read/not read data into list element "data"
#   path
#     the directory to search.  If omitted, the "Downloads"
#     directory inside the user's USERPROFILE directory is
#     used. (This is dependent on WINDOWS)
######################################################
# RETURNS
#   a list with elements matching the file parts, INCLUDING the data by default
#   NULL if file is not found
######################################################
#' Find the newest file (Windows only)
#'
#' Searches a path for a file matching the pattern and returns a list of items, including the contents
#' of the file, for common file parts
#'
#' @param pattern A grep-style pattern (NOT Windows wildcards) that is used to find the file
#' @param path A system-style path value
#' @param read.data logical - TRUE include the contents of the file in the list
#' @param ... Other options passed to read.csv
#'
#' @return LIST with members path, created, age, data
#' @export
findnewestfile <- function( pattern, read.data=TRUE, path=NULL,...) {
  if (is.null(path)) path<-paste(Sys.getenv("USERPROFILE"),"Downloads",sep="\\")
  rc <- NULL
  fl <- list.files(path=path,pattern=pattern,ignore.case=TRUE,full.names=TRUE)
  if (length(fl)==0) return(NULL)
  rc[["path"]]<-fl[order(file.info(fl)$mtime,decreasing=TRUE)[1]]
  rc[["created"]]<- file.info(rc[["path"]])$ctime
  rc[["age"]]<-as.numeric(Sys.time() - file.info(rc[["path"]])$ctime,units="hours")
  if (read.data) rc[["data"]]<-utils::read.csv(file=rc[["path"]],...)
  return(rc)
}

######################################################
# FUNCTION readelpascores
#   elpafiledirectory
#     location where the files for a single year's ELPA
#     results are located.  Should be the unzipped files
#     from the ELPA portal's CSV exports, one for "all"
#     and one with the scores per grade across the
#     district.
######################################################
# RETURNS
#   a data.table of ELPA scores
######################################################
# NOTES:
######################################################
#' Reads a directory of ELPA scores
#'
#' Reads the contents of a directory where the ELPA21 scores from the file portal have been expanded.
#' User is expected to download the CSV files from the ELPA21 portal, and unzip them.
#'
#' @param elpafiledirectory The path to the location where the ELPA21 files have been expanded.
#'
#' @note This helper package assumes that there are 13 grades for your district - K through 12.
#' For districts/schools with fewer grades, errors may result.  The tool needs testing to see if
#' it can be made to work correctly with a partial dataset.
#'
#' @return ELPA Dataset data.frame
#'
#' @export
readelpascores<-function(elpafiledirectory = "C:/Users/acrosby/Downloads/ELPASpring2022/") {
  # Find and read in all of the individual files, including the "all combined" file.

  # Note: There is no district name supplied so this code could be shared with others...

  workfiles<-NULL
#  workfiles[["ALL"]] <- findnewestfile(".*(ACO-WAN).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["K"]] <- findnewestfile(".*(KindergartenELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G01"]] <- findnewestfile(".*(Grade1ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G02"]] <- findnewestfile(".*(Grade2ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G03"]] <- findnewestfile(".*(Grade3ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G04"]] <- findnewestfile(".*(Grade4ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G05"]] <- findnewestfile(".*(Grade5ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G06"]] <- findnewestfile(".*(Grade6ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G07"]] <- findnewestfile(".*(Grade7ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G08"]] <- findnewestfile(".*(Grade8ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G09"]] <- findnewestfile(".*(Grade9ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G10"]] <- findnewestfile(".*(Grade10ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G11"]] <- findnewestfile(".*(Grade11ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")
  workfiles[["G12"]] <- findnewestfile(".*(Grade12ELPA).*(Summative).*(csv)$",path=elpafiledirectory,colClasses="character")

#  # Fix some variable name issues with ALL file
#  names(workfiles[["ALL"]][["data"]])[1]<-c("Test.Group")
#  workfiles[["ALL"]][["data"]]<-select(workfiles[["ALL"]][["data"]],-.data$X)

  # Fix column names in the other files, and create a single "merged" dataset for the entire district...
  merged<-NULL
  for (g in names(workfiles)[1:13]) {
    workfiles[[g]][["data"]]<-select(workfiles[[g]][["data"]],-.data$X)
    names(workfiles[[g]][["data"]])[1]<-c("Student.Name")
    names(workfiles[[g]][["data"]])[14:18]<-c("Summative.ScaleScore","Summative.ScaleScore.Standard.Error",
                                              "Summative.ComprehensionScaleScore","ComprehensionScaleScore.Standard.Error",
                                              "Summative.PerformanceLevel")
    merged<-bind_rows(merged,workfiles[[g]][["data"]])
  }

  merged
}

#' Is the data set formatted as an ELPA data set?
#'
#' A simple helper function that just checks for valid variable names in the dataset.  Will error if there are any
#' extra variables.
#'
#' Expects to find these variables: Student.ID, School, Ethnicity, Scale.Score, Performance.Level, SY
#'
#' @param ds A dataframe to validate
#' @return TRUE or FALSE
#' @export
helper.isvalidelpa<-function(ds=NULL) {
  case_when(
    is.null(ds) ~ FALSE,
    is.null(names(ds)) ~ FALSE,
    all(names(ds) %in% c("Student.ID","Enrolled.Grade","School","Ethnicity","Scale.Score","Performance.Level","SY")) &
      all(c("Student.ID","Enrolled.Grade","School","Ethnicity","Scale.Score","Performance.Level","SY") %in% names(ds)) ~ TRUE,
    TRUE ~ FALSE
  )
}

#' Reduce ELPA dataset
#'
#' Reduce a data set read using the \code{readelpadata()} function to the format expected by
#' \code{helper.isvalidelpa()} function.
#'
#' @param ds A dataset read by the \code{readelpadata()} function
#' @param SY The School Year for the dataset
#'
#' @return data frame formatted as ELPA data for this package
#' @export
helper.reducedata<-function(ds,SY=2022){
  ds %>% dplyr::select(
    Student.ID=.data$Student.ID,
    Enrolled.Grade=.data$Enrolled.Grade,
    School=.data$Enrolled.School,
    Ethnicity=.data$Ethnicity,
    Scale.Score=.data$Summative.ScaleScore,
    Performance.Level=.data$Summative.PerformanceLevel
  )%>%
    mutate(SY=SY)
}

#' Shift factorial by N levels
#'
#' Function
#'
#' @param x A vector of factors to be shifted
#' @param increment The number of levels to shift by applying function .fun (defaults to add)
#' @param cap If true, return first element if factor level shifted below first level
#' or last level if shifted above last level; otherwise return NA if level shifts out
#' of bounds
#' @param wrap UNIMPLEMENTED, if TRUE, wrap values around when shifting out of bounds;
#' ie. next level of last level is first level.
#' @param .fun Function to apply when shifting levels.  Default is `+` to increment to
#' next level, `-` will shift to previous level, other functions will work but may not
#' be meaningful
#'
#' @return
#' @export
#'
#' @examples
fct_shift_ord <- function(x, increment = 1, cap = TRUE,wrap=FALSE, .fun = `+`){
  x_nlevel <- nlevels(x)
  x_lables <- levels(x)

  # apply function .fun to the numeric of the ordered vector
  erg <-.fun(as.numeric(x), increment)


  # cap to 1 and x_nlevel if the increment was larger than the original range of the factor levels
  if (cap) {
    erg[erg<1] <- 1
    erg[erg>x_nlevel] <- x_nlevel
  }
  ordered(round(erg), levels = 1:x_nlevel, labels = x_lables)
}

#' Function to clean elpa dataset
#'
#' Performs several standardized housekeeping tasks to a 'standard' ELPA dataset.
#'
#' Fixes school names in the *School* variable to remove non alpha characters, maps *Enrolled.Grade*
#' so that it's always a two digit value, maps common values in *Ethnicity* to a standard set,
#' and sets *Performance.Level*,*Enrolled.Grade*,*Ethnicity* to be factors.
#'
#' @param ds data frame that is constructed to the expected format
#' @return Data frame
#' @export
#' @importFrom dplyr mutate
#' @importFrom stringr str_squish str_extract_all str_detect str_sub str_to_upper
#' @importFrom hablar convert chr int fct
helper.cleandata<-function(ds) {
  stopifnot("Dataset is not formatted as elpa data"=helper.isvalidelpa(ds))
  ds %>%
  dplyr::mutate(
    School=stringr::str_squish(unlist(lapply(stringr::str_extract_all(.data$School,"[A-Za-z .]+"),FUN=paste,collapse=''))),
    Enrolled.Grade=stringr::str_sub(paste("00",.data$Enrolled.Grade,sep=''),-2,-1),
    Ethnicity=dplyr::case_when(
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'WHITE') ~ 'White',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'CAUCASIAN') ~ 'White',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'ASIAN') ~ 'Asian',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'MULTI') ~ 'Multi-Racial',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'HISPANIC') ~ 'Hispanic',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'LATINO') ~ 'Hispanic',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'ISLANDER') ~ 'Islander',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'PACIFIC') ~ 'Islander',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'HAWAIIAN') ~ 'Islander',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'BLACK') ~ 'Black',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'AFRICAN') ~ 'Black',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'INDIAN') ~ 'Native American',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'ALASKAN') ~ 'Native American',
      stringr::str_detect(stringr::str_to_upper(.data$Ethnicity),'NATIVE') ~ 'Native American',
      TRUE ~ 'Other'
    )
  ) %>%
    hablar::convert(
      hablar::chr(.data$Student.ID),
      hablar::fct(.data$Enrolled.Grade,.args=list(ordered=T,levels=c('KG','01','02','03','04','05','06','07','08','09','10','11','12'))),
      hablar::fct(.data$Performance.Level,.args=list(ordered=T,levels=c("Emerging","Progressing","Proficient"))),
      hablar::fct(.data$Ethnicity),
      hablar::int(.data$Scale.Score)
    )
}

#' Map grades to ELPA21 Grade Bands
#'
#' The ELPA21 test is differentiated across Grade Bands; this helper function returns
#' a grade band for the grade passed.
#'
#' Built with bands as of 2019: KG, 1, 2-3, 4-5, 6-8, 9-12.  Unknown values return
#' Grade Band 9-12.  Value is returned as an ordered factor.
#'
#' @param grade Grade number
#' @return Ordered factor for gradeband
#' @export
helper.gradeband<-function(grade){
  factor(case_when(
    grade == 'KG' ~ 'KG',
    grade == '01' ~ '1',
    grade %in% c('02','03') ~ '2-3',
    grade %in% c('04','05') ~ '4-5',
    grade %in% c('06','07','08') ~ '6-8',
    TRUE ~ '9-12'
  ),ordered=TRUE,levels=c('KG','1','2-3','4-5','6-8','9-12'))
}

#' Collapses ethnicity to three groups
#'
#' Returns Ethnicity collapsed for charts
#'
#' In Springdale, the largest groups of language learners are Hispanic, followed by Pacific Islanders.
#' This function returns ethnicity as simply 'Hispanic', 'Islander' or 'Other'.
#'
#' @param Ethnicity Ethnicity, as modified by \code{helper.cleandata()}
#' @return Grouped ethnicity, as factor
#' @export
helper.ethnicity.groups<-function(Ethnicity) {
  factor(case_when(Ethnicity=='Hispanic'~ 'Hispanic',Ethnicity=='Islander'~'Islander', TRUE~'Other'))
}

#' Returns an alphabetized list of schools that serve a specified grade or grades
#'
#' Identify schools that serve a specified grade *or* any one of the grades in a vector of grades
#'
#' @param df ELPA data set
#' @param grade a single grade or a vector of grades
#' @return Schools that serve the grade(s) indicated ordered alphabetically, as a vector of character strings
#' @examples
#' helper.schools.with.grade(elpadata,'KG')
#' helper.schools.with.grade(elpadata,c('KG','06'))
#' @export
helper.schools.with.grade<-function(df,grade) {
  unlist(df %>% dplyr::filter(.data$Enrolled.Grade %in% grade) %>% dplyr::distinct(.data$School) %>% dplyr::arrange(.data$School))
}

#' Creates a Title Page using ggplot
#'
#' This function creates a simple title page that can be included in a PDF or set of graphical slides
#'
#' Uses ggplot and geom_fit_text to create a title page with standardized options.
#'
#' @param title String to place as a title block on the page
#' @param subtitle A slightly smaller second line of a title
#' @param author String positioned immediately below the subtitle
#' @param footnote A string of text that is positioned at the lower right corner of the page
#' @param cornertext String to place in the top left corner of the page
#' @param textbox A string to place as a textbox in the "middle" of the title page
#' @param title.face Font face for the title (plain, bold, italic, etc.)
#' @param title.family Font family for the title
#' @param title.reflow TRUE - allow the Title to be reflown across multiple lines
#' @param subtitle.face Font face for the subtitle (plain, bold, italic, etc.)
#' @param subtitle.reflow TRUE - allow the Subtitle to reflown across multiple lines
#' @param author.face Font face for the "author" line (plain, bold, italic, etc.)
#' @examples
#' helper.titlepage("ELPA Graphics in R","A Package to Help Explore ELPA21 Data","Albert Crosby")
#' @return ggplot object that contains a title page
#' @export
#' @importFrom ggplot2 ggplot theme_void
#'
helper.titlepage<-function(title="",subtitle="",author="",footnote="",cornertext="",textbox="",
                           title.face="bold",title.family="serif",title.reflow=TRUE,
                           subtitle.face="plain",subtitle.reflow=FALSE,
                           author.face="italic") {
  ggplot2::ggplot()+
    ggplot2::theme_void() +
    ggfittext::geom_fit_text(size=50,ggplot2::aes(x=5,y=9,label=title,family=title.family,fontface=title.face),reflow=title.reflow,width=10,height=1)+
    ggfittext::geom_fit_text(size=25,ggplot2::aes(x=5,y=8,label=subtitle,fontface=subtitle.face),reflow=subtitle.reflow,width=9,height=.75)+
    ggfittext::geom_fit_text(size=15,ggplot2::aes(x=5,y=7.5,label=author,fontface=author.face),reflow=FALSE,width=4,height=.5)+
    ggfittext::geom_fit_text(size=10,place="topleft",ggplot2::aes(x=5,y=3.75,label=textbox),reflow=TRUE,width=10,height=6,hjust=0,vjust=0)+
    ggfittext::geom_fit_text(place="topleft",ggplot2::aes(xmin=0,xmax=10,ymin=0,ymax=10,label=cornertext))+
    ggfittext::geom_fit_text(place="bottomright",ggplot2::aes(xmin=0,xmax=10,ymin=0,ymax=10,label=footnote))
}

######################################################
# PLOT FUNCTIONS
######################################################

#' Plot Number of Students Tested By SY
#'
#' Plots the number of students tested in each School Year (SY) included in the dataset.  Bars are split by
#' the overall performance level reported, and labelled with N sizes.
#'
#' @param ds ELPA21 dataset
#' @param label.size Size of label, defaults to 3
#' @return plot object from ggplot
#' @export
elpa.plot.ntested<-function(ds,label.size=3) {
  base<-ds %>% ggplot2::ggplot(ggplot2::aes(y=.data$SY,fill=.data$Performance.Level)) +
    ggplot2::geom_bar() +
  ggplot2::geom_text(ggplot2::aes(label = ggplot2::after_stat(.data$count)),position="stack", stat = "count",angle=-90,size=label.size) +
  ggplot2::facet_grid(.data$SY~.,scales="free_y",switch="both")
  base+ggplot2::xlab("")+ggplot2::ylab("School Year")+
    ggplot2::scale_fill_manual(values=c('khaki1','lightBlue','lightgreen'))+
    ggplot2::theme(
      axis.text.y=ggplot2::element_blank(),
      axis.line.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank()
    )
}

#' Density Plots for All Years Overlaid
#'
#' Density plots show the distribution of values on a common scale.  This plot shows the density distribution of
#' the Scale.Score, by grade and all levels of SY (School Year) in the dataset.
#'
#' @param ds An ELPA21 dataset
#' @note The title is not properly adjusted if there is only one level of SY in the dataset.
#' @export
elpa.plot.density.allyears<-function(ds) {
  ds %>% ggplot2::ggplot(ggplot2::aes(x=.data$Scale.Score,fill=.data$SY,color=.data$SY)) +
    ggplot2::geom_density(alpha=.4) + ggplot2::facet_grid(Enrolled.Grade ~ .) +
    ggplot2::theme(
      axis.ticks.y=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
    ) +
    ggplot2::xlab("Overall Scaled Score") +
    ggplot2::ylab("Density Distribution") +
    ggplot2::ggtitle(paste("Density Distributions of",min(ds$SY),"to",max(ds$SY),"ELPA Summative Overall Scaled Scores By Grade"))
}

#' Comparison Plot of Density for Two Years Using Z-test
#'
#' This plot compares two distributions of the Scale.Score, by grade, using the Z-test.  The Density Distributions are
#' colored based on the size of the Z-statistic.
#'
#' The Z-statistic is based on the standard normal distribution and creates a score that demonstrates the likliehood
#' that two distributions are from the same or different distributions.  Z-statistics less than 1.99 are considered
#' "Same", Z-statistics between 2 and 2.5 are considered "Marginally Different", 2.5 to 3.0 is considered
#' "Significantly Different", and two distributions with a Z-statistic greater than 3.0 are considered to be
#' "Highly Significant Different".\cr
#' The dataset is filtered for each identified school year and joined to compare distributions given to students in
#' the same grade in each year.
#'
#' @param ds ELPA format dataset
#' @param y1 School Year 1 for comparison.  If null, is the min of SY in the dataset.
#' @param y2 School Year 2 for comparison.  If null, is the max of SY in the dataset.
#' @return A ggplot of Density Distrbutions, colored by the Z-statistic comparison of the distributions, by grade.
#' @references http://homework.uoregon.edu/pub/class/es202/ztest.html
#' @export
elpa.plot.density.compare<-function(ds,y1=NULL,y2=NULL) {
  # for a reference on the Z-test:
  #   http://homework.uoregon.edu/pub/class/es202/ztest.html
  if(is.null(y1)) {y1<-min(ds$SY)}
  if(is.null(y2)) {y2<-max(ds$SY)}
  s<-ds%>%
    dplyr::group_by(.data$SY,.data$Enrolled.Grade) %>%
    dplyr::summarise(
      average=mean(as.numeric(.data$Scale.Score)),
      sd=stats::sd(as.numeric(.data$Scale.Score)),
      n=dplyr::n()
    )
  s2<-dplyr::inner_join(dplyr::filter(s,.data$SY==y1),dplyr::filter(s,.data$SY==y2),by=c("Enrolled.Grade"="Enrolled.Grade"))%>%
    dplyr::mutate(
      Z_Stat=abs((.data$average.x-.data$average.y) / sqrt( (.data$sd.x/sqrt(.data$n.x))^2 + (.data$sd.y/sqrt(.data$n.y))^2 )),
      Distribution=dplyr::case_when(
        between(Z_Stat,0,1.9999) ~ 'Same',
        between(Z_Stat,2.0,2.4999) ~ 'Marginally Different',
        between(Z_Stat,2.5,2.9999) ~ 'Significantly Different',
        TRUE ~ 'Highly Significantly Different'
      )
    )%>%
    convert(fct(.data$Distribution,.args=list(levels=c('Same','Marginally Different','Significantly Different','Highly Significantly Different'))))

  dplyr::left_join(ds,select(s2,.data$Enrolled.Grade,.data$Distribution),by=c("Enrolled.Grade"="Enrolled.Grade"))%>%
    dplyr::filter(.data$SY %in% c(y1,y2))%>%
    ggplot2::ggplot(ggplot2::aes(x=.data$Scale.Score,fill=.data$Distribution)) +
    ggplot2::geom_density() + ggplot2::facet_grid(.data$Enrolled.Grade ~ .data$SY) +
    ggplot2::scale_fill_manual(values=c('lightGreen','lightBlue','khaki1','indianred1'))+
    ggplot2::theme(
      axis.ticks.y=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
    ) +
    ggplot2::xlab("Overall Scaled Score") +
    ggplot2::ylab("Density Distribution") +
    ggplot2::labs(fill="Density Comparison") +
    ggplot2::ggtitle(paste("Density Distributions of",y1,"vs",y2,"ELPA Summative Overall Scaled Scores"))
}

#' Bar chart of Change in Proficiency Levels Between Two Years
#'
#' A simple bar chart  of students who tested in both of two speficied years
#' showing the proportion of students by Proficiency Level in the first year compared
#' to proficiency level in the second year.
#'
#' @param ds An ELPA data set
#' @param y1 The first School Year to compare - defaults to min(SY) in the dataset
#' @param y2 The second School Year for comparison - defaults to max(SY)
#' @param label.size Size of labels on the bar charts
#' @param label.hjust hjust of the labels
#' @return Bar chart (ggplot) of the proportion of students who tested in both of the years specified by proficiency change.
#' @seealso ggplot2::ggplot ggplot2::geom_bar ggplot2::geom_text
#' @export
elpa.plot.profchange<-function(ds,y1=NULL,y2=NULL,label.size=3.8,label.hjust=0) {
  if(is.null(y1)) {y1<-min(ds$SY)}
  if(is.null(y2)) {y2<-max(ds$SY)}
  dplyr::inner_join(dplyr::filter(ds,.data$SY==y1),dplyr::filter(ds,.data$SY==y2),by=c("Student.ID")) %>%
    dplyr::mutate(
      Change = factor(paste(.data$Performance.Level.x,'->',.data$Performance.Level.y,sep="\n"),
                      levels=c('Emerging\n->\nEmerging','Emerging\n->\nProgressing','Emerging\n->\nProficient',
                               'Progressing\n->\nEmerging','Progressing\n->\nProgressing','Progressing\n->\nProficient',
                               'Proficient\n->\nEmerging','Proficient\n->\nProgressing','Proficient\n->\nProficient'
                      ))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(y=.data$Change)) +
    ggplot2::geom_bar(ggplot2::aes(fill=.data$Performance.Level.x,color=dplyr::case_when(
      .data$Performance.Level.x<.data$Performance.Level.y ~ 'Regressed',
      .data$Performance.Level.x>.data$Performance.Level.y ~ 'Advanced',
      TRUE ~ 'No Change')
    ),size=1) +
    ggplot2::ylab("") +
    ggplot2::xlab("Number of Students") +
    ggplot2::ggtitle(paste("Change in Overall Proficiency Level SY",y1,"to SY",y2),
            subtitle=paste("Students Who Tested in BOTH",y1,"and",y2))+
    ggplot2::geom_text(
      ggplot2::aes(label=ggplot2::after_stat(scales::label_percent(accuracy=0.1)(.data$prop)),group=1),
      stat='count',hjust=label.hjust,size=label.size
    )+ggplot2::theme(legend.position="none")+ggplot2::scale_color_manual(values=c("red","ivory4","green"))+
    ggplot2::scale_fill_manual(values=c("firebrick","deepskyblue","forestgreen"))
}

#' Density Distribution Comparing New To District to Students
#'
#' A density distribution comparing students who are new to the district to students who tested in two given
#' years.  For this comparison, Kindergartners testing for the first time in the second year are considered
#' "New to District".
#'
#' @param ds ELPA dataset
#' @param y1 First year to compare
#' @param y2 Second year to compare
#' @param overlaid Overlay the Density Plots (TRUE) or Side-by-side (FALSE)
#' @param label.size Size of label on the chart
#' @return A Density Plot (ggplot oblect) comparing "New to District" students to returning students.
#' @export
elpa.plot.density.compare.newtodistrict<-function(ds,y1,y2,overlaid=FALSE,label.size=3.8) {
  tmp<-dplyr::left_join(dplyr::filter(ds,.data$SY==y2),dplyr::filter(ds,.data$SY==y1),by="Student.ID") %>%
    dplyr::mutate(
      Tested=dplyr::if_else(!is.na(.data$SY.y),paste('Tested SY',y1),paste('Not Tested SY',y1))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$Scale.Score.x,fill=.data$Tested))+
    ggplot2::geom_density()+
    ggplot2::xlab("Scale Score")+
    ggplot2::ggtitle(paste('Distribution of Spring',y2,'Overal Scaled Score'))+
    ggplot2::theme(
      axis.text.y=ggplot2::element_blank(),
      axis.line.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank()
    )
  if (overlaid) tmp<-tmp+ggplot2::facet_grid(Enrolled.Grade.x~Tested,switch="both")
  return(tmp)
}

#' Plot a Histogram of Scale.Score
#'
#' Plots a simple histogram of the Scale Score by levels of SY in the dataset.
#'
#' @param ds ELPA data set
#' @param binwidth Number of Scale Score points to treat as a single bar on the histogram
#' @param text.size.y Size of labels on the Y axis
#' @return Histogram (ggplot object) of Scale Score
#' @export
elpa.plot.histogram<-function(ds,binwidth=50,text.size.y=10) {
  ds%>%
    ggplot2::ggplot(ggplot2::aes(x=.data$Scale.Score))+ggplot2::geom_histogram(alpha=0.5,position="identity",binwidth=binwidth)+
    ggplot2::ggtitle(paste('Distribution of Spring Overall Scaled Score'))+
    ggplot2::theme(
      axis.text.y=ggplot2::element_text(size=text.size.y),
      axis.line.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank()
    ) + ggplot2::xlab(paste("Scale Score")) + ggplot2::ylab("Number of Students")+
    ggplot2::scale_fill_manual(values=c("blue3","red","green")) + ggplot2::facet_grid(.data$SY ~ .,switch="both")
}

#' Plot a Histogram of Scale.Score Comparing New to District to Returning Students
#'
#' Plots a histogram of the Scale Score by levels of SY in the dataset, comparing
#' students who only tested in Year 2 to students who tested in both Year 1 and Year 2.
#'
#' @param ds ELPA data set
#' @param y1 School Year 1
#' @param y2 School Year 2
#' @param binwidth Number of Scale Score points to treat as a single bar on the histogram
#' @param text.size.y Size of labels on the Y axis
#' @return Histogram (ggplot object) of Scale Score
#' @export
elpa.plot.histogram.compare.newtodistrict<-function(ds,y1,y2,binwidth=50,text.size.y=10) {
  dplyr::left_join(dplyr::filter(ds,.data$SY==y2),dplyr::filter(ds,.data$SY==y1),by="Student.ID") %>%
    dplyr::mutate(
      Tested=factor(dplyr::if_else(!is.na(.data$SY.y),paste('Tested in District SY',y1),paste('New To District/K SY',y2)),ordered=T,
                    levels=c(paste('Tested in District SY',y1),paste('New To District/K SY',y2)))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$Scale.Score.x,fill=.data$Tested))+
    ggplot2::geom_histogram(alpha=0.5,position="identity",binwidth=binwidth)+
    ggplot2::ggtitle(paste('Distribution of Spring',y2,'Overall Scaled Score'))+
    ggplot2::theme(
      axis.text.y=ggplot2::element_text(size=text.size.y),
      axis.line.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank()
    ) + ggplot2::xlab(paste("Scale Score SY",y2)) + ggplot2::ylab("Number of Students")+
    ggplot2::scale_fill_manual(values=c("blue3","red","green"))
}

#' Simple Plot of Students who Tested in Both of Two Years
#'
#' Growth can only be considered for students who tested in more than one year.  This bar chart shows
#' the number of students who tested in BOTH years and the proportion scoring each Proficiency Level by
#' school year.
#'
#' @param ds ELPA dataset
#' @param y1 First Year
#' @param y2 Second Year (must be one year after first year)
#' @returns Bar chart (ggplot object)
#' @export
elpa.plot.testedboth<-function(ds,y1,y2) {
  if(is.null(y1)) {y1<-min(ds$SY)}
  if(is.null(y2)) {y2<-max(ds$SY)}
  stopifnot("Year 2 must be the year following Year 1" = as.integer(y2)-as.integer(y1)==1)

  dplyr::semi_join(ds,dplyr::inner_join(dplyr::filter(ds,.data$SY==y1),dplyr::filter(ds,.data$SY==y2),by=c("Student.ID"),suffix=c(".y1",".y2"))) %>%
    dplyr::filter(.data$SY %in% c(y1,y2)) %>% dplyr::arrange(.data$Student.ID,.data$SY) %>%
    hablar::convert(hablar::chr(.data$Enrolled.Grade)) %>%
    dplyr::mutate(
      # Make a 'MatchedGrade' variable that connects 10th graders in Y1 with 11th graders in Y2, value should be grade in Y2
      Grade.in.y2 = factor(dplyr::case_when(
        .data$SY == y2 & .data$Enrolled.Grade == 'KG' ~ '01', # Put the handful of retained kindergartners in 1st grade to avoid confusion
        .data$SY == y2 ~ .data$Enrolled.Grade, # Except for retained kindergartners, use the Grade in y2
        .data$Enrolled.Grade == 'KG' ~ '01',
        .data$Enrolled.Grade == '01' ~ '02',
        .data$Enrolled.Grade == '02' ~ '03',
        .data$Enrolled.Grade == '03' ~ '04',
        .data$Enrolled.Grade == '04' ~ '05',
        .data$Enrolled.Grade == '05' ~ '06',
        .data$Enrolled.Grade == '06' ~ '07',
        .data$Enrolled.Grade == '07' ~ '08',
        .data$Enrolled.Grade == '08' ~ '09',
        .data$Enrolled.Grade == '09' ~ '10',
        .data$Enrolled.Grade == '10' ~ '11',
        .data$Enrolled.Grade == '11' ~ '12',
        .data$Enrolled.Grade == '12' ~ '12', # People who were seniors in y1 are still seniors in y2
        TRUE ~ 'XX'
      ),levels=c('KG','01','02','03','04','05','06','07','08','09','10','11','12')
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$Performance.Level,fill=.data$Performance.Level))+
    ggplot2::geom_bar() +
    ggplot2::facet_grid(.data$SY ~ .data$Grade.in.y2 , switch='both') +
    ggplot2::geom_text(
      ggplot2::aes(label=ggplot2::after_stat(scales::label_percent(accuracy=2)(.data$prop)),group=1),
      stat='count',vjust=-.5,size=2
    ) +
    ggplot2::ylab("Number of Students") +
    ggplot2::xlab(paste("Grade in SY",y2,sep='')) +
    ggplot2::theme(
      axis.text.x=ggplot2::element_blank(),
      axis.line.x=ggplot2::element_blank(),
      axis.ticks.x=ggplot2::element_blank()
    ) + ggplot2::ggtitle("Overall ELPA Performance Level",
                subtitle=paste('Students who tested in BOTH',y1,'and',y2))
}

#' Stacked Plot of Proficiency Change Between Two Years
#'
#' A stacked version of the simple bar chart  of students who tested in both of two speficied years
#' showing the proportion of students by Proficiency Level in the first year compared
#' to proficiency level in the second year.
#'
#' @param ds An ELPA data set
#' @param y1 The first School Year to compare - defaults to min(SY) in the dataset
#' @param y2 The second School Year for comparison - defaults to max(SY)
#'
#' @return Bar chart (ggplot) of the proportion of students who tested in both of the years specified by proficiency change.
#'
#' @seealso ggplot2::ggplot ggplot2::geom_bar ggplot2::geom_text
#' @export
elpa.plot.profchange.bylevel.stacked<-function(ds,y1,y2){
  dplyr::inner_join(dplyr::filter(ds,.data$SY==y1),dplyr::filter(ds,.data$SY==y2),by=c("Student.ID"),suffix=c(".y1",".y2")) %>%
    dplyr::mutate(
      Change = paste(.data$Performance.Level.y1,.data$Performance.Level.y2,sep="->")
    ) %>%
    hablar::convert(hablar::fct(.data$Change,.args=list(
      levels=c('Emerging->Emerging','Emerging->Progressing','Emerging->Proficient',
               'Progressing->Emerging','Progressing->Progressing','Progressing->Proficient',
               'Proficient->Emerging','Proficient->Progressing','Proficient->Proficient'
      ))),
      hablar::fct(.data$Performance.Level.y1,.data$Performance.Level.y2,.args=list(ordered=T,levels=c('Proficient','Progressing','Emerging')))
    )%>% ggplot2::ggplot( ggplot2::aes(y=.data$Performance.Level.y1, fill = .data$Performance.Level.y2))+
    ggplot2::geom_bar(position = "fill") +
    ggplot2::ylab(paste("Proficiency Level in",y1)) +
    ggplot2::xlab("") +
    ggplot2::geom_text(ggplot2::aes(label = .data$..count..), stat = "count", position = "fill", angle = 90) +
    ggplot2::scale_x_continuous(labels=scales::percent)+
    ggplot2::scale_fill_manual(values=c('lightBlue','lightgreen','khaki1'))+
    ggplot2::guides(fill=ggplot2::guide_legend(title=paste("Proficiency Level\nSchool Year ",y2,sep="")))+
    ggplot2::facet_grid(.data$Performance.Level.y1 ~ .,scales="free",switch="both") +
    ggplot2::theme(
      axis.text.y=ggplot2::element_blank(),
      axis.line.y=ggplot2::element_blank(),
      axis.ticks.y=ggplot2::element_blank()
    )+
    ggplot2::ggtitle(paste(y2,"Change in Overall Proficiency Level By Performance Level in",y1),
            subtitle=paste("Students Who Tested in BOTH ",y1,"and",y2))
}

#' Flow Plot for ELPA Students Between Two Years
#'
#' A "Flow" plot (alluvial plot) showing the movement of EL students from one year to the next.
#'
#' @param df ELPA dataset
#' @param y1 Starting Year
#' @param y2 Ending Year
#' @param adjustExit (not yet implemented) Adjust "Exit" to show that some exited students may graduate or leave
#' the district without exiting.
#' @param incN TRUE/FALSE show the N sizes in the columns at the right and left
#' @param nsep String between labels in column at left and right and N sizes.  Defaults to \\n (newline).
#' @param labelflow TRUE/FALSE to put label sizes in the flows
#' @param unit Name of the unit.  Suggestions:  District (default), School, State
#' @param flow.toright TRUE Colors flows from Left to Right, highlighting movement from left to right, highlighting
#' movement from the Starting Year to the Ending Year.  FALSE reverses the color highlighting.
#' @return ELPA Flow Plot (ggplot object)
#' @note This package uses ONLY information available in the ELPA data set.\cr
#' A student is considered "New" if they only tested in the second year.\cr
#' Students who only tested in the first year experienced one of several possible
#' outcomes: (1) they may have "Left"; (2) if they were tested as Proficient in the first year, they may have
#' "Exited" in the second year; (3) if they were Seniors in the first year, they may have graduated at the end
#' of the first year and thus not tested in the second year.\cr
#' For convenience sake, ALL students who were proficient
#' when tested in the first year who did not test in the second year are counted as "Exited".\cr
#' The planned *adjustExit* parameter would add an adjustment to show that some proportion of the students
#' who tested Proficient in the first year but did not test in the second year actually remained in services
#' and either graduated or moved out of the district.
#' @seealso ggalluvial::ggalluvial
#' @export
elpa.plot.flow<-function(df,y1,y2,adjustExit=FALSE,incN=FALSE,nsep='\n',
                         labelflow=FALSE,unit='District',
                         flow.toright=TRUE) {
  elpadata.both<-dplyr::full_join(
    dplyr::filter(df,.data$SY==y1),
    dplyr::filter(df,.data$SY==y2),
    by="Student.ID",suffix=c(".y1",".y2")
  ) %>% dplyr::select(-dplyr::starts_with(c("Scale","SY"))) %>%
    mutate(
      StartY1= case_when(
        .data$Enrolled.Grade.y2=="KG" & is.na(.data$Performance.Level.y1) ~ paste("Kindergartner in",y2),
        is.na(.data$Performance.Level.y1) & !is.na(.data$Performance.Level.y2) ~ paste("New to",unit,"in",y2),
        TRUE ~ paste(.data$Performance.Level.y1,"in",y1)
      ),
      EndY2=case_when(
        .data$Enrolled.Grade.y1=="12" & is.na(.data$Performance.Level.y2) ~ paste("Graduated in",y1),
        .data$Performance.Level.y1=="Proficient" & is.na(.data$Performance.Level.y2) ~ paste("Exited Services in",y2),
        is.na(.data$Performance.Level.y2) & !is.na(.data$Performance.Level.y1) ~ paste("Left",unit,"in",y2),
        TRUE ~ paste(.data$Performance.Level.y2,"in",y2)
      )
    ) %>% select(.data$Student.ID,.data$StartY1,.data$EndY2) %>%
    dplyr::group_by(.data$StartY1,.data$EndY2) %>%
    dplyr::summarise(
      freq=dplyr::n()
    ) %>%
    hablar::convert(
      hablar::fct(.data$StartY1,.args=list(ordered=TRUE,
                             levels=c(paste("Proficient in",y1),paste("Progressing in",y1),
                                      paste("Emerging in",y1),paste("Kindergartner in",y2),
                                      paste("New to",unit,"in",y2)))),
      hablar::fct(.data$EndY2,.args=list(ordered=TRUE,
                           levels=c(paste("Exited Services in",y2),
                                    paste("Proficient in",y2),paste("Progressing in",y2),
                                    paste("Emerging in",y2),
                                    paste("Graduated in",y1),
                                    paste("Left",unit,"in",y2))))
    )

  if (adjustExit){

  }

  tmp<-elpadata.both %>%
    ggplot2::ggplot(
      ggplot2::aes(axis1=.data$StartY1,axis2=.data$EndY2,y=.data$freq)
    )
  if (flow.toright) tmp<-tmp+ggalluvial::geom_alluvium(ggplot2::aes(fill=.data$StartY1))
  else tmp<-tmp+ggalluvial::geom_alluvium(ggplot2::aes(fill=.data$EndY2))

  tmp<-tmp+
    ggalluvial::geom_stratum(fill="lightblue")+
    ggplot2::scale_x_discrete(limits = c("Survey", "Response"),
                     expand = c(0.15, 0.05)) +
    ggplot2::theme_void()+
    ggplot2::theme(legend.position="none")+
    ggplot2::scale_fill_manual(values=c("blue","gold","red","green","turquoise4","salmon"),drop=FALSE) +
    ggplot2::labs(title=paste("ELPA21 Students Tested in Either",y1,"and",y2))+
    ggplot2::labs(caption="Note: Report is based solely on information received directly from the ELPA21 Portal.")

  if (incN) {
    tmp<-tmp+ggfittext::geom_fit_text(widt=1/3,stat="stratum",min.size=2,ggplot2::aes(label=paste(ggplot2::after_stat(.data$stratum),
                                                                   nsep,'(',ggplot2::after_stat(.data$count),')',
                                                                   sep='')
                                                                  )
    )
  } else {
    tmp<-tmp+ggfittext::geom_fit_text(width=1/3,stat="stratum",min.size=2,ggplot2::aes(label=ggplot2::after_stat(.data$stratum)))
  }
  if (labelflow) {
    tmp <- tmp+ggplot2::geom_text(size=2,stat="alluvium",nudge_x=c(.19),color="grey25",ggplot2::aes(label=ggplot2::after_stat(.data$count))) +
      ggplot2::geom_text(size=2,stat="alluvium",nudge_x=c(-.19),color="grey25",ggplot2::aes(label=ggplot2::after_stat(.data$count))) +
      ggplot2::geom_vline(xintercept=2.1875,color="white",size=5)+
      ggplot2::geom_vline(xintercept=0.8,color="white",size=9)
  }
  tmp
}

#' Flow Plot for a Single Building
#'
#' A version of the Flow Plot that is filtered to a single school.
#'
#' If a *testedyear* is not supplied, then ALL students who are tested in ***either*** year at the
#' specified school are included in the plot.  For example, if a student tested at
#' School 1 in Grade 5 in Year 1, and then tested at School 2 in Grade 6 in Year 2, her scores for *both*
#' Grade 5 and Grade 6.  This can result in some of the counts appearing inflated.\cr\cr
#' If *testedyear* is supplied, then only students who actually tested at the specified building
#' in that year will be included.
#'
#' @param df An ELPA data set
#' @param school The name of a single school appearing in the data set.
#' @param y1 The first year for comparison
#' @param y2 The second year for comparison
#' @param testedyear (OPTIONAL) If supplied, only students who tested at the building in this year
#' will be included
#' @param unit The label for the Unit field, defaults to "District"
#' @param ... other options to pass to elpa.plot.flow
#'
#' @return ggplot object containing an ELPA Flow Plot for a specified building
#'
#' @export
elpa.plot.flow.building<-function(df,school,y1,y2,testedyear=NULL,unit="District",...) {
  if (is.null(testedyear)) {
    limit<-dplyr::bind_rows(
      df %>% dplyr::filter(.data$School==school & .data$SY==y1) %>%
        dplyr::select(.data$Student.ID,.data$Enrolled.Grade,.data$SY),
      df %>% dplyr::filter(.data$School==school & .data$SY==y2) %>%
        dplyr::select(.data$Student.ID,.data$Enrolled.Grade,.data$SY)
    )
    tyear<-paste("Either",y1,"or",y2)
  } else {
    limit<-df%>%dplyr::filter(.data$School==school & .data$SY==testedyear) %>% select(.data$Student.ID,.data$Enrolled.Grade,.data$SY)
    tyear<-testedyear
  }

  subtitle=paste("Includes",y2,"scores for students tested in",y1,"who changed building in district in",y2)

  flow.toright<-!identical(testedyear,y2)
  dplyr::semi_join(df,
            limit,
            by=c("Student.ID")
  ) %>% elpa.plot.flow(y1,y2,unit=unit,flow.toright=flow.toright,...) + ggplot2::labs(title=paste("Students Tested at",school," in",tyear)

  )
}


#' ELPA Flow Plot of a Single Grade
#'
#' Creates an ELPA Flow Plot for the students in a single grade in EITHER the First Year
#' or the Second Year.
#'
#' @param df An ELPA dataset
#' @param grade The grade level to display
#' @param y1 First year of comparison
#' @param y2 Second year of comparison
#' @param gradeyear The year that the specified grade is matched.  If unspecified,
#' the grade is assumed to be in the Second Year.
#' @param ... Other parameters passed elpa.plot.flow
#'
#' @seealso elpa.plot.flow
#'
#' @return A ggplot object containing an ELPA Flow Chart
#' @export
#'
#' @examples
#' elpa.plot.flow.grade(elpadata,'04',2021,2022,gradeyear=2022)
elpa.plot.flow.grade<-function(df,grade,y1,y2,gradeyear=NULL,...) {
  if (is.null(gradeyear)) gradeyear=y2
  flow.toright<-!identical(gradeyear,y2)
  dplyr::semi_join(df,
              df %>% dplyr::filter(.data$SY==gradeyear & .data$Enrolled.Grade==grade) %>%
                select(.data$Student.ID,.data$Enrolled.Grade,.data$SY),
            by=c("Student.ID")
  ) %>% elpa.plot.flow(y1,y2,flow.toright=flow.toright,...) + ggplot2::labs(title=paste("Students Who Were Tested in Grade",grade,"in",gradeyear))
}

#' Replace Identifying Information With Randomized Values
#'
#' This function replaces Personally Identifiable Information with randomly ordered numeric identifiers between
#' 1 and the number of distinct values for the identifier in the dataset.
#'
#' If an identifier is repeated in the data set, it is replaced in all occurrences with its intended replacement.
#' The numeric portion of the replacement identifier is padded with zeros to ensure that all identifiers are the
#' same length.\cr\cr
#' If a sample.size is specified, the returned data set will (usually) contain more than sample.size rows; however,
#' there will only be sample.size distinct values for the idvar in the resulting dataset.
#'
#' @param ds A dataframe or tibble.
#' @param idvar The variable to replace
#' @param prefix (Optional) A prefix to place in front of the identifier, default is two random lowercase letters.
#' @param sample.size (Optional) If supplied, return a dataset with only N levels of the idvar included
#' @return In final form, a dataframe in the same format with the contents of the idvar column replaced.
#' @export
helper.deidentify<-function(ds,idvar=.data$Student.ID,prefix=NULL,sample.size=NULL) {
  ids <- ds %>% dplyr::select({{idvar}}) %>% dplyr::distinct()
  if (is.null(sample.size))  sample.size=nrow(ids)
  if (sample.size>nrow(ids)) sample.size=nrow(ids)
  ids <- ids %>% dplyr::slice_sample(n=sample.size)
  if (is.null(prefix)) {
    ids <- ids %>% dplyr::mutate(
      pfx=paste(sample(letters,dplyr::n(),replace=TRUE),sample(letters,dplyr::n(),replace=TRUE),sep=""),
      New.ID=paste(.data$pfx,stringr::str_pad(1:dplyr::n(),stringr::str_length(dplyr::n()),pad="0"),sep='')
    ) %>% select(-.data$pfx)
  }
  else {
    ids <- ids %>% dplyr::mutate(
      New.ID=paste(prefix,stringr::str_pad(1:dplyr::n(),stringr::str_length(dplyr::n()),pad="0"),sep='')
    )
  }

  dplyr::inner_join(ds,ids) %>% dplyr::mutate( {{idvar}}:=.data$New.ID) %>% dplyr::select(-.data$New.ID)
}

#' Build a sample data set to include with the package
#'
#' This function is used to generate the "elpadata" dataset that will be incldued with the
#' completed package.
#'
#' @seealso readelpascores, helper.deidentify
#' @note This function depends on files that are located on the developer's system, and are not
#' included in the package.
#' @export
helper.buildelpadata<-function() {
  dplyr::bind_rows(
    utils::read.csv(file="C:/Users/acrosby/Documents/ELPA2021/elpaspring2020.csv") %>%
      dplyr::select(
        Student.ID=.data$StudentID,
        Enrolled.Grade=.data$EnrolledGrade,
        School=.data$EnrolledSchool,
        Ethnicity=.data$Ethnicity,
        Scale.Score=.data$ELPA21SummativeScaleScore,
        Performance.Level=.data$ELPA21SummativePerformanceLevel
        )%>%
      dplyr::mutate(SY='2020')%>%
      helper.cleandata(),
    readelpascores("C:/Users/acrosby/Downloads/ELPA2021/")%>%
      helper.reducedata(SY='2021') %>%
      helper.cleandata(),
    readelpascores("C:/Users/acrosby/Downloads/ELPASpring2022/") %>%
      helper.reducedata(SY='2022') %>%
      helper.cleandata()
  ) %>%
    helper.deidentify(idvar=.data$School,prefix="School_") %>%
    helper.deidentify(idvar=.data$Student.ID)
}

