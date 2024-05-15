#' Add this in example R markdown file
#' MONI-PAM data from Hyytiälä forest station in Finland
#'
#' This dataset contains the MONI-PAM data measured in Hyytiälä forest station in Finland from 2014 to 2015 (2014-2015 folder) and from 2016 to 2017 (2016-2017 folder).All the data are origninal MONI-PAM data with extension '.PAM' data type.
#'
#' @format  A data table with XX rows and XX variables:
#'  \describe{
#'    \item{Date}{Date}
#'    \item{SEASON}{observation period}
#'    \item{F_}{F' measured in MONIPAM}
#'    \item{Fm_}{Fm' measured in MONIPAM}
#'  }
#' @source MONIPAM data recorded in Hyytiala station in Finland as the example
#'
#' @examples
#' # check the data folder location measured in 2014-2015 observation season
#' data20142015.path<-system.file('extdata/2014-2015',package='processMONIPAM')
#' # list all the files inside 2014-2015 folder
#' list.files(data20142015.path)
#'
#' # check the data folder location measured in 2016-2017 observation season
#' data20162017.path<-system.file('extdata/2016-2017',package='processMONIPAM')
#' # list all the files inside 2016-2017 folder
#' list.files(data20162017.path)
#'
#' #MONI-PAM data can be checked by using basic R functions, for example
#' filename<-list.files(data20162017.path,full.names=T)
#' #check first MONI-PAM file inside 2016-2017 folder
#' test1<-read.delim(filename[1])
#'
#' @note example code provided here is only used for checking the data example
#' provided for this R package. We recommend to use [read.MONIPAM] function in
#' this R package to manage your MONIPAM data.
#' @export
