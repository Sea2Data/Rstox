#' Read pipe separated file with specified columns
#' @noRd
read_psv <- function(file, encoding, col_types){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"),locale=loc, col_types = col_types)
  return(db)
}

#' Parses logbooks (ERS) 
#' @description 
#'  Parses electronic logbooks (ERS) from tabular format delivered by Directorate of Fisheries (FDIR)
#' @details 
#'  The format is a pipe-separated format encoding aggregated ERS records (logbooks).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#' @param file path to file
#' @param encoding encoding for 'file'
#' @return data.table() with logbooks
#' @export
readErsFile <- function(file, encoding="latin1"){
  
  spec_log <- readr::cols(
    RC = readr::col_character(),
    REGM = readr::col_character(),
    STORSTE_LENGDE = readr::col_double(),
    BRUTTOTONNASJE = readr::col_integer(),
    MOTORKRAFT = readr::col_integer(),
    TM1 = readr::col_character(),
    AKTIVITET_KODE = readr::col_character(),
    AKTIVITET = readr::col_character(),
    PUMPET_FRA = readr::col_character(),
    FANGSTAR = readr::col_integer(),
    STARTTIDSPUNKT = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    START_LT = readr::col_double(),
    START_LG = readr::col_double(),
    SONE = readr::col_character(),
    KVOTETYPE_KODE = readr::col_character(),
    KVOTETYPE = readr::col_character(),
    REDSKAP_FAO = readr::col_character(),
    REDSKAP_NS = readr::col_character(),
    REDSKAP = readr::col_character(),
    REDSKAPSSPESIFIKASJON_KODE = readr::col_character(),
    REDSKAPSSPESIFIKASJON = readr::col_character(),
    MASKEVIDDE = readr::col_integer(),
    REDSKAP_PROBLEMER_KODE = readr::col_character(),
    REDSKAP_PROBLEMER = readr::col_character(),
    STOPPTIDSPUNKT = readr::col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    STOPP_LT = readr::col_double(),
    STOPP_LG = readr::col_double(),
    VARIGHET = readr::col_integer(),
    INNSATS = readr::col_number(),
    SILD_BESTAND_KODE = readr::col_character(),
    SILD_BESTAND_NS = readr::col_character(),
    SILD_BESTAND = readr::col_character(),
    HOVEDART_FAO = readr::col_character(),
    HOVEDART_NS = readr::col_character(),
    HOVEDART = readr::col_character(),
    INT_OMR_GML_START = readr::col_character(),
    INT_OMR_NY_START = readr::col_character(),
    INT_OMR_GML_STOPP = readr::col_character(),
    INT_OMR_NY_STOPP = readr::col_character(),
    HAV_DYBDE_START = readr::col_number(),
    HAV_DYBDE_STOPP = readr::col_number(),
    LOKASJON_START = readr::col_character(),
    LOKASJON_STOPP = readr::col_character(),
    TREKK_AVSTAND_METER = readr::col_integer(),
    FANGSTART_FAO = readr::col_character(),
    FANGSTART_NS = readr::col_character(),
    FANGSTART = readr::col_character(),
    RUNDVEKT = readr::col_double()
  )
  names(spec_log$cols) <- c(names(spec_log$cols)[1:2], "ST\u00D8RSTE_LENGDE", names(spec_log$cols)[4:9], "FANGST\u00C5R", names(spec_log$cols)[11:length(spec_log$cols)])
  
  logb <- read_psv(file, encoding, col_types=spec_log)
  
  return(data.table::as.data.table(logb))
}

#' Calculates catch proportions
#' @description
#'  Calculates the proportion of catch that was caught in specified cells,
#'  according to logbook (ERS) records.
#' @details 
#'  A cell is a combination of categorical values, e.g. quarter.
#'  'totalcell' are cells which should have total fraction of 1.
#'  'subcells' are cells which should be assigned a proportion of the total. 
#'  These are defined relative to totalcell.
#'  
#'  E.g: In order to get the fraction of a certain species, caught at each location for a vessel in a year:
#'  calculateCatchProportions(logbook, totalcell=c("REGM", "FANGSTART"), subcell=c("LOKASJON_START")),
#'  specifying that the fractions sum to one over REGM (vessel) and FANGSTART (species), 
#'  and that each LOKASJON_START (location) should be assigned the fraction of the catch caught there.
#' @param logbook data.table with logbook records, as parsed by \code{\link[Rstox]{readErsFile}}
#' @param totalcell vector of strings identifying column names in 'logbook', identifying cells with totals
#' @param subcell vector of strings identifying column names in 'logbook', identifying cells which are to be assigned a proportion
#' @param weight string identifying the column containing weights.
#' @return
#' @export
calculateCatchProportions <- function(logbook, totalcell=c("REGM", "FANGSTART"), subcell=c("LOKASJON_START"), weight="RUNDVEKT", na.rm=F){
  
  if (any(subcell %in% totalcell)){
    stop("Misspesified cells. Do not include subcell in totalcell")
  }
  if (!all(totalcell %in% names(logbook))){
    stop("Not all columns defining totalcell is in logbooks")
  }
  if (!all(subcell %in% names(logbook))){
    stop("Not all columns defining subcell is in logbooks")
  }
  if (any(is.na(logbook[,c(totalcell, subcell)]))){
    stop("NA for some columns specifying cells")
  }
  
  totallist <- list()
  for (cellvar in totalcell){
    totallist[[cellvar]] <- logbook[[cellvar]]
  }
  sublist <- list()
  for (cellvar in c(totalcell, subcell)){
    sublist[[cellvar]] <- logbook[[cellvar]]
  }
  
  aggvar <- list(var=logbook[[weight]])
  names(aggvar) <- weight
  
  totalcells <- aggregate(list(totalweight=logbook[[weight]]), by=totallist, FUN=function(x){sum(x, na.rm=na.rm)})
  subcells <- aggregate(list(subcellweight=logbook[[weight]]), by=sublist, FUN=function(x){sum(x, na.rm=na.rm)})
  
  partitioning <- merge(totalcells, subcells)
  partitioning$fraction <- partitioning$subcellweight / partitioning$totalweight
  partitioning$totalweight <- NULL
  partitioning$subcellweight <- NULL
  
  stopifnot(all(!is.na(partitioning$fraction)))
  
  return(partitioning)
  
}
