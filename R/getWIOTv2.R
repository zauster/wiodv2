#' Download a WIOT from the second version of the WIOD project
#'
#' This function allows you to download a World Input-Output Table
#' (WIOT) from the second generation of the WIOD project
#' (www.wiod.org) for a given year. Additionally you can specify among
#' three possible formats (wide, long, list).
#' @param period the year of the WIOT
#' @param format a character string to specify the needed format,
#'     either "wide" (default), "long" or "list". See Details for more
#'     information
#' @param as.DT a logical value: Should the "wide" or "long" formats
#'     be returned as data.tables? Default is TRUE.
#' @param version for (possible further) revisions. Currently not
#'     used.
#' @details If format = "wide" is chosen, the data is returned in a
#'     wide data.table. This format is the format you know from the
#'     excel-sheets (available from www.wiod.org). The format option
#'     "long" returns a reshaped/melted data.table. Finally, format =
#'     "list" returns a list of four matrices/vectors: 1) matrix Z,
#'     the square (2464 x 2464) matrix of intermediates, 2) matrix F,
#'     the final demand block (2464 x 220), 3) vector x, gross output
#'     per country sector, 4) vector v, value added per country
#'     sector, 5) matrix va.block, the valuation block (i.e. taxes,
#'     adjustments for exports, international trade margins, etc.) and
#'     6) 1-element vector year, specifying the year of the WIOT.
#' @return the function returns either a data.table (or data.frame
#'     when as.DT = FALSE) for the chosen formats "wide" or "long", or
#'     a list for format "list"
#' @author Oliver Reiter
#' @seealso www.wiod.org
#' @references ===> xxx TO BE included xxx <===
#' @keywords data, input-output, wiod
#' @export
#' @import data.table
#' @examples
#' wiot.2005 <- getWIOTv2(period = 2005, format = "wide")
#'
#' ## wiod.list <- list()
#' ##for(this.year in 2000:2014) {
#' ##   wiod.list[[as.character(this.year)]] <- getWIOTv2(period = this.year,
#' ##                                                     format = "list")
#' ##}
getWIOTv2 <- function(period = 2010,
                      format = "wide",  # or "long" or "list"
                      version = "October16", # default and only option
                      as.DT = TRUE) {        # as data.table?

    ##
    ## sanity checks
    ##
    if(period < 2000 | period > 2014) {
        stop(" -> WIOTs are available for the years 2000 till 2014!")
    }
    
    if(!(format %in% c("wide", "long", "list"))) {
        stop(" -> The only possible format options are 'wide', 'long' or 'list'!")
    }
    
    if(version != "October16") {
        warning("No other version available. This option is without effect.")
    }
    
    if(!is.logical(as.DT)) {
        stop(" -> Please specify either TRUE or FALSE for the as.DT-option.")
    }

    if((as.DT == FALSE) & (format == "list")) {
        warning("For format = 'list', as.DT does not have an effect.")
    }

    ## WIOT2000_October16_ROW_list.rds
    base.url <- "http://wiiw.ac.at/files/staff-content/reiter/"

    res <- readRDS(file = gzcon(url(paste0(base.url, "WIOT", period, "_",
                                           version, "_ROW",
                                           ## "_", format,
                                           ifelse(format == "wide", "",
                                                  paste0("_", format)),
                                           ".rds"))))

    ## load(file = url(paste0(base.url, "WIOT", period, "_",
    ##                        version, "_ROW",
    ##                        ifelse(format == "wide", "",
    ##                               paste0("_", format)),
    ##                        ".RData")))
    ## if(format == "wide") {
    ##     res <- wiot
    ## } else if(format == "long") {
    ##     res <- wiot.long
    ## } else if(format == "list") {
    ##     res <- wiot.list
    ## }
    

    if(format %in% c("wide", "long") & !as.DT) {
        ## print(format)
        ## print(as.DT)
        res <- as.data.frame(res)
    }

    return(res)
}
