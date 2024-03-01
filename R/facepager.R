#' Read Facepager CSV files
#' 
#' @param x Path of a csv file exported from Facepager. Can be a URL.
#' @return A `tibble` with columns "User Name", "URL", "date" and "Message".
#' @export
#' @importFrom tibble as_tibble
#' @importFrom utils read.csv2
fp_read_csv <- function(x){
  if (length(x) > 1L){
    li <- lapply(x, fp_read_csv)
    data <- do.call(rbind, li)
    return(data)
  } else if (length(x) == 1L){
    doc <- read.csv2(x) # readr::read_csv2() mixes up lines
    doc_min <- subset(doc, doc[["object_type"]] == "data")
    doc_min <- subset(doc_min, !grepl("^\\s*$", message))
    doc_min[["query_time"]] <- as.Date(doc_min[["query_time"]])
    doc_min[["date"]] <- as.Date(doc_min[["created_time"]])
    doc_min[["updated_time"]] <- as.Date(doc_min[["updated_time"]])
    doc_min[["Message"]] <- doc_min[["message"]]
    doc_min[["Facebook Id"]] <- gsub("^(\\d+)_.*$", "\\1", doc_min[["object_id"]])
    doc_min[["User Name"]] <- merge(
      doc_min,
      PartyPages::partypages_mapping,
      by.x = "Facebook Id",
      by.y = "facebook_id", 
    )$user_name
    doc_min[["Post Id"]] <- gsub("^\\d+_(\\d+)$", "\\1", doc_min[["object_id"]])
    doc_min[["URL"]] <- sprintf(
      "https://www.facebook.com/%s/posts/%s",
      doc_min[["Facebook Id"]],
      doc_min[["Post Id"]]
    )
    doc_min <- doc_min[, c("User Name", "URL", "date", "Message")]
    retval <- historical_report_purge(doc_min)
    return(as_tibble(retval))
  } else {
    stop("invalid input")
  }
}
