#' Read historical report
#' 
#' @details `historical_report_purge()` drops posts with `NA` content. These
#'   may posts with picture content only.
#' @param x Path of file to read. Expected to be historical report from 
#'   Crowdtangle.
#' @param verbose A `logical` value, whether to output progress messages.
#' @param data A `tibble` with parsed historical report.
#' @param ... Arguments passed into `aws.s3::get_object()`, if data is retrieved
#'   from S3.
#' @importFrom readr read_delim cols col_character col_integer
#' @importFrom aws.s3 get_object
#' @importFrom cli cli_progress_step cli_progress_done cli_alert_info
#'   cli_alert_success cli_alert_warning
#' @export
#' @rdname historical_report
#' @examples
#' fname <- "~/Downloads/2023-05-16-04-54-58-EDT-Historical-Report-PartyPages2-2014-01-01--2015-01-01.csv"
#' 
#' tbl <- ct_read_csv(fname)
#' tbl <- historical_report_purge(tbl)
ct_read_csv <- function(x, verbose = TRUE, ...){
  if (length(x) > 1L){
    li <- lapply(x, ct_read_csv, ...)
    data <- do.call(rbind, li)
    return(data)
  } else if (length(x) == 1L){
    if (verbose){
      from <- gsub(
        "^.*-(\\d{4}-\\d{2}-\\d{2})--\\d{4}-\\d{2}-\\d{2}\\.csv$",
        "\\1",
        x
      )
      to <- gsub(
        "^.*-\\d{4}-\\d{2}-\\d{2}--(\\d{4}-\\d{2}-\\d{2})\\.csv$",
        "\\1",
        x
      )
      if (!is.na(as.Date(from)) && !is.na(as.Date(to))){
        cli_alert_info(
          "coverage according to filname: {.val {as.Date(from)}} to {.val {as.Date(to)}}"
        )
      }
    }
    
    if (!file.exists(x)){
      x <- get_object(object = x, ...)
      firstline <- strsplit(rawToChar(x), "\n")[[1]][[1]]
    } else {
      firstline <- readLines(x, n = 1L)
    }
    delim <- if (";" %in% unique(strsplit(firstline, "")[[1]])) ";" else ","
    columns <- strsplit(firstline, split = delim)[[1]]

    if (verbose) cli_progress_step("read input file")
    col_spec <- cols(
      `Facebook Id` = col_character(),
      `Link Text` = col_character(),
      `Followers at Posting` = col_integer(),
      `Sponsor Id` = col_character(), # col unused here, to avoid warning/problem
      `Sponsor Name` = col_character(), # col unused here, to avoid warning/problem
      `Sponsor Category` = col_character(),
      `Video Length` = col_character() # it is a difftime
    )
    
    # The naming of the column "Overperforming Score" has been simplified, but
    # we want the function to work for old data
    overperforming_col <- grep("Overperforming Score", columns, value = TRUE)
    col_spec$cols[[gsub("\u2014", "-", overperforming_col)]] <- col_character()
    if (!"Sponsor Category" %in% columns) col_spec$cols[["Sponsor Category"]] <- NULL
    
    data <- readr::read_delim(
      x,
      delim = delim,
      col_types = col_spec,
      name_repair = function(x) gsub("\u2014", "-", x),
      na = c("", "N/A")
    )
    if (verbose) cli_progress_done()
    if (verbose) cli_alert_info("read historical report with {.val {nrow(data)}} posts")
    
    if ("Created" %in% colnames(data))
      colnames(data) <- gsub("^Created$", "Post Created", colnames(data))
    colnames(data) <- gsub("^Overperforming Score.*$", "Overperforming Score", colnames(data))
    if (all(colnames(data) %in% historical_report_colnames)){
      if (verbose) cli_alert_success("colnames known: TRUE")
    } else {
      if (verbose) cli_alert_success("colnames known: FALSE")
    }
    
    missing <- setdiff(historical_report_colnames, colnames(data))
    if (length(missing) > 0L){
      if (verbose) cli_alert_warning("missing colnames: {paste(missing, collapse = ', ')}")
    }
    
    if (verbose) cli_progress_step("process column 'Video Length'")
    data[["Video Length"]] <- as.difftime(data[["Video Length"]], format = "%H:%M:%S")
    
    if (verbose) cli_progress_step("process column 'Post Created'")
    
    data[["Post Created"]] <- as.POSIXct(data[["Post Created"]])
    data[["date"]] <- as.Date(data[["Post Created"]])
    
    if (verbose) cli_progress_done()
    if (verbose)
      cli_alert_info("coverage in data: {.val {min(data$date)}} to {.val {max(data$date)}}")
    return(data)
  } else {
    stop("something is invalid about the filename")
  }
}

#' @export 
#' @rdname historical_report
historical_report_purge <- function(data, verbose = TRUE){
  
  if (verbose) cli_progress_step("drop posts with `NA` message text")
  na_messages <- which(is.na(data[["Message"]]))
  if (length(na_messages) > 0L){
    data <- data[-na_messages,]
  }
  if (verbose) cli_progress_done()
  if (verbose) cli_alert_info(
    "dropped {.val {length(na_messages)}} posts with `NA` message text (new nrow: {.val {nrow(data)}})"
  )

  if (verbose) cli_progress_step("purge messages from special chars")
  data[["Message"]] <- gsub("\\&", "+", data[["Message"]])
  data[["Message"]] <- gsub("<", "", data[["Message"]])
  data[["Message"]] <- gsub(">", "", data[["Message"]])
  data[["Message"]] <- gsub("[^-0-9a-zA-Z\u00E4\u00F6\u00FC\u00C4\u00D6\u00DC\u00DF\\.,!\\?:;#/ ]+", "", data[["Message"]])
  if (verbose) cli_progress_done()
  
  empty <- which(grepl("^\\s*$", data[["Message"]]))
  if (length(empty) > 0L){
    if (verbose) cli_alert_info(
      "drop {.val {length(empty)}} messages empty after purging special chars (new nrow: {.val {nrow(data)}})"
    )
    data <- data[-empty,]
  }
  
  if (verbose) cli_alert_info("return table with {.val {nrow(data)}} rows")
  
  data
}

historical_report_colnames <- c(
  "Page Name",
  "User Name",
  "Facebook Id",
  "Page Category",
  "Page Admin Top Country",
  "Page Description",
  "Page Created",
  "Likes at Posting",
  "Followers at Posting",
  "Post Created",
  "Post Created Date",
  "Post Created Time",
  "Type",
  "Total Interactions",
  "Likes",
  "Comments",
  "Shares",
  "Love",
  "Wow",
  "Haha",
  "Sad",
  "Angry",
  "Care",
  "Video Share Status",
  "Is Video Owner?",
  "Post Views",
  "Total Views",
  "Total Views For All Crossposts",
  "Video Length",
  "URL",
  "Message",
  "Link",
  "Final Link",
  "Image Text",
  "Link Text",
  "Description",
  "Sponsor Id",
  "Sponsor Name",
  "Sponsor Category",
  "Overperforming Score"
)
