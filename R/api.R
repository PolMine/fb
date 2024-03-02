#' Retrieve posts from CrowdTangle API
#' 
#' @param from Begin date, format YYYY-MM-DD.
#' @param to End date, format YYYY-MM-DD.
#' @param token Credentials/token.
#' @param ids Id of saved post lists of the dashboard associated with the token.
#' @param sort_by Defaults to "date".
#' @param count An integer value, max 100.
#' @importFrom httr GET content
#' @importFrom cli cli_progress_step cli_progress_done cli_alert_success
#'   cli_alert_danger
#' @export
#' @rdname ct_api
#' @examples
#' token <- readLines("~/.crowdtangle/token")
#' y <- ct_get(
#'   token = token,
#'   ids = 1771322,
#'   from = "2021-01-01",
#'   to = "2021-01-31"
#' )
ct_get <- function(token, ids, sort_by = "date", count = 100, from, to){
  
  # higher value avoids too many requests HTTP error; 100 is max!
  if (count > 100) warning("Argument `count` may not be higher than 100.")
  if (as.Date(from) > as.Date(to))
    warning("start date (argument `from`) is higher than end date (argument `to`)")
  
  cli::cli_progress_step("retrieve result set #1")
  page_raw <- GET(
    "https://api.crowdtangle.com/posts",
    query = list(
      token = token,
      listIds = ids,
      sortBy = sort_by,
      count = 100, 
      startDate = from,
      endDate = to
    )
  )
  cli::cli_progress_done()
  
  page <- content(page_raw)
  tab <- list()
  i <- 2L
  while(i > 0L){
    if (page[[1]] == 200){
      cli::cli_alert_success("HTTP Status: OK")
    } else if (page[[1]] == 429){
      cli::cli_alert_danger("HTTP Status: 429/Too Many Requests (rate limit met)")
    } else {
      cli::cli_alert_danger("unknown HTTP Status")
    }
    
    tablist <- lapply(
      seq_along(page[[2]][[1]]),
      function(i){
        if ("message" %in% names(x)){
          x <- page[[2]][[1]][[i]]
          if (is.null(x[["message"]])) return(NULL)
          df <- data.frame(
            platformId = x[["platformId"]],
            date = x[["date"]],
            updated = x[["updated"]],
            message = x[["message"]],
            account_id = x[["account"]][["id"]],
            account_name = x[["account"]][["name"]],
            account_handle = x[["account"]][["handle"]],
            account_platform_id = if (is.null(x[["account"]][["platformId"]]))
              NA
            else
              x[["account"]][["platformId"]]
          )
        } else {
          return(NULL)
        }
      }
    )
    tab[[i]] <- do.call(rbind, tablist)
      
    if (length(page[[2]]) > 1L){
      if ("nextPage" %in% names(page[[2]][[2]])){
        next_page <- page[[2]][[2]][["nextPage"]] 
        cli::cli_progress_step("retrieve result set {.val {i}}")
        page <- GET(next_page) |> content()
        cli::cli_progress_done()
        i <- i + 1L
        Sys.sleep(2)
      } else {
        break
      }
    } else {
      i <- -1L
    }
  }
  
  do.call(rbind, tab)
}


#' @details `ct_lists()` - get lists saved at Dashboard.
#' @export
#' @rdname ct_api
#' @examples
#' token <- readLines("~/.crowdtangle/token")
#' ct_lists(token)
ct_lists <- function(token){
  page_raw <- GET(
    "https://api.crowdtangle.com/lists",
    query = list(token = token)
  )
  do.call(rbind, lapply(content(page_raw)[["result"]][["lists"]], data.frame))
}


#' @details `ct_list_accounts()` - retrieve the accounts for a given list, see
#' \url{https://github.com/CrowdTangle/API/wiki/List-Accounts}.
#' @param list_id ID of the list for which to get accounts.
#' @param offset Start from this offset position.
#' @export
#' @rdname ct_api
#' @importFrom dplyr bind_rows
#' @examples
#' token <- readLines("~/.crowdtangle/token")
#' ct_list_accounts(token, list_id = 1771322)
ct_list_accounts <- function(token, list_id, offset = 0L){
  response_raw <- GET(
    sprintf("https://api.crowdtangle.com/lists/%d/accounts", list_id),
    query = list(
      token = token,
      count = 100L,
      offset = offset
    )
  )
  
  response <- content(response_raw)
  # We use `dplyr::bind_rows()` not do.call(rbind, ...) because the column
  # 'pageAdminTopCountry' may be missing 
  retval <- bind_rows(lapply(response[["result"]][["accounts"]], data.frame))
  
  while (!is.null(response[["result"]][["pagination"]][["nextPage"]])){
    next_page <- response[["result"]][["pagination"]][["nextPage"]]
    offset_next <- as.integer(gsub("^.*?offset=(\\d+)$", "\\1", next_page))
    cli::cli_alert_info("get tokens starting at offset: {.val {offset_next}}")
    response <- content(GET(next_page))
    retval <- bind_rows(
      list(
        retval,
        bind_rows(lapply(response[["result"]][["accounts"]], data.frame))
      )
    )
  }
  
  retval
}
