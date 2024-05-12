#' @importFrom tibble as_tibble
.get_post_data <- function(i, content){
  x <- content[[2]][[1]][[i]]
  if ("message" %in% names(x)){
    if (is.null(x[["message"]])) return(NULL)
    df <- data.frame(
      platformId = x[["platformId"]],
      post_url = x[["postUrl"]],
      score = x[["score"]],
      subcriber_count = x[["subscriberCount"]],
      date = x[["date"]],
      updated = x[["updated"]],
      message = x[["message"]],
      account_id = x[["account"]][["id"]],
      account_name = x[["account"]][["name"]],
      account_handle = if (is.null(x[["account"]][["handle"]]))
        NA
      else
        x[["account"]][["handle"]],
      account_platform_id = if (is.null(x[["account"]][["platformId"]]))
        NA
      else
        x[["account"]][["platformId"]]
    )
    
    if (!is.null(x[["statistics"]])){
      if (!is.null(x[["statistics"]][["actual"]])){
        stats_actual <- data.frame(x[["statistics"]][["actual"]])
        colnames(stats_actual) <- paste(
          "stats_actual",
          colnames(stats_actual),
          sep = "_"
        )
        df <- cbind(df, stats_actual)
      }
      
      if (!is.null(x[["statistics"]][["actual"]])){
        stats_expected <- data.frame(x[["statistics"]][["expected"]])
        colnames(stats_expected) <- paste(
          "statistics_expected",
          colnames(stats_expected),
          sep = "_"
        )
        df <- cbind(df, stats_expected)
      }
    }

  } else {
    return(NULL)
  }
  as_tibble(df)
}

#' Retrieve posts from CrowdTangle API
#' 
#' @param from Begin date, format YYYY-MM-DD.
#' @param to End date, format YYYY-MM-DD.
#' @param token Credentials/token.
#' @param ids Id of saved post lists of the dashboard associated with the token.
#' @param sort_by Defaults to "date".
#' @param count An integer value, max 100.
#' @param sleep Passed into `Sys.sleep()` between API requests to conform to 
#'   rate limit. Defaults to 10 as there is a maximum of 6 calls per minute.
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
#'   to = "2021-01-10"
#' )
ct_get <- function(token, ids, sort_by = "date", count = 100, from, to, sleep = 10){
  
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
      cli::cli_alert_danger(
        "HTTP Status: 429/Too Many Requests (rate limit met)"
      )
    } else {
      cli::cli_alert_danger("unknown HTTP Status")
    }
    
    tablist <- lapply(seq_along(page[[2]][[1]]), .get_post_data, content = page)
    
    tab[[i]] <- do.call(rbind, tablist)
      
    if (length(page[[2]]) > 1L){
      if ("nextPage" %in% names(page[[2]][[2]])){
        next_page <- page[[2]][[2]][["nextPage"]] 
        cli::cli_progress_step("retrieve result set {.val {i}}")
        page <- GET(next_page) |> content()
        cli::cli_progress_done()
        i <- i + 1L
        Sys.sleep(sleep)
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

#' @param id The ID of the post on its platform which corresponds to the
#'   platformId property of the Post object. This is provided as a path variable
#'   in the URL. See examples for examples.
#' @details `ct_post()` - get specific post, see
#'   \url{https://github.com/CrowdTangle/API/wiki/Posts#get-postid}.
#' @export
#' @rdname ct_api
#' @examples
#' token <- readLines("~/.crowdtangle/token")
#' ct_post(token = token, id = "100043924819479_1001812481292903") # is available
#' ct_post(token = token, id = "100044285296893_951352766350891") # is gone 
#' 
#' \dontrun{
#' ct_post(
#'   token = token, 
#'   id = c(
#'     "100044285296893_951352766350891",
#'     "100050313077113_959694992384261"
#'    )
#' )
#' }
#' @importFrom cli cli_alert_info
ct_post <- function(token, id, sleep = 10){
  if (length(id) == 1L){
    cli_alert_info("retrieve post data for platformId {.val {id[[1]]}}")
    request <- GET(
      sprintf("https://api.crowdtangle.com/post/%s", id),
      query = list(token = token)
    )
    retval <- .get_post_data(i = 1, content = content(request))
  } else {
    post_data_list <- lapply(
      seq_along(id),
      function(i){
        post <- ct_post(token = token, id = id[[i]])
        if (i < length(id)) Sys.sleep(time = sleep)
        post
      }
    )
    retval <- bind_rows(post_data_list)
  }
  retval
}
