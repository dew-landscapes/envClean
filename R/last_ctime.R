
#' Return the object with the latest `ctime` attribute
#' 
#'
#' 
#' @param prefix 
#' 
#' @return
#' @export
#'
#' @examples
#'
  last_ctime <- function(prefix = "bio_") {
  
    ls(pattern = prefix
                 , envir = .GlobalEnv
                 ) %>%
      tibble::enframe(name = NULL
                      , value = "name"
                      ) %>%
      dplyr::mutate(ctime = purrr::map(name
                                     , ~attr(get(.), "ctime")
                                     )
                    ) %>%
      tidyr::unnest(cols = c(ctime)) %>%
      dplyr::arrange(ctime) %>%
      dplyr::pull(name) %>%
      tail(1)
  
  }
  