#' Launches a Google Drive P2P Worker
#'
#' @examplesIf interactive()
#' googledrive::drive_auth()
#' google_drive_p2p_worker()
#'
#' @importFrom future resolve plan sequential
#' @export
google_drive_p2p_worker <- function() {
  with(plan(sequential), local = TRUE)
  
  while (TRUE) {
    message("Wait for future ...")
    
    ## Pop a future from P2P Google Drive
    f <- pop_future()
    message(sprintf("Future UUID: %s", paste(f[["uuid"]], collapse = "-")))

    message("Process future ...")
    ## Process future
    f <- resolve(f)
  
    message("Push results ...")
    ## Push results to P2P Google Drive
    res <- push_result(f)
    print(res)
  }
} ## google_drive_p2p_worker()
