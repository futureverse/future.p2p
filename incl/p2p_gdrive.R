library(future.p2p)

## Authenticate with Google Drive (preset or via web browser)
googledrive::drive_auth()


## -------------------------------------------
## User 'alice'
## -------------------------------------------
if (user == "alice") {
  plan(future.p2p::google_drive_p2p, .init = FALSE)

  ## Create future
  a <- 42
  f <- future({ 2 * a }, lazy = TRUE)
  
  ## Push it to P2P Google Drive
  res <- push_future(f)
  print(res)
  
  ## Wait for results to appear og P2P Google Drive
  f <- get_result(f)
  v <- value(f)
  print(v)
}


## -------------------------------------------
## User 'bob'
## -------------------------------------------
if (user == "bob") {
  plan(sequential)

  while (TRUE) {
    message("Wait for future ...")
    
    ## Pop a future from P2P Google Drive
    f <- pop_future()
#    print(f)
    message(sprintf("Future UUID: %s", paste(f[["uuid"]], collapse = "-")))

    message("Process future ...")
    ## Process future
    f <- resolve(f)
  
    message("Push results ...")
    ## Push results to P2P Google Drive
    res <- push_result(f)
    print(res)
  }
}
