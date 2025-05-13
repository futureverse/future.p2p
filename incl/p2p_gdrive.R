library(future.p2p)

## Authenticate with Google Drive (preset or via web browser)
googledrive::drive_auth()


## -------------------------------------------
## User 'alice'
## -------------------------------------------
if (user == "alice") {
  ## Create future
  a <- 42
  f <- future({ 2 * a }, lazy = TRUE)
  
  ## Push it to P2P Google Drive
  res <- push_future(f)
  print(res)
  
  ## Wait for results to appear og P2P Google Drive
  f <- get_result(f)
  print(f)
  
  value(f)
}


## -------------------------------------------
## User 'bob'
## -------------------------------------------
if (user == "bob") {
  ## Pop a future from P2P Google Drive
  f <- pop_future()
  print(f)

  ## Process future
  f <- resolve(f)

  ## Push results to P2P Google Drive
  res <- push_result(f)
  print(res)
}
