library(future.p2p)

## Authenticate with Google Drive (preset or via web browser)
googledrive::drive_auth()
# googledrive::drive_mkdir("p2p-todo", path = "~/")
# googledrive::drive_mkdir("p2p-done", path = "~/")
# googledrive::drive_mkdir("p2p-checked-out", path = "~/")


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
  
  ## Wait for it to be process by a peer
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
  
  r <- result(f)
  print(r)
  
  res <- push_result(f)
  print(res)
}