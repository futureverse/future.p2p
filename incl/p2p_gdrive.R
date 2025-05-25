## -------------------------------------------
## User 'alice'
## -------------------------------------------
if (user == "alice") {
  library(future)

  googledrive::drive_auth()
  
  ## Futures are pushed to P2P Google Drive and 
  ## results are collected from there
  plan(future.p2p::google_drive_p2p, .init = FALSE)

  ## Create future
  a <- 42
  f <- future({ 2 * a })
  
  ## Get results
  v <- value(f)
  print(v)
}


## -------------------------------------------
## Worker process
## -------------------------------------------
if (user  == "bob") {
  ## Authenticate with Google Drive (preset or via web browser)
  googledrive::drive_auth()

  ## Launches a worker that download a future from P2P Google Drive,
  ## processes it, uploads the results, and repeats
  future.p2p::google_drive_p2p_worker()
}
