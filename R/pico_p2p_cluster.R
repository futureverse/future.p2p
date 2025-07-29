#' Launches a P2P cluster
#'
#' @inheritParams pico_pipe
#'
#' @param cluster (character string) The name of the p2p cluster.
#'
#' @param users (character vector) Names of Pico users who should have access,
#' in addition to the owner.
#'
#' @param name The name of the cluster owner as publicized on the P2P cluster.
#'
#' @param duration Duration (in seconds) to offer this cluster.
#'
#' @examplesIf interactive()
#' pico_p2p_cluster()
#'
#' @importFrom future resolve plan sequential
#' @export
pico_p2p_cluster <- function(cluster = "mycluster", users = character(0L), name = p2p_name(), host = "pipe.pico.sh", ssh_args = NULL, duration = 14*24*60*60) {
  parts <- strsplit(cluster, split = "/", fixed = TRUE)[[1]]
  okay <- FALSE
  if (length(parts) == 1L) {
    okay <- TRUE
    cluster_name <- cluster
    cluster_owner <- pico_username()
    cluster <- sprintf("%s/%s", cluster_owner, cluster_name)
  } else if (length(parts) == 2L) {
    cluster_owner <- parts[1]
    if (cluster_owner == pico_username()) {
      okay <- TRUE
      cluster_name <- parts[2]
    }
  }
  if (!okay) {
    stop(sprintf("Argument 'cluster' must be of format '{owner}/{name}' or '{name}' where '{owner}' is your Pico username (%s): %s", sQuote(cluster), sQuote(pico_username())))
  }

  stopifnot(
    is.character(users), !anyNA(users),
    !any(grepl("[[:blank:]]", users))
  )

  users <- unlist(strsplit(users, split = ",", fixed = TRUE))
  users <- c(cluster_owner, users)
  users <- unique(users)

  clusters <- pico_hosted_clusters(host = host, ssh_args = ssh_args)
  if (cluster_name %in% clusters$name) {
    stop(sprintf("P2P cluster is already running: %s", sQuote(cluster)))
  }

  now <- pico_time()

  expires <- pico_time(delta = duration)
  duration <- difftime(duration, 0)

  info("Launch p2p cluster %s for %d users (%s) until %s (%s)", sQuote(cluster), length(users), commaq(users), format(Sys.time() + duration), format(duration))
  topic <- sprintf("%s/future.p2p", cluster_name)
  args <- c("-r")
  if (length(users) > 0) {
    args <- c(args, "-a", paste(users, collapse = ","))
  }
  p <- pico_pipe(topic, args = args, user = name, host = host, ssh_args = ssh_args)
  p$users <- users
  
  repeat {
    if (Sys.time() > expires) {
      info("time is out")
      break
    }
    
    info("hello")
    m <- pico_hello(p, type = "cluster", expires = expires)

    info("access list")
    data <- data.frame(
      owner = p$user,
      nusers = length(p$users),
      users = paste(p$users, collapse = ";"),
      expires = expires
    )
    m <- pico_send_message_dataframe(p, data)

    lapse_time <- pico_time(delta = 2 * 60)
    while (Sys.time() < lapse_time) {
      msg <- pico_receive_message(p, n = 1L)
      if (length(msg) > 0) {
        message(msg)
      } else {
        Sys.sleep(1.0)
      }
    }
  } ## repeat()
 
  info("shutting down cluster ...")
  pico_terminate(p)
  info("bye")
} ## pico_p2p_cluster()


## Expose function on the CLI
cli_fcn(pico_p2p_cluster) <- c("--(cluster)=(.*)", "--(name)=(.*)", "--(users)=(.*)", "--(host)=(.*)", "--(ssh_args)=(.*)", "--(duration)=([[:digit:]]+)")


info <- function(fmtstr, ..., time = Sys.time(), timefmt = "%T", from = c("worker", "client")) {
  from <- match.arg(from)
  msg <- sprintf(fmtstr, ...)
  msg <- sprintf("%s [%s] %s", format(time, format = timefmt), from, msg)
  message(msg)
}

