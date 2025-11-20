#' Hosts a shared P2P cluster
#'
#' @inheritParams pico_pipe
#'
#' @param cluster (character string) The name of the p2p cluster.
#'
#' @param users (character vector) Names of Pico users who should have access,
#' in addition to the owner.
#' The default is a personal cluster that only you have access to.
#'
#' @param duration Duration (in seconds) to offer this cluster.
#'
#' @examplesIf interactive()
#' # Connect to personal P2P cluster, which is automatically launched
#' host_cluster(users = c("bob", "carol"))
#'
#' @importFrom future resolve plan sequential
#' @export
host_cluster <- function(cluster = p2p_cluster_name(users, host = host, ssh_args = ssh_args), users = character(0L), host = "pipe.pico.sh", ssh_args = NULL, duration = 14*24*60*60) {
  stopifnot(length(cluster) == 1L, is.character(cluster), !is.na(cluster), nzchar(cluster))
  
  parts <- strsplit(cluster, split = "/", fixed = TRUE)[[1]]
  okay <- FALSE
  if (length(parts) == 1L) {
    okay <- TRUE
    cluster_name <- cluster
    cluster_owner <- pico_username(host = host, ssh_args = ssh_args)
    cluster <- sprintf("%s/%s", cluster_owner, cluster_name)
  } else if (length(parts) == 2L) {
    cluster_owner <- parts[1]
    if (cluster_owner == pico_username(host = host, ssh_args = ssh_args)) {
      okay <- TRUE
      cluster_name <- parts[2]
    }
  }
  if (!okay) {
    stop(sprintf("Argument 'cluster' must be of format '{owner}/{name}' or '{name}' where '{owner}' is your Pico username (%s): %s", sQuote(cluster), sQuote(pico_username(host = host, ssh_args = ssh_args))))
  }

  stopifnot(
    is.character(users), !anyNA(users),
    !any(grepl("[[:blank:]]", users))
  )

  users <- unlist(strsplit(users, split = ",", fixed = TRUE))
  users <- c(cluster_owner, users)
  users <- unique(users)

  clusters <- pico_p2p_hosted_clusters(host = host, ssh_args = ssh_args)
  if (cluster_name %in% clusters$name) {
    msg <- sprintf("P2P cluster is already running: %s", sQuote(cluster))
    msg <- sprintf("%s (see 'ssh pipe.pico.sh ls' for details)", msg)
    stop(FutureError(msg))
  }

  now <- pico_p2p_time()

  expires <- pico_p2p_time(delta = duration)
  duration <- difftime2(duration, 0)

  info("Launch p2p cluster %s for %d users (%s) until %s (%s)", sQuote(cluster), length(users), commaq(users), format(Sys.time() + duration), format(duration))
  topic <- sprintf("%s/future.p2p", cluster_name)
  args <- c("-r")
  if (length(users) > 0) {
    args <- c(args, "-a", paste(users, collapse = ","))
  }
  name <- p2p_client_id()
  p <- pico_pipe(topic, args = args, user = name, host = host, ssh_args = ssh_args)
  p$users <- users
  
  repeat {
    if (Sys.time() > expires) {
      info("time is out")
      break
    }
    
    info("hello")
    m <- pico_p2p_hello(p, type = "cluster", expires = expires)

    info("access list")
    data <- data.frame(
      owner = p$user,
      nusers = length(p$users),
      users = paste(p$users, collapse = ";"),
      expires = expires
    )
    m <- pico_send_message_dataframe(p, data)

    lapse_time <- pico_p2p_time(delta = 2 * 60)
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
} ## host_cluster()


## Expose function on the CLI
cli_fcn(host_cluster) <- c("--(cluster)=(.*)", "--(users)=(.*)", "--(host)=(.*)", "--(ssh_args)=(.*)", "--(duration)=([[:digit:]]+)")
