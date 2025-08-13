# future.p2p - A Peer-to-Peer Compute Cluster in R <img border="0" src="man/figures/world-p2p-network-three-users.png" width="150px" align="right"/>

_- Share R compute among friends across the world_

<br>
<br>

## TL;DR

```r
library(future)

## Resolve futures via a P2P cluster shared among friends
plan(future.p2p::cluster, cluster = "alice/friends")

## Create future
f <- future(Sys.getpid())
  
## Get results
v <- value(f)
print(v)
```

## ⚠️ Security ⚠️

⚠️ _Important warning: Please note that there is nothing preventing a
user in your P2P cluster from sending malicious R code to your P2P
worker!_

For example, a P2P user may submit a future that erases all files on
the P2P worker or a future that attempts to read non-encrypted secret
files of yours, e.g.

```r
f <- future(system("erase-all-user-files"))
```

and

```r
f <- future(readLines("~/.ssh/id_ed25519"))
```

Because of this, it is important that you only join shared P2P cluster
that you trust, i.e. where you trust all the P2P user and the user who
hosts it such that they do not invite non-trusted or unknown users.

There a mechanisms for launching P2P workers in _sandboxed_
environments. For instance, by running P2P workers in a sandboxed
virtual machine (VM), in a sandboxed Linux container
(e.g. [Apptainer], [Docker] and [Podman]), or via dedicated sandboxing
tools (e.g. [Bubblewrap], [Firejail], and macOS `sandbox-exec`), you
can mitigate some of the risk that malicious code can access the host
machine where your personal data lives.


## Installation

```r
install.packages('future.p2p', repos = c('https://futureverse.r-universe.dev', 'https://cloud.r-project.org'))
```


## Getting started

In order to join a future P2P cluster, you must:

1. have an _SSH key pair_ configured, and

2. have a _[pico.sh] account_.

See the 'Getting Started' vignette for how to set this up, but the
gist for creating an SSH key pair if you already don't have one is to:

```sh
$ ssh-keygen
```

With the key pair create a [pico.sh] account by logging into their
server:

```sh
$ ssh pico.sh
```

Choose your pico.sh username, which will also be your P2P cluster
username, and click <kbd>ENTER</kbd>. Finally, verify SSH access to
`pipe.pico.sh` (sic!);

```sh
$ ssh pipe.pico.sh
```

That's it!


## Set up a shared P2P cluster

Let's assume P2P users 'alice', 'bob', 'carol', and 'diana' decides to
share a P2P cluster and user 'alice' agrees to host it. Hosting a P2P
cluster only means that you control who has access - there's no extra
load added. So, to host, 'alice' calls:

```sh
{alice}$ Rscript -e future.p2p::host_cluster --users=bob,carol,diana --cluster=alice/friends
```

A future P2P cluster can be hosted from anywhere in the world, and it
does not have to on a machine where you run your own R analysis.


## Parallelize via P2P cluster (all users)

Any user with access to the 'alice/friends' cluster can use it. In our
example, this means 'bob', 'carol', 'diana', and 'alice' may use the
P2P cluster at the same time. Just like with any other future backend,
we use `plan()` to specifying that we want to parallelize via the P2P
cluster.

For example,

```r
library(future)
plan(future.p2p::cluster, cluster = "alice/friends")

## Evaluate a R expression via the P2P cluster
f <- future(Sys.getpid())

## Retrieve value
v <- value(f)
print(v)
```

## Share your compute power with your friends (any user)

Without parallel workers, the P2P cluster is useless and will not
process any parallel tasks. This is where the peer-to-peer concept
comes in, where we contribute our idle compute cycles to the cluster
for others to make use of. To contribute your R compute power to the
`alice/friends` cluster, launch a P2P worker as:

```sh
{bob}$ Rscript -e future.p2p::worker --cluster=alice/friends
```

This will contribute one parallel worker to the p2p cluster. You can
contribute additional ones by repeating the same command one or more
times.



## Appendix

### Connecting to the same pico.sh account from different machines

If you have multiple computers, you can add your public SSH keys for
those as well by logging in again by calling `ssh pico.sh`. Then go to
the `pubkeys` menu, where you have options to add additional _public
SSH keys_ of yours. This way, you can use your pico.sh account from
multiple computer systems, which can be handy if you want to set up
parallel workers on one system and harness their compute power from
another.


### Set up a worker to connect to pico.sh via a jumphost

```sh
{bob}$ Rscript -e future.p2p::worker --ssh_args="-J somehost" --cluster=alice/friends
```

### Troubleshoot Wormhole

If you are behind a firewall with a proxy, wormhole might fail to
establish an outbound connection. For example, if you try:

```r
> system2(future.p2p::find_wormhole(), args = c("send", "--text", "hello"))
```

it might stall forever.  If that happens, press <kbd>Ctrl-C</kbd> to
interrupt and retry by disabling the proxy settings using:

```sh
> Sys.unsetenv("http_proxy")
> system2(future.p2p::find_wormhole(), args = c("send", "--text", "hello"))
On the other computer, please run: wormhole receive (or wormhole-william recv)                                                       
Wormhole code is: 53-visitor-physique
```

If the latter works for you, launch R by unsetting environment
variable `http_proxy`, e.g.

```sh
{bob}$ http_proxy="" Rscript -e future.p2p::worker --cluster=alice/friends
```


[pico.sh]: https://pico.sh/
[Magic-Wormhole]: https://magic-wormhole.readthedocs.io/en/latest/
[wormhole-william]: https://github.com/psanford/wormhole-william
[Apptainer]: https://apptainer.org/
[Docker]: https://www.docker.com/
[Podman]: https://podman.io/
[Bubblewrap]: https://github.com/containers/bubblewrap
[Firejail]: https://github.com/netblue30/firejail
