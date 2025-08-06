# future.p2p - A Peer-to-Peer Compute Cluster via Futureverse


## Prerequisites

In order to join a future P2P cluster, you must:

1. have a [pico.sh] account, and

2. have SSH access to pipe.pico.sh


## Create a P2P account (once; all users)

To create a [pico.sh] account, call:

```sh
$ ssh pico.sh
```

and choose a username and click <kbd>ENTER</kbd>.  This will add your
public SSH key to the pico.sh servers, which is then used to identify
you in all future interactions. Next, verify SSH access to
`pipe.pico.sh`;

```sh
$ ssh pipe.pico.sh
```

_Important_: Make sure to accept the SSH fingerprint, otherwise you
will not be able to connect to the P2P cluster from R.


## Launch a P2P cluster (one of the users)

Pico.sh user 'alice' sets up a P2P cluster named `alice/p2p` that
pico.sh users 'bob', 'carol', and 'diana' have access to;

```r
future.p2p::pico_p2p_cluster(cluster = "alice/p2p", users = c("bob", "carol", "diana"))
```

Alternatively, launch it directly from the command line using:

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_cluster --cluster=alice/p2p --users=bob,carol,diana
```

After this, 'bob', 'carol', 'diana', and 'alice' can use and share
their R compute resources.  This 'alice/p2p' cluster is available as
long as the above R function of 'alice' is running.

A future P2P cluster can be launched from anywhere in the world, and
it does not have to on a machine where 'alice' runs their own R
analysis.  Note that the `alice/` prefix is reserved for pico user
`alice`.  This is why user `bob` can _not_ create a cluster named
`alice/pop` - only one called `bob/{name}`.


## Parallelize via P2P cluster (any user)

Any user with access to the 'alice/p2p' cluster can harness the
collective compute power. For example,

```r
library(future)

## Resolve future via your friends' P2P cluster
plan(future.p2p::pico_p2p, cluster = "alice/p2p")

## Create future
f <- future(Sys.getpid())
  
## Get results
v <- value(f)
print(v)
```

_Comment:_ The first time you run this, you might find that
the [wormhole-william] executable is installed.


## Share your compute power with your friends (any user)

To contribute your R compute power to the `alice/p2p` cluster, launch
a P2P worker as:

```r
future.p2p::pico_p2p_worker(cluster = "alice/p2p")
```

Alternatively, launch it directly from the command line using:

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_worker --cluster=alice/p2p
```

This will contribute one parallel worker to the p2p cluster. You can
contribute additional ones by repeating the same command one or more
times.

_Comment:_ The first time you run this, you might find that
the [wormhole-william] executable is installed.


## Appendix

### Connecting to the same pico.sh account from different machines

If you have multiple computers, you can add your public SSH keys for
those as well by logging in again by calling `ssh pico.sh`. Then go to
the `pubkeys` menu, where you have options to add additional public
SSH keys of yours. This way, you can use your pico.sh account from
multiple computer systems, which can be handy if you want to setup
parallel workers on one system and harness their compute power from
another.


### Set up a worker to connect to pico.sh via a jumphost

```r
> future.p2p::pico_p2p_worker(cluster = "alice/p2p", ssh_args = c("-J", "somehost"))
```

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_worker --cluster=alice/p2p --ssh_args="-J somehost"
```

### Troubleshoot Wormhole

If you are behind a firewall with a proxy, wormhole might fail to
establish an outbound connection. For example, if you try:

```r
> library(future.p2p)
> system2(find_wormhole(), args = c("send", "--text", "hello"))
```

it might stall forever.  If that happens, press <kbd>Ctrl-C</kbd> to
interrupt and retry by disabling the proxy settings using:

```sh
> library(future.p2p)
> Sys.unsetenv("http_proxy")
> system2(find_wormhole(), args = c("send", "--text", "hello"))
On the other computer, please run: wormhole receive (or wormhole-william recv)                                                       
Wormhole code is: 53-visitor-physique
```

If the latter works for you, launch R by unsetting environment
variable `http_proxy`, e.g.

```sh
{bob}$ http_proxy="" Rscript -e future.p2p::pico_p2p_worker --cluster=alice/p2p
```


[pico.sh]: https://pico.sh/
[Magic-Wormhole]: https://magic-wormhole.readthedocs.io/en/latest/
[wormhole-william]: https://github.com/psanford/wormhole-william
