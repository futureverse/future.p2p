# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse


## Create P2P account (once)

Create a [pico.sh] account by calling:

```sh
$ ssh pico.sh
```

and then choose a username. That's it! This will add your public SSH
key to the pico.sh server, which is then used to identify you in all
future interactions. If you have multiple computers, you can add your
public SSH keys for those as well. This way, you can use your pico.sh
account from multiple computer systems, which can be handy if you want
to setup parallel workers on one system and harness their compute
power from another.


## Setup P2P network

Pico.sh user 'alice' sets up a P2P cluster that pico.sh users 'bob',
'carol', and 'diana' have access to;

```r
$ ssh pipe.pico.sh pipe p2p -a bob,carol,diana
```

This will allow 'bob', 'carol', 'diana' and 'alice' themselves to
connect to the P2P cluster named `alice/p2p`. As long as 'alice'
maintains the above SSH connect, this cluster is available to all of
the listed members.


## Parallelize via friends P2P Network

In order to distribute R tasks on a P2P cluster, the current machine
must have:

1. SSH access to pipe.pico.sh, and
2. `wormhole` installed and support it


```r
library(future)

## Resolve future via your friends' P2P cluster
plan(future.p2p::pico_p2p, channel = 'alice/p2p')

## Create future
f <- future(Sys.getpid())
  
## Get results
v <- value(f)
print(v)
```


## Share your compute power with your friends

In order to share your compute resources on a machine, it must have:

1. SSH access to pipe.pico.sh, and
2. `wormhole` installed and support it

To launch a P2P worker, call:

```r
future.p2p::pico_p2p_worker(channel = "alice/p2p")
```

Alternatively, launch it directly from the command line using:

```sh
$ Rscript -e future.p2p::pico_p2p_worker --channel=alice/p2p
```

This will contribute one parallel worker to the p2p cluster. You can
contribute additional ones by repeating the same command one or more
times.



[pico.sh]: https://pico.sh/
