# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse


## Create P2P account (once)

Create a [pico.sh] account by calling:

```sh
$ ssh pico.sh
```

and then choose a username. That's it!


## Setup P2P network

Pico.sh user 'alice' sets up a P2P cluster that pico.sh users 'bob',
'carol', and 'diana' have access to;

```r
$ ssh pipe.pico.sh pipe p2p -a bob,carol,diana
```


## Parallelize via friends P2P Network

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

Launch a P2P worker that will take on tasks;

```r
$ Rscript -e "future.p2p::pico_p2p_worker(channel = 'alice/p2p')"
```

Launch more to offer more compute power.



[pico.sh]: https://pico.sh/
