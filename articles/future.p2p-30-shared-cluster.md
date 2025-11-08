# future.p2p: A Shared P2P Cluster among Friends

You and your friends can share your R compute powers with each other,
regardless where in the world you are located. To do this, one of you
will host a “friends” P2P cluster by specifying which users should have
access. After this, each of you can contribute as many workers as you
like, and each of you can distribute your R processing to this P2P
cluster.

This document gives instructions on how to set up a *shared* P2P
cluster. The process is almost as simple as setting up a personal P2P
cluster. If this is the first time using
**[future.p2p](https://future.p2p.futureverse.org/)**, I recommend
starting out with a *personal* P2P cluster just to get a feel for what
is involved. After that, setting one up among friends, is almost as
easy.

## Hosting a P2P cluster

Lets assume that P2P users ‘alice’, ‘bob’, ‘carol’, and ‘diana’ wish
share a P2P cluster and user ‘alice’ agrees to host it. They can do this
by calling:

``` sh
{alice}$ Rscript -e future.p2p::host_cluster --users=bob,carol,diana --cluster=alice/friends
```

Hosting a P2P cluster only means that you control who has access -
nothing else. For example, in our example, there will be *no* extra
traffic going through the computer of ‘alice’.

## Adding P2P workers

After this, ‘bob’, ‘carol’, ‘diana’, and ‘alice’ have equal access to
the P2P cluster ‘alice/friends’.

Those who have access can contribute as many workers as they like. To
launch a P2P worker, call:[^1][^2]

``` sh
{bob}$ Rscript -e future.p2p::worker --cluster=alice/friends
```

Anyone can add more workers more later on.

## Using P2P cluster

With the P2P set up and P2P workers being up and running, you can run
your R code on it my setting the future plan. For example,

``` r

library(future)
plan(future.p2p::cluster)

f <- future(Sys.getpid())
v <- value(f)
print(v)
```

will give you the process ID of the P2P worker that took on this
future.[^3][^4]

Next, try the Mandelbrot demo of the
**[future](https://future.futureverse.org)** package;

``` r

library(future)
plan(future.p2p::cluster)
demo("mandelbrot", ask = TRUE)
```

Each tile will be processed by a separate P2P worker.

[^1]: The first time you launch a worker, or configure
    [`plan()`](https://future.futureverse.org/reference/plan.html) to
    use the P2P cluster, you might find that the \[wormhole-william\]
    executable is installed.

[^2]: If you are on MS Windows, you will get a Windows Security Alert
    asking you to “Allow access” for the Wormhole executable to access
    “public and private networks”. Check and accept both. Details: (i)
    Allow “private” networks if you have other local computers you want
    to participate in the P2P cluster. (ii) Allow “public” networks if
    you want to participate in a P2P cluster with computers running
    externally, e.g. your friends computers.

[^3]: The first time you launch a worker, or configure
    [`plan()`](https://future.futureverse.org/reference/plan.html) to
    use the P2P cluster, you might find that the \[wormhole-william\]
    executable is installed.

[^4]: If you are on MS Windows, you will get a Windows Security Alert
    asking you to “Allow access” for the Wormhole executable to access
    “public and private networks”. Check and accept both. Details: (i)
    Allow “private” networks if you have other local computers you want
    to participate in the P2P cluster. (ii) Allow “public” networks if
    you want to participate in a P2P cluster with computers running
    externally, e.g. your friends computers.
