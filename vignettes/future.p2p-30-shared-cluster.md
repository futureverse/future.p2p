<!--
%\VignetteIndexEntry{future.p2p: P2P Cluster among Friends}
%\VignetteAuthor{Henrik Bengtsson}
%\VignetteKeyword{R}
%\VignetteKeyword{package}
%\VignetteKeyword{vignette}
%\VignetteKeyword{Rprofile}
%\VignetteKeyword{Renviron}
%\VignetteEngine{future.p2p::selfonly}
-->

You and your friends can share your R compute powers with each other,
regardless where in the world you are located. To do this, one of you
will host a "friends" P2P cluster by specifying which users should
have access. After this, each of you can contribute as many workers as
you like, and each of you can distribute your R processing to this P2P
cluster.

This document gives instructions how to set up a _shared_ P2P
cluster. The process is almost as simple as setting up a personal P2P
cluster. If this is the first time using **[future.p2p]**, I recommend
starting out with a _personal_ P2P cluster just to get a feel for what
is involved. After that, setting one up among friends, is almost as
easy.


## Hosting a P2P cluster

Lets assume that P2P users 'alice', 'bob', 'carol', and 'diana' wish
share a P2P cluster and user 'alice' agrees to host it. They can do
this by calling:

```sh
{alice}$ Rscript -e future.p2p::host-cluster --users=bob,carol,diana --cluster=alice/friends
```

Hosting a P2P cluster only means that you control who has access -
nothing else. For example, in our example, there will be _no_ extra
traffic going through the computer of 'alice'.


## Adding P2P workers

After this, 'bob', 'carol', 'diana', and 'alice' have equal access to
the P2P cluster 'alice/friends'.

Those who have access can contribute as many workers as they like. To
launch a P2P worker, call:

```sh
{bob}$ Rscript -e future.p2p::worker --cluster=alice/friends
```

Anyone can add more workers more later on.


## Using P2P cluster

With the P2P set up and P2P workers being up and running, you can run
your R code on it my setting the future plan. For example,

```r
library(future)
plan(future.p2p::cluster)

f <- future(Sys.getpid())
v <- value(f)
print(v)
```

will give you the process ID of the P2P worker that took on this
future.

Next, try the Mandelbrot demo of the **[future]** package;

```r
library(future)
plan(future.p2p::cluster)
demo("mandelbrot", ask = TRUE)
```

Each tile will be processed by a separate P2P worker.


[future.p2p]: https://github.com/HenrikBengtsson/future.p2p
[future]: https://future.futureverse.org
