<!--
%\VignetteIndexEntry{future.p2p: Personal P2P Cluster}
%\VignetteAuthor{Henrik Bengtsson}
%\VignetteKeyword{R}
%\VignetteKeyword{package}
%\VignetteKeyword{vignette}
%\VignetteKeyword{Rprofile}
%\VignetteKeyword{Renviron}
%\VignetteEngine{future.p2p::selfonly}
-->

If you have access to different machines, locally or remotely, you can
set up a "personal" P2P cluster to distribute you R tasks to those
machines using the **[future.p2p]** package.

This document gives instructions how to set up a _personal_ P2P
cluster. Setting up a "personal" cluster is very easy. You can either
set up the `plan()` first or launch your P2P workers first - the order
does not matter. I find in convenient to launch the P2P workers first.
To launch workers, log into your different machines and run:

```sh
$ Rscript -e future.p2p::worker
```

Launch as many workers as you needed. You can add more later, if
needed.

With your P2P workers set up, you can start using your personal
cluster, by setting the future plan.  For example,

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

PS. Note how we did not have to call `host_cluster()`, which is only
needed for _shared_ P2P cluster.

[future.p2p]: https://github.com/HenrikBengtsson/future.p2p
[future]: https://future.futureverse.org
