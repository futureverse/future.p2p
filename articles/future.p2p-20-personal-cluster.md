# future.p2p: A Personal P2P Cluster

If you have access to different machines, locally or remotely, you can
set up a “personal” P2P cluster to distribute you R tasks to those
machines using the **[future.p2p](https://future.p2p.futureverse.org/)**
package.

This document gives instructions on how to set up a *personal* P2P
cluster. Setting up a “personal” cluster is very easy. You can either
set up the
[`plan()`](https://future.futureverse.org/reference/plan.html) first or
launch your P2P workers first - the order does not matter. I find in
convenient to launch the P2P workers first. To launch workers, log into
your different machines and run:[^1][^2]

``` sh
$ Rscript -e future.p2p::worker
```

Launch as many workers as you needed. You can add more later, if needed.

With your P2P workers set up, you can start using your personal cluster,
by setting the future plan. For example,

``` r

library(future)
plan(future.p2p::cluster)

f <- future(Sys.getpid())
v <- value(f)
print(v)
```

will give you the process ID of the P2P worker that took on this future.

Next, try the Mandelbrot demo of the
**[future](https://future.futureverse.org)** package;

``` r

library(future)
plan(future.p2p::cluster)
demo("mandelbrot", ask = TRUE)
```

Each tile will be processed by a separate P2P worker.

PS. Note how we did not have to call
[`host_cluster()`](https://future.p2p.futureverse.org/reference/host_cluster.md),
which is only needed for *shared* P2P cluster.

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
