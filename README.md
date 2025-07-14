# future.p2p - Use Shared Peer-to-Peer Compute Resources via Futureverse


## Create P2P account (once; all users)

Sign up on [pico.sh] by calling:

```sh
$ ssh pico.sh
```

and choose a username and click ENTER. That's it!

This will add your public SSH key to the pico.sh server, which is then
used to identify you in all future interactions. If you have multiple
computers, you can add your public SSH keys for those as well. This
way, you can use your pico.sh account from multiple computer systems,
which can be handy if you want to setup parallel workers on one system
and harness their compute power from another.


## Setup P2P network (one user)

Pico.sh user 'alice' sets up a P2P cluster that pico.sh users 'bob',
'carol', and 'diana' have access to;

```sh
{alice}$ ssh pipe.pico.sh pipe p2p -a bob,carol,diana
```

This can be done from anywhere in the world and it does not have to be
where you run R. This will allow 'bob', 'carol', 'diana' and 'alice'
themselves to connect to the P2P cluster named `alice/p2p`. As long as
'alice' maintains the above SSH connect, this cluster is available to
all of the listed members.

After they launched the above, they can verify that they are now
operating a private P2P cluster `alice/p2p` that a select set of
pico.sh users have access to;

```sh
{alice}$ ssh pipe.pico.sh ls
Channel Information
- alice/p2p: (Access List: bob, carol, diana)
  Clients:
    Pipes:
    - 2c6483d6-9491-4cc8-9707-3e6bc85bc30b (alice@1.2.3.4:6543)
```

If one of their authorized friends connect using:

```sh
{bob}$ ssh pipe.pico.sh pipe alice/p2p
```

then they can see this:

```sh
{alice}$ ssh pipe.pico.sh ls
Channel Information
- alice/p2p: (Access List: alice, bob, carol, diana)
  Clients:
    Pipes:
    - 2c6483d6-9491-4cc8-9707-3e6bc85bc30b (alice@1.2.3.4:6543)
    - 747898a5-25fd-4547-b5a0-70f6ab92798b (bob@4.3.2.1:4694)
```

This tells 'alice' that both are connected to the `alice/p2p` cluster.

_Comment_: If 'bob' calls `ssh pipe.pico.sh ls`, they will _not_ see
the above. This is because that command only lists clusters that
oneself operates ("hosts"), but not clusters one are connected to.


## Parallelize via private P2P Network (any user)

In order to distribute R tasks on a P2P cluster, the current machine
must:

1. have SSH access to pipe.pico.sh,
2. have `wormhole` installed, and
3. support the `ws://` protocol used by wormhole


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


## Share your compute power with your friends (any user)

In order to share your compute resources on a machine, it must:

1. have SSH access to pipe.pico.sh,
2. have `wormhole` installed, and
3. support the `ws://` protocol used by wormhole

To launch a P2P worker, call:

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


## Appendix

### Set up a worker to connect to pico.sh via a jumphost

```r
> future.p2p::pico_p2p_worker(cluster = "alice/p2p", ssh_args = c("-J", "somehost"))
```

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_worker --cluster=alice/p2p --ssh_args="-J somehost"
```

[pico.sh]: https://pico.sh/
