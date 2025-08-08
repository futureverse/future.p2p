# future.p2p - A Peer-to-Peer Compute Cluster via Futureverse

_- Share R compute among friends across the world_

## TL;DR

```r
library(future)

## Resolve futures via a P2P cluster shared among friends
plan(future.p2p::pico_p2p, cluster = "alice/friends")

## Create future
f <- future(Sys.getpid())
  
## Get results
v <- value(f)
print(v)
```


## Getting started

In order to join a future P2P cluster, you must:

1. have _SSH key pairs_ configured,

2. have a _[pico.sh] account_, and

3. have SSH access to pipe.pico.sh


In order to have a [pico.sh] account, you need to connect to their
services using _SSH keys_ - they do not accept password logins, which
also would not work for **future.p2p**. If you don't know about SSH
keys, the gist is that they allow you to SSH without having to enter
your password each time. Instead, your computer authenticates with the
server using secure public-private keys. It's a very convenient way of
working with SSH. You can read more it in the Wikibooks article
['OpenSSH/Cookbook/Public Key
Authentication'](https://en.wikibooks.org/wiki/OpenSSH%2FCookbook%2FPublic_Key_Authentication),
which also provides detailed instructions. The gist for creating a SSH
key pair is:

```sh
$ mkdir ~/.ssh/
$ chmod 0700 ~/.ssh/
$ ssh-keygen
```

and then follow the instructions. Although you can leave the
passphrase empty, I recommend to set one and let the operating
system's _SSH agent_ to manage authentication. This means that you
will only have to authenticate once when you log in into your
computer, instead of at each SSH connection.

With SSH key pairs configured, you should now be able to create a
[pico.sh] account by calling:

```sh
$ ssh pico.sh
```

If you're asked to accept the "SSH fingerprint", do so. Then choose
your pico.sh username and click <kbd>ENTER</kbd>.  That's it!  This
will add your public SSH key to the pico.sh servers, which is then
used to identify you in all future interactions. Press
<kbd>Ctrl-C</kbd> to exit.

Finally, verify SSH access to `pipe.pico.sh` (sic!);

```sh
$ ssh pipe.pico.sh
```

It is important that you do this and accept the SSH fingerprint for
this server too, otherwise you will not be able to connect to the P2P
cluster from R.


## Set up a shared P2P cluster (managed by one of the users)

Pico.sh users 'alice', 'bob', 'carol', and 'diana' decides to share a
P2P cluster and 'alice' agrees to set it up. They can do this by
calling:

```r
future.p2p::pico_p2p_cluster(cluster = "alice/friends", users = c("bob", "carol", "diana"))
```

Alternatively, they can set it up directly from the terminal using:

```sh
{alice}$ Rscript -e future.p2p::pico_p2p_cluster --cluster=alice/friends --users=bob,carol,diana
```

After this, 'bob', 'carol', 'diana', and 'alice' can use and share
each others compute resources from within R.

A future P2P cluster can be launched from anywhere in the world, and
it does not have to on a machine where 'alice' runs their own R
analysis. Being a manager of a P2P cluster comes with no cost,
e.g. there will be _no_ traffic going through alice's computer.

Note that the `alice/` prefix is reserved for pico user `alice`. This
is why user `bob` can _not_ create a cluster named `alice/pop` - only
one called `bob/{name}`.


## Parallelize via P2P cluster (all users)

Any user with access to the 'alice/friends' cluster can harness the
collective compute power. In our example, this means 'bob', 'carol',
'diana', and 'alice' may use the P2P cluster at the same time.

Just like with any other future backend, we specify that we want to
use the P2P cluster via `plan()` of the **future** package. Here is a
small example that evaluates `Sys.getpid()` on one of the 'alice/friends'
cluster workers, which one we don't know:

```r
library(future)

plan(future.p2p::pico_p2p, cluster = "alice/friends")

## Evaluate expression via P2P cluster
f <- future(Sys.getpid())

## Retrieve value
v <- value(f)
print(v)
```

_Comment:_ The first time you run this, you might find that the
[wormhole-william] executable is installed. If you are on MS Windows,
you will get a Windows Security Alert asking you to "Allow access" for
the Wormhole executable to access "public and private networks". Check
and accept both. Details: (i) Allow "private" networks if you have
other local computers you want to participate in the P2P cluster. (ii)
Allow "public" networks if you want to participate in a P2P cluster
with computers running externally, e.g. your friends computers.


## Share your compute power with your friends (any user)

Without parallel workers, the P2P cluster is useless and will not
process any parallel tasks. This is where the peer-to-peer concept
comes in, where we contribute our idle compute cycles to the cluster
for others to make use of. To contribute your R compute power to the
`alice/friends` cluster, launch a P2P worker as:

```r
future.p2p::pico_p2p_worker(cluster = "alice/friends")
```

Alternatively, you can launch it directly from the command line using:

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_worker --cluster=alice/friends
```

This will contribute one parallel worker to the p2p cluster. You can
contribute additional ones by repeating the same command one or more
times.

_Comment:_ The first time you run this, you might find that the
[wormhole-william] executable is installed. If you are on MS Windows,
you will get a Windows Security Alert asking you to "Allow access" for
the Wormhole executable to access "public and private networks". Check
and accept both. Details: (i) Allow "private" networks if you have
other local computers you want to participate in the P2P cluster. (ii)
Allow "public" networks if you want to participate in a P2P cluster
with computers running externally, e.g. your friends computers.


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

```r
> future.p2p::pico_p2p_worker(cluster = "alice/friends", ssh_args = c("-J", "somehost"))
```

```sh
{bob}$ Rscript -e future.p2p::pico_p2p_worker --cluster=alice/friends --ssh_args="-J somehost"
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
{bob}$ http_proxy="" Rscript -e future.p2p::pico_p2p_worker --cluster=alice/friends
```


[pico.sh]: https://pico.sh/
[Magic-Wormhole]: https://magic-wormhole.readthedocs.io/en/latest/
[wormhole-william]: https://github.com/psanford/wormhole-william
