---
title: Installing Amulet
description: Get started with installing Amulet!
next: 01-intro.html
---

### Using pacman (Arch Linux)

After adding one of the repositories below, refresh your package lists
and install install the `amuletml` package:

```
# pacman -Sy amuletml
```


#### Releases only

Add the following to your `/etc/pacman.conf`:

```
[amuletml]
Server = http://hydraz.semi.works/amulet/$arch
```

Download and sign Matheus' GPG key, `0E843EFDBA828772`:

```
# pacman-key --keyserver keys.openpgp.org --recv-keys 0E843EFDBA828772
# pacman-key --lsign-key 0E843EFDBA828772
```

If you would rather not use a key server, the key is also available
[here](https://hydraz.semi.works/me) and
[here](https://ahti.space/~mat/me).

#### Nightly builds

Our [Jenkins] compiles [nightly releases] every day at midnight (give or
take a handful of minutes). These are available in a separate Arch Linux
repository, which conflicts with the one above.


```
[amuletml-nightly]
Server = http://callisto.amulet.works/x86_64
```

Nightly builds are signed with the Jenkins release signing key,
available from our website [here](https://amulet.works/buildbot.pub).
Import it to the pacman keyring, then locally sign it:

```
# curl https://amulet.works/buildbot.pub | pacman-key --import
# pacman-key --lsign-key 8C2FA63AF6E8B500
```

[Jenkins]: https://callisto.amulet.works

### Using the AUR (Arch Linux)

The `amuletml-bin` AUR package is updated only with GitHub releases, but
building it will *always* build the latest available development
version; Therefore, the output of `--version` might not correspond to
the tagged version.

### Installing a binary distribution (other Linux)

You can use the installation script provided in this website:

```
curl https://amulet.works/install.sh | bash -
```

This will automatically download and install the latest Amulet build to
a prefix of your choosing (set the environment variable `PREFIX` to
change it). These binaries are statically linked and thus should work on
any Linux distribution; Please open an [issue] if it doesn't.

If you wish to download a nightly distribution, set the environment
variable `CHANNEL` to `nightly` (or `NIGHTLY`).

This installation script verifies the signature for both the release and
nightly channels. It will tell you what key you need to import if the
signature is invalid.

[issue]:
https://github.com/tmpim/amulet/issues/new?title=Installation%20Failure&labels=x-distribution

### Building from source (anything else)

Make sure you have [the Haskell Stack] installed.

Clone the repository and install using Stack:

```
git clone https://github.com/tmpim/amulet --depth 1
cd amulet
stack install
```

[the Haskell Stack]: https://haskellstack.org
