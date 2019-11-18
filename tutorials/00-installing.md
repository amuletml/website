---
title: Installing Amulet
description: Get started with installing Amulet!
next: 01-intro.html
---

### Using `pacman` (Arch Linux)

Add the following to your `/etc/pacman.conf`:

```
[amuletml]
Server = http://amulet.ahti.space/$arch
Server = http://hydraz.semi.works/amulet/$arch
```

Download and sign my GPG key, `0E843EFDBA828772`:

```
# pacman-key --keyserver keys.openpgp.org --recv-keys 0E843EFDBA828772
# pacman-key --lsign-key 0E843EFDBA828772
```

The key is also available [here](https://hydraz.semi.works/me) and
[here](https://ahti.space/~mat/me).

Refresh the repositories and install the `amuletml` package:

```
# pacman -Sy amuletml
```

### Using the AUR (Arch Linux)

The `amuletml-bin` AUR package is updated only with GitHub releases, but
building it will *always* build the latest available development
version; Therefore, the output of `--version` might not correspond to
the tagged version.

### Installing a binary distribution (other Linux)

You can use the installation script provided in this website:

```
curl https://amulet.ahti.space/install.sh | bash
```

This will automatically download and install the latest Amulet build to
a prefix of your choosing (set the environment variable `PREFIX` to
change it). These binaries are statically linked and thus should work on
any Linux distribution; Please open an [issue] if it doesn't.

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
