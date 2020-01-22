---
title: Installing Amulet
description: Get started with installing Amulet!
next: 01-intro.html
---

### Installing a binary distribution (Linux)

You can use the installation script provided in this website:

```
curl https://amulet.works/install.sh | bash -
```

This will automatically download and install the latest Amulet build to
a prefix of your choosing (set the environment variable `PREFIX` to
change it). These binaries are statically linked and thus should work on
any Linux distribution; Please open an [issue] if it doesn't.

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
