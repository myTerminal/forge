# forge

[![License](https://img.shields.io/github/license/myTerminal/forge.svg)](https://opensource.org/licenses/MIT)

Quickly create your daily-driver workstation setup from a simple-to-read configuration file

## What is *forge*?

*forge* tends to provide a way to quickly configure a Linux (or UNIX-like) setup by automating the process of setting up package repositories, installing software packages, creating users, and other such steps involved in setting up a typical daily driver.

## History

This project takes birth from [one of my oldest projects](https://github.com/myTerminal/dotfiles) that I've used as a central repository to store not only my shared workstation configuration but also setup scripts for multiple operating systems further down to tweaks for very specific computers. Just the way I extracted out my [.emacs.d](https://github.com/myTerminal/.emacs.d) from it to be maintained as a separate project dedicated for my Emacs setup and configuration, I hope to create *forge* as a re-usable setup tool that can be used by anyone with minimal code and the least number of external dependencies.

## Supported Platforms

- [Void](https://voidlinux.org) (untested)
- [Arch](https://archlinux.org) (untested)
- [Fedora](https://getfedora.org) (untested)
- [Debian](https://www.debian.org) (untested)
- [macOS](https://www.apple.com/macos) (untested)

## External Dependencies

- [SBCL](http://www.sbcl.org)

## How to Use

(Coming soon...)

## Example(s)

Please refer to [the example config file](example/forge-config.lisp) for basic layout or view [one that is being used](https://github.com/myTerminal/dotfiles/blob/master/.setup/forge-config.lisp) for a more detailed example.

## To-Do

(Coming soon...)
