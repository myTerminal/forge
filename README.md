# forge

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/myTerminal/forge.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

Quickly create your daily-driver workstation setup from a simple-to-read configuration file

## What is *forge*?

*forge* tends to provide a way to quickly configure a Linux (or UNIX-like) setup by automating the process of setting up package repositories, installing software packages, creating users, and other steps involved in setting up a typical daily driver.

## History

This project takes birth from [one of my oldest projects](https://github.com/myTerminal/dotfiles) that I've used as a central repository to store not only my shared workstation configuration but also setup scripts for multiple operating systems, even further down to tweaks for specific computer hardware. I extracted all possible setup-related scripts into a re-usable program (just the way I did with my [.emacs.d](https://github.com/myTerminal/.emacs.d) so that it lives as a standalone and dedicated project to hold my Emacs configuration) and I hope to create *forge* as a re-usable setup tool driven with a minimal configuration file and the least number of external dependencies.

## Supported Platforms

- [Void](https://voidlinux.org) (TESTED)
- [Arch](https://archlinux.org) (untested)
- [Fedora](https://getfedora.org) (untested)
- [Debian](https://www.debian.org) (untested)
- [macOS](https://www.apple.com/macos) (untested)
- more (untested)

## External Dependencies

Dependencies that are (mostly) installed automatically:

- [SBCL](http://www.sbcl.org)

Rest of the dependencies that are also needed:

- [MAKE](https://www.gnu.org/software/make)

## How to Use

*forge* can be used with your configuration file written in a Lisp-like format, specifying information about supported operating systems, software packages to be installed, and custom scripts to be executed in order to personalize the machine for one's own needs. *forge* can be started by executing [start](start) located at the root of the project and passing to it a run mode and a configuration file. Below is an example:

    ./start debug ~/.setup/forge-config.lisp

The above assumes that you are running the command at the root of the project directory cloned in a local workspace.

## Example(s)

Please refer to [the example config file](example/forge-config.lisp) for the basic layout or view [one that is being used](https://github.com/myTerminal/dotfiles/blob/master/.setup/forge-config.lisp) for a more detailed example.

## To-Do

- Detection of C-c interrupt
