
# [<img src="http://haxe.org/img/haxe-logo-horizontal.svg" alt="Haxe logo" width="140">](http://haxe.org) - [The Cross-Platform Toolkit](http://haxe.org)
[![TravisCI Build Status](https://travis-ci.org/HaxeFoundation/haxe.svg?branch=development)](https://travis-ci.org/HaxeFoundation/haxe)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/HaxeFoundation/haxe?branch=development&svg=true)](https://ci.appveyor.com/project/HaxeFoundation/haxe)
[![SauceLabs Test Status](https://saucelabs.com/buildstatus/haxe)](https://saucelabs.com/u/haxe)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/HaxeFoundation/haxe?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Haxe is an open source toolkit that allows you to easily build cross-platform tools and applications that target many mainstream platforms. The Haxe toolkit includes:

 * **The Haxe programming language**, a modern, high-level, strictly-typed programming language
 * **The Haxe cross-compiler**, a state-of-the-art, lightning-speed compiler for many targets
 * **The Haxe standard library**, a complete, cross-platform library of common functionality

Haxe allows you to compile for the following targets:

 * ActionScript 3
 * C++
 * C#
 * Flash
 * Java
 * JavaScript
 * NekoVM
 * PHP
 * Python

You can try Haxe directly from your browser at [try.haxe.org](http://try.haxe.org)!

For more information about Haxe, head to the [offical Haxe website](http://haxe.org).

## License

The Haxe project has several licenses, covering different parts of the projects.

 * The Haxe compiler is released under the GNU General Public License version 2 or any later version.
 * The Haxe libraries are released under a "two-clause" BSD license.
 * The Neko runtime is licensed under the GNU Lesser General Public License version 2.1 or any later version.

For the complete Haxe licenses, please see http://haxe.org/foundation/open-source.html or [extra/LICENSE.txt](extra/LICENSE.txt).

## Installing Haxe

The latest stable release is [Haxe 3.2.0](http://haxe.org/download/version/3.2.0/). Pre-built binaries are available for your platform:

 * **[Windows installer](http://haxe.org/download/file/3.2.0/haxe-3.2.0-win.exe)**
 * **[Windows binaries](http://haxe.org/download/file/3.2.0/haxe-3.2.0-win.zip)**
 * **[OSX installer](http://haxe.org/download/file/3.2.0/haxe-3.2.0-osx-installer.pkg)**
 * **[OSX binaries](http://haxe.org/download/file/3.2.0/haxe-3.2.0-osx.tar.gz)**
 * **[Linux 32-bit binaries](http://haxe.org/download/file/3.2.0/haxe-3.2.0-linux32.tar.gz)**
 * **[Linux 64-bit binaries](http://haxe.org/download/file/3.2.0/haxe-3.2.0-linux64.tar.gz)**

Automated development builds are available from [build.haxe.org](http://build.haxe.org).

## Building from source

 1. Clone the repository using git. Be sure to initialize and fetch the submodules.

        git clone --recursive git://github.com/HaxeFoundation/haxe.git
        cd haxe

 2. Follow the [documentation on building Haxe for your platform](http://haxe.org/documentation/introduction/building-haxe.html).

## Using Haxe

For information on on using Haxe, consult the [Haxe documentation](http://haxe.org/documentation):

 * [Haxe Introduction](http://haxe.org/documentation/introduction), an introduction to the Haxe toolkit
 * [The Haxe Manual](http://haxe.org/manual), the reference manual for the Haxe language
 * [Haxe API](http://api.haxe.org), documentation for the Haxe standard and native APIs
 * [Haxelib](http://lib.haxe.org), a repository of Haxe libraries for a variety of needs

## Community

You can get help and talk with fellow Haxers from around the world via:

 * the [official Haxe Google Group](https://groups.google.com/forum/#!forum/haxelang)
 * the [Haxe IRC chatroom](http://unic0rn.github.io/tiramisu/haxe), #haxe on chat.freenode.net

## Version compatibility

Haxe   | neko
----   | -----
2.*    | 1.*
3.0.0  | 2.0.0
3.1.3  | 2.0.0
3.2.0  | 2.0.0
