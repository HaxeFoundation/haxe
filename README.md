
# [<img src="https://haxe.org/img/haxe-logo-horizontal.svg" alt="Haxe logo" width="140">](https://haxe.org) - [The Cross-Platform Toolkit](https://haxe.org)
[![TravisCI Build Status](https://travis-ci.org/HaxeFoundation/haxe.svg?branch=development)](https://travis-ci.org/HaxeFoundation/haxe)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/HaxeFoundation/haxe?branch=development&svg=true)](https://ci.appveyor.com/project/HaxeFoundation/haxe)
[![SauceLabs Test Status](https://saucelabs.com/buildstatus/haxe)](https://saucelabs.com/u/haxe)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/HaxeFoundation/haxe?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Haxe is an open source toolkit that allows you to easily build cross-platform tools and applications that target many mainstream platforms. The Haxe toolkit includes:

 * **The Haxe programming language**, a modern, high-level, strictly-typed programming language
 * **The Haxe cross-compiler**, a state-of-the-art, lightning-speed compiler for many targets
 * **The Haxe standard library**, a complete, cross-platform library of common functionality

Haxe allows you to compile for the following targets:

 * JavaScript
 * C++
 * C#
 * [HashLink](http://hashlink.haxe.org/)
 * Java
 * Lua
 * PHP
 * Python 3
 * [NekoVM](http://nekovm.org/)
 * Flash
 * ActionScript 3

You can try Haxe directly from your browser at [try.haxe.org](https://try.haxe.org)!

For more information about Haxe, head to the [offical Haxe website](https://haxe.org).

## License

The Haxe project has several licenses, covering different parts of the projects.

 * The Haxe compiler is released under the GNU General Public License version 2 or any later version.
 * The Haxe standard library is released under the MIT license.
 * The Neko virtual machine is released under the MIT license. Its bundled runtime libraries (ndll) and tools are released under open source licenses as described in https://github.com/HaxeFoundation/neko/blob/master/LICENSE

For the complete Haxe licenses, please see https://haxe.org/foundation/open-source.html or [extra/LICENSE.txt](extra/LICENSE.txt).

## Installing Haxe

The latest stable release is available at [https://haxe.org/download/](https://haxe.org/download/). Pre-built binaries are available for your platform:

 * **[Windows installer](https://haxe.org/download/file/latest/haxe-latest-win.exe/)**
 * **[Windows binaries](https://haxe.org/download/file/latest/haxe-latest-win.zip/)**
 * **[OSX installer](https://haxe.org/download/file/latest/haxe-latest-osx-installer.pkg/)**
 * **[OSX binaries](https://haxe.org/download/file/latest/haxe-latest-osx.tar.gz/)**
 * **[Linux Software Packages](https://haxe.org/download/linux/)**
 * **[Linux 32-bit binaries](https://haxe.org/download/file/latest/haxe-latest-linux32.tar.gz/)**
 * **[Linux 64-bit binaries](https://haxe.org/download/file/latest/haxe-latest-linux64.tar.gz/)**

Automated development builds are available from [build.haxe.org](http://build.haxe.org).

## Building from source

See [extra/BUILDING.md](extra/BUILDING.md).

## Using Haxe

For information on on using Haxe, consult the [Haxe documentation](https://haxe.org/documentation/):

 * [Haxe Introduction](https://haxe.org/documentation/introduction/), an introduction to the Haxe toolkit
 * [The Haxe Manual](https://haxe.org/manual/), the reference manual for the Haxe language
 * [Haxe Code Cookbook](https://code.haxe.org), code snippets / learning resource
 * [Haxe API](https://api.haxe.org), documentation for the Haxe standard and native APIs
 * [Haxelib](https://lib.haxe.org), Haxelib is the package manager for the Haxe Toolkit.

## Community

You can get help and talk with fellow Haxers from around the world via:

 * [Haxe Community Forum](http://community.haxe.org)
 * [Haxe on Stack Overflow](https://stackoverflow.com/questions/tagged/haxe)
 * [Haxe Gittr chatroom](https://gitter.im/HaxeFoundation/haxe/)
 * [#haxe on Twitter](https://twitter.com/hashtag/haxe?src=hash)

:+1: Get notified of the latest Haxe news, follow us on [Twitter](https://twitter.com/haxelang), [Facebook](https://www.facebook.com/haxe.org) and don't forget to read the [Haxe roundups](https://haxe.io/).

## Version compatibility

Haxe          | Neko  | SWF |  Python   | HL    | PHP   | LUA  | 
----          | ----  | ----   | ----   |  ---- | ----  | ---- |
2.*           | 1.*   | 8-10   | -      | -     | -     | -    |
3.0.0         | 2.0.0 |        | -      | -     | 5.1+  | -    |
3.2.0         |       | 12-14  | 3.2+   | -     |       | -    |
3.3.0         | 2.1.0 | 21     |        | -     |       | 5.1, 5.2, 5.3, LuaJIT 2.0, LuaJIT 2.1 |
3.4.0         |       |        |        | 1.1   | 5.1 and 7.0 (with `-D php7`)   |      |
4.0 preview 1 |       |        |        | 1.2   | 7.0+  |      |
4.0 preview 3 |       |        |        | 1.3   |       |      |
4.0 preview 4 |       |        |        | 1.6   |       |      |


## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for more. Thank you!
