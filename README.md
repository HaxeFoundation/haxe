<p align="center">
  <a href="https://haxe.org/" title="haxe.org"><img src="extra/images/Readme.png" /></a>
</p>

<p align="center">
	<a href="https://dev.azure.com/HaxeFoundation/GitHubPublic/_build/latest?definitionId=1&branchName=development"><img src="https://dev.azure.com/HaxeFoundation/GitHubPublic/_apis/build/status/HaxeFoundation.haxe?branchName=development" alt="Azure Pipelines Build Status"></a>
	<a href="https://github.com/HaxeFoundation/haxe/actions"><img src="https://github.com/HaxeFoundation/haxe/workflows/CI/badge.svg" alt="GitHub Build Status"></a>
	<a href="https://saucelabs.com/u/haxe"><img src="https://saucelabs.com/buildstatus/haxe" alt="SauceLabs Test Status"></a>
	<a href="https://gitter.im/HaxeFoundation/haxe?utm_source=badge&amp;utm_medium=badge&amp;utm_campaign=pr-badge"><img src="https://badges.gitter.im/Join%20Chat.svg" alt="Gitter"></a>
	<a href="https://discordapp.com/invite/0uEuWH3spjck73Lo"><img src="https://img.shields.io/discord/162395145352904705.svg?logo=discord" alt="Discord"></a>
</p>

#

Haxe is an open source toolkit that allows you to easily build cross-platform tools and applications that target many mainstream platforms. The Haxe toolkit includes:

 * **The Haxe programming language**, a modern, high-level, strictly-typed programming language
 * **The Haxe cross-compiler**, a state-of-the-art, lightning-speed compiler for many targets
 * **The Haxe standard library**, a complete, cross-platform library of common functionality

Haxe allows you to compile for the following targets:

 * JavaScript
 * C++
 * C#
 * Java
 * JVM
 * Lua
 * PHP 7
 * Python 3
 * [HashLink](https://hashlink.haxe.org/)
 * [NekoVM](https://nekovm.org/)
 * Flash (SWF Bytecode)
 * And its own [interpreter](https://haxe.org/blog/eval/)

You can try Haxe directly from your browser at [try.haxe.org](https://try.haxe.org)!

For more information about Haxe, head to the [official Haxe website](https://haxe.org).

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

For information on using Haxe, consult the [Haxe documentation](https://haxe.org/documentation/):

 * [Haxe Introduction](https://haxe.org/documentation/introduction/), an introduction to the Haxe toolkit
 * [The Haxe Manual](https://haxe.org/manual/), the reference manual for the Haxe language
 * [Haxe Code Cookbook](https://code.haxe.org), code snippets / learning resource
 * [Haxe API](https://api.haxe.org), documentation for the Haxe standard and native APIs
 * [Haxelib](https://lib.haxe.org), Haxelib is the package manager for the Haxe Toolkit.

## Community

You can get help and talk with fellow Haxers from around the world via:

 * [Haxe Community Forum](http://community.haxe.org)
 * [Haxe on Stack Overflow](https://stackoverflow.com/questions/tagged/haxe)
 * [Haxe Gitter chatroom](https://gitter.im/HaxeFoundation/haxe/)
 * [Haxe Discord server](https://discordapp.com/invite/0uEuWH3spjck73Lo)
 * [#haxe on Twitter](https://twitter.com/hashtag/haxe?src=hash)

:+1: Get notified of the latest Haxe news, follow us on [Twitter](https://twitter.com/haxelang), [Facebook](https://www.facebook.com/haxe.org) and don't forget to read the [Haxe roundups](https://haxe.io/).

## Version compatibility

Haxe            | Neko  | SWF   | Python | HL   | PHP  | Lua |
--------------- | ----- | ----- | ------ | ---- | ---- | --- |
2.*             | 1.*   | 8-10  | -      | -    | -    | -   |
3.0.0           | 2.0.0 |       | -      | -    | 5.1+ | -   |
3.2.0           |       | 12-14 | 3.2+   | -    |      | -   |
3.3.0           | 2.1.0 | 21    |        | -    |      | 5.1, 5.2, 5.3, LuaJIT 2.0, 2.1 |
3.4.0           |       |       |        | 1.1  | 5.4+ and 7.0+ (with `-D php7`) |     |
4.0.0           | 2.3.0 |       |        | 1.11 | 7.0+ |     |

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for more. Thank you!
