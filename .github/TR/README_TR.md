<p align="center">
  <a href="https://haxe.org/" title="haxe.org"><img src="https://github.com/HaxeFoundation/haxe/blob/development/extra/images/Readme.png" /></a>
</p>

<p align="center">
	<a href="https://github.com/HaxeFoundation/haxe/actions"><img src="https://github.com/HaxeFoundation/haxe/workflows/CI/badge.svg" alt="GitHub Build Status"></a>
	<a href="https://saucelabs.com/u/haxe"><img src="https://saucelabs.com/buildstatus/haxe" alt="SauceLabs Test Status"></a>
	<a href="https://gitter.im/HaxeFoundation/haxe?utm_source=badge&amp;utm_medium=badge&amp;utm_campaign=pr-badge"><img src="https://badges.gitter.im/Join%20Chat.svg" alt="Gitter"></a>
	<a href="https://discordapp.com/invite/0uEuWH3spjck73Lo"><img src="https://img.shields.io/discord/162395145352904705.svg?logo=discord" alt="Discord"></a>
</p>

#
Haxe, pek çok anayayın platformlarını hedef alan çapraz platform araçlarını ve uygulamalarını kolayca yapmanızı sağlayan açık kaynaklı bir araçsetidir. Haxe araçkiti şunları içerir:

 * **Haxe programlama dili**, modern, yüksek seviye, kesinlikle yazılmış programlama dili
 * **Haxe Çapraz derleyicisi**, sanatın statüsü, pekçok hedef için şimşek hızında derleyici
 * **Haxe standart kütüphanesi**, tamami, sıradan işlevler için bir çapraz platform kütüphanesi

<!--Haxe is an open source toolkit that allows you to easily build cross-platform tools and applications that target many mainstream platforms. The Haxe toolkit includes:

 * **The Haxe programming language**, a modern, high-level, strictly-typed programming language
 * **The Haxe cross-compiler**, a state-of-the-art, lightning-speed compiler for many targets
 * **The Haxe standard library**, a complete, cross-platform library of common functionality-->

Haxe, aşağıdaki hedefler için derleme yapmanıza olanak verir:

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
 * Ve kendi [tercümanı](https://haxe.org/blog/eval/)

<!--Haxe allows you to compile for the following targets:

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
 * And its own [interpreter](https://haxe.org/blog/eval/)-->

Haxe'yi doğrudan [try.haxe.org](https://try.haxe.org)'da tarayıcımız üzerinden deneyebilirsiniz!

Haxe hakkında daha fazla bilgi için [resmi Haxe websitesine](https://haxe.org) gidin.

<!--You can try Haxe directly from your browser at [try.haxe.org](https://try.haxe.org)!

For more information about Haxe, head to the [official Haxe website](https://haxe.org).-->
## Lisans

Haxe projesi, projenin farklı parçalarını kapsayan birkaç lisansa sahiptir.

 * Haxe Derleyicisi, GNU Genel Kamu Lisansı sürüm 2 veya herhangi en son sürümü altında yayınlanır.
 * Haxe Standart Kütüphanesi, MIT Lisansı altında yayınlanır.
 * Neko sanal makinesi, MIT Lisansı altında yayınlandı. Paketlenmiş çalışma zamanı kütüphaneleri (ndll) ve araçları https://github.com/HaxeFoundation/neko/blob/master/LICENSE'de belirtildiği haldeki lisanslar altında yayınlanır.

Tüm Haxe lisansları için, lütfen https://haxe.org/foundation/open-source.html ya da [extra/LICENSE.txt](extra/LICENSE.txt)'ye bakın.

<!--## License

The Haxe project has several licenses, covering different parts of the projects.

 * The Haxe compiler is released under the GNU General Public License version 2 or any later version.
 * The Haxe standard library is released under the MIT license.
 * The Neko virtual machine is released under the MIT license. Its bundled runtime libraries (ndll) and tools are released under open source licenses as described in https://github.com/HaxeFoundation/neko/blob/master/LICENSE

For the complete Haxe licenses, please see https://haxe.org/foundation/open-source.html or [extra/LICENSE.txt](extra/LICENSE.txt).-->
## Haxe'yi yükle

En son kararlı sürüm [https://haxe.org/download/](https://haxe.org/download/)'da bulunabilir. Önceden inşa edilmiş ikili dosyalar platformunuz için bulunabilir:

 * **[Windows Yükleyicisi](https://haxe.org/download/file/latest/haxe-latest-win.exe/)**
 * **[Windows ikili dosyaları](https://haxe.org/download/file/latest/haxe-latest-win.zip/)**
 * **[OSX Yükleyicisi](https://haxe.org/download/file/latest/haxe-latest-osx-installer.pkg/)**
 * **[OSX ikili dosyaları](https://haxe.org/download/file/latest/haxe-latest-osx.tar.gz/)**
 * **[Linux Yazılım Paketleri](https://haxe.org/download/linux/)**
 * **[Linux 32-bit ikili dosyaları](https://haxe.org/download/file/latest/haxe-latest-linux32.tar.gz/)**
 * **[Linux 64-bit ikili dosyaları](https://haxe.org/download/file/latest/haxe-latest-linux64.tar.gz/)**

Otomatikleştirilmiş geliştirme yapıları [build.haxe.org](http://build.haxe.org)'dan bulunabilir.

<!--## Installing Haxe

The latest stable release is available at [https://haxe.org/download/](https://haxe.org/download/). Pre-built binaries are available for your platform:

 * **[Windows installer](https://haxe.org/download/file/latest/haxe-latest-win.exe/)**
 * **[Windows binaries](https://haxe.org/download/file/latest/haxe-latest-win.zip/)**
 * **[OSX installer](https://haxe.org/download/file/latest/haxe-latest-osx-installer.pkg/)**
 * **[OSX binaries](https://haxe.org/download/file/latest/haxe-latest-osx.tar.gz/)**
 * **[Linux Software Packages](https://haxe.org/download/linux/)**
 * **[Linux 32-bit binaries](https://haxe.org/download/file/latest/haxe-latest-linux32.tar.gz/)**
 * **[Linux 64-bit binaries](https://haxe.org/download/file/latest/haxe-latest-linux64.tar.gz/)**

Automated development builds are available from [build.haxe.org](http://build.haxe.org).-->
<!--## Building from source

See [extra/BUILDING.md](extra/BUILDING.md).-->
## Kaynaktan inşa etme

[extra/BUILDING.md](extra/BUILDING.md)'ye bakın.

## Haxe'yi Kullanma

Haxe'yi kullanma hakkında bilgi için, [Haxe dökümantasyonuna](https://haxe.org/documentation/) danışın:

 * [Haxe'ye Giriş](https://haxe.org/documentation/introduction/), Haxe araçsetine giriş
 * [Haxe Kılavuzu](https://haxe.org/manual/), Haxe dili için referans kılavuz
 * [Haxe Kod Tarif Kitabı](https://code.haxe.org), kod parçacıkları / öğrenim kaynağu
 * [Haxe API](https://api.haxe.org), Haxe'nin standart ve yerli API'leri için dökümantasyon
 * [Haxelib](https://lib.haxe.org), Haxelib, Haxe araçseti için paket yöneticisidir.
<!--## Using Haxe

For information on using Haxe, consult the [Haxe documentation](https://haxe.org/documentation/):

 * [Haxe Introduction](https://haxe.org/documentation/introduction/), an introduction to the Haxe toolkit
 * [The Haxe Manual](https://haxe.org/manual/), the reference manual for the Haxe language
 * [Haxe Code Cookbook](https://code.haxe.org), code snippets / learning resource
 * [Haxe API](https://api.haxe.org), documentation for the Haxe standard and native APIs
 * [Haxelib](https://lib.haxe.org), Haxelib is the package manager for the Haxe Toolkit.-->
## Topluluk

Şunlar ile değerli Haxerlerden yardım isteyebilir ve onlarla konuşabilirsiniz:

 * [Haxe Topluluk Forumu](http://community.haxe.org)
 * [Stack Overflow'da Haxe](https://stackoverflow.com/questions/tagged/haxe)
 * [Haxe Gitter sohbet odası](https://gitter.im/HaxeFoundation/haxe/)

:+1: En son Haxe haberlerinden haberdar olun, bizi [Twitter'dan](https://twitter.com/haxelang) ve [Facebook'tan](https://www.facebook.com/haxe.org) takip edin ve [Haxe tpoarlamalarını](https://haxe.io/) okumayı unutmayın.
 <!--## Community

You can get help and talk with fellow Haxers from around the world via:

 * [Haxe Community Forum](http://community.haxe.org)
 * [Haxe on Stack Overflow](https://stackoverflow.com/questions/tagged/haxe)
 * [Haxe Gitter chatroom](https://gitter.im/HaxeFoundation/haxe/)
 
:+1: Get notified of the latest Haxe news, follow us on [Twitter](https://twitter.com/haxelang), [Facebook](https://www.facebook.com/haxe.org) and don't forget to read the [Haxe roundups](https://haxe.io/).-->

## Version compatibility

Haxe            | Neko  | SWF   | Python | HL   | PHP  | Lua |
--------------- | ----- | ----- | ------ | ---- | ---- | --- |
2.*             | 1.*   | 8-10  | -      | -    | -    | -   |
3.0.0           | 2.0.0 |       | -      | -    | 5.1+ | -   |
3.2.0           |       | 12-14 | 3.2+   | -    |      | -   |
3.3.0           | 2.1.0 | 21    |        | -    |      | 5.1, 5.2, 5.3, LuaJIT 2.0, 2.1 |
3.4.0           |       |       |        | 1.1  | 5.4+ ve 7.0+ (`-D php7` ile) |     |
4.0.0           | 2.3.0 |       |        | 1.11 | 7.0+ |     |

## Katkıda Bulunma

Daha fazlası için [CONTRIBUTING_TR.md'ye](CONTRIBUTING.md) bakın. Teşekkür ederiz!

<!--## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for more. Thank you!-->
