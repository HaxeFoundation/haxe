## Bir sorunu dosyalamadan önce/dosyalarken kontrol edilecek şeyler:

- Eğer gerçekten Haxe kodunda bir sorun olduğunu düşünüyorsanız kontrol edin. Eğer kendinizi "...'ı nasıl yaparım?" yazarken bulduysanız, farklı iletişim kanallarına bakmayı tercih edebilirsiniz. Daha fazla bilgi için https://haxe.org/community/community-support.html sayfasına bakın.
- Kodunuzu küçük bir örneğe sığdırın (http://sscce.org/ 'a bakın). Özellikle kütüphane bağımlılıklarından kaçının: Belirli bir kütüphane kullanmadan sorununuzu çoğaltamıyorsanız, başlangıçta Haxe ile ilgili bir sorun olmayabilir.
- Haxe geliştirme sürümünde sorununuzun zaten çözülüp çözülmediğine bakın (yapılar için http://build.haxe.org/ 'yi görün).
- Çoğu hedefler, okunabilir kod üretir. Üretilen kodun yanlış olabileceğinden şüpheleniyorssanız, çıktıyı kontrol etmeyi deneyin, Derleme parametrelerinize `-D dump = pretty` ekleyebilir ve üreteçlere iletilen kodu `dump` alt dizininde bulabilirsiniz.

<!--## Things to check before/while filing an issue:

- Check if you actually suspect that there's an issue in the Haxe code. If you find yourself writing "How do I..." you may want to consider a different communication channel. Refer to https://haxe.org/community/community-support.html for more information.
- Reduce your code to a minimal example (see http://sscce.org/). In particular avoid library dependencies: If you cannot reproduce your issue without using a specific library, it might not be a Haxe issue to begin with.
- Check if your problems are already resolved in the Haxe development version (for builds see http://build.haxe.org/).
- Most targets produce readable code. If you suspect the generated code to be wrong, try checking the output. Note that you can add `-D dump=pretty` to your compilation parameters and find the code which is passed to the generators in a `dump` subdirectory.-->

## Bu depo, sorunları iletmek için doğru depo mu?

Bu depo, Haxe derleyicisinin kendisi ve Haxe standart kütüphanesi hakkındadır. Haxe ekosisteminin parçası olan depoların genel önizlemesi şunlardır:

* haxelib komut satırı aracı ya da lib.haxe.org: <https://github.com/HaxeFoundation/haxelib/issues>
* try.haxe.org'daki bazı şeyler: <https://github.com/HaxeFoundation/try.haxe.org/issues>
* haxe.org/manual altındaki bazı şeyler: <https://github.com/HaxeFoundation/HaxeManual/issues>
* api.haxe.org'daki bazı şeyler: Bu depo, içerik için muhtemelen doğru depodur. Eğer sunum için ise, onun yerine <https://github.com/HaxeFoundation/dox/issues>'i deneyin.
* haxe.org'daki diğer bazı şeyler: <https://github.com/HaxeFoundation/haxe.org/issues>

<!--## Is this the right repository to report the issue?

This repository is about the Haxe compiler itself and the Haxe standard library. Here's an overview of repositories that are part of the Haxe ecosystem:

* The haxelib command line tool or lib.haxe.org: <https://github.com/HaxeFoundation/haxelib/issues>
* Something on try.haxe.org: <https://github.com/HaxeFoundation/try.haxe.org/issues>
* Something under haxe.org/manual: <https://github.com/HaxeFoundation/HaxeManual/issues>
* Something on api.haxe.org: For content this is probably the right repository. If it's about the representation, try <https://github.com/HaxeFoundation/dox/issues> instead.
* Something else on haxe.org: <https://github.com/HaxeFoundation/haxe.org/issues> -->

## Başka işaretler:

- Bazı durumlarda insanlar, sadece bozuk parçaları değil, aynı zamanda çalışan "benzer" kodları da dahil etmek suretiyle özellikle yardımsever olmaya çalışır. Çoğu zaman bu daha çok dikkat dağıtıcıdır ve yararlı değildir. Böyle bir şeyi vurgulamak istiyorsanız, işleyen kodu yorum satırına almayı düşünün.
- Klasik bir "Ne görüyorsun / ne bekliyorsun?" formuna ihtiyacımız yok, ancak bazı durumlarda gerçek sorunu nerede düşündüğünüzü anlamak zor olabilir..
- İnsanların bu sayfayı okuma şansını artırmak adına bu sayfayı kısa tutuyoruz.
