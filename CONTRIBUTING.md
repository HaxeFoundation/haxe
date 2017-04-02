## Things to check before/while filing an issue:

- Check if you actually suspect that there's an issue in the Haxe code. If you find yourself writing "How do I..." you may want to consider a different communication channel. Refer to https://haxe.org/community/community-support.html for more information.
- Reduce your code to a minimal example (see http://sscce.org/). In particular avoid library dependencies: If you cannot reproduce your issue without using a specific library, it might not be a Haxe issue to begin with.
- Check if your problems are already resolved in the Haxe development version (for builds see http://builds.haxe.org/).
- Most targets produce readable code. If you suspect the generated code to be wrong, try checking the output. Note that you can add `-D dump=pretty` to your compilation parameters and find the code which is passed to the generators in a `dump` subdirectory.

## Is this the right repository to report the issue?

This repository is about the Haxe compiler itself and the Haxe standard library. Here's an overview of repositories that are part of the Haxe ecosystem:

* The haxelib command line tool or lib.haxe.org: <https://github.com/HaxeFoundation/haxelib/issues>
* Something on try.haxe.org: <https://github.com/clemos/try-haxe/issues>
* Something under haxe.org/manual: <https://github.com/HaxeFoundation/HaxeManual/issues>
* Something on api.haxe.org: For content this is probably the right repository. If it's about the representation, try <https://github.com/dpeek/dox/issues> instead.
* Something else on haxe.org: <https://github.com/HaxeFoundation/haxe.org/issues>

## Other remarks:

- Sometimes people try to be particularly helpful by not only including broken parts in their code, but also "similar" code which is working. More often than not this is more distracting than helpful. If you want to highlight something like this, consider adding the working code commented out.
- We do not require a classic "What do you see/what do you expect?" form, but in some cases it is hard to figure out where you think the actual problem is otherwise.
- We're keeping this page quite short so there's a higher chance that people actually read it.
