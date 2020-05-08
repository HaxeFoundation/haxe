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
* Something on api.haxe.org: For content this is probably the right repository. If it's about the representation, try <https://github.com/HaxeFoundation/dox/issues> instead.
* Something else on haxe.org: <https://github.com/HaxeFoundation/haxe.org/issues>

## Submitting a Pull-Request

Thank you for your interest in contributing to Haxe! Haxe is a
community-driven project and your help is vital and appreciated!

When preparing to submit a pull-request, please make your PR as easy
as possible for core devs to evaluate and merge. To that end:

  * In your PR comments, include:

      * the reason for your proposed changes (What problem are you fixing?)
      * some possible solutions, and rationale for the one you chose
      * a summary of the code changes in your PR
      * any pros and cons to note about the solution you implemented
      * links to any relevant GitHub issues, PR's, and/or forum
        discussions

  * If you've found and fixed a bug, have you also included a
    corresponding test for it?
  * Does your code formatting match that of the rest of the project?
  * If your changes require updates to the documentation, does your PR
    include those as well?

Please also bear the following in mind:

  * Evaluating PR's takes time and effort. Even taking a look at a PR
    in order to request more info or clarification is not zero-cost.
  * Most members of the core team are volunteers too, and at any given time
    are typically already busy working on other areas of Haxe.
  * It's no fun providing negative feedback to a PR. The better you
    can craft and champion your PR, the more likely it is to be
    speedily evaluated.


## Debugging Hints

### Using a debugger

To debug the Haxe compiler, you can use either a system debugger (`gdb`/`lldb`), or [ocamldebug](http://caml.inria.fr/pub/docs/manual-ocaml/debugger.html). `ocamldebug` provides a better debugging experience. To use it, uncomment `(modes byte)` from [src/dune](src/dune) and recompile.

### Using printf

To print information about a type, you can add the following before most lines:

```ocaml
Printf.printf "%s\n" (s_type_kind t);
```

There are lots of other stringifying functions, search for "Printing" in `src/core/type.ml` and scroll down to find them.

## Other remarks:

- Sometimes people try to be particularly helpful by not only including broken parts in their code, but also "similar" code which is working. More often than not this is more distracting than helpful. If you want to highlight something like this, consider adding the working code commented out.
- We do not require a classic "What do you see/what do you expect?" form, but in some cases it is hard to figure out where you think the actual problem is otherwise.
- We're keeping this page quite short so there's a higher chance that people actually read it.
