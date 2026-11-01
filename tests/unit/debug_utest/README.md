This is a small and very simple replacement for utest library.
The goal is to be able to run Haxe unit tests as simply as possible
so it becomes more easy to develop a new Haxe target without having
to fully support utest processing.

This can be enabled by changing in ../compile-each.hxml:
- remove `-lib utest`
- add `-cp debug_test`
- add `-dce no`
- several tests might not compile and will need to be disabled
- you can optionally activate utest.Macros.GENERIC to turn eq() into a generic function
  (it will help when unit testing native code, but some tests will not compile)

