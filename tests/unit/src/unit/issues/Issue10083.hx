package unit.issues;

private enum FooBar {
  Foo;
  Bar;
}

class Issue10083 extends Test {
  function test() {
    var hasFoo = false;
    var tasks:Array<Void->Void> = [];
    var flags = ["A" => Bar, "B" => Foo, "C" => Bar];

    for (flag in flags) {
      switch (flag) {
        case Foo:
          hasFoo = true;

        case Bar:
          tasks.push(() -> eq(true, hasFoo));
      }
    }

    while (tasks.length > 0) tasks.shift()();
  }
}
