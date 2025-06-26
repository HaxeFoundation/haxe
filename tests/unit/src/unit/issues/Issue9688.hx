package unit.issues;

class Issue9688 extends Test {
  function test() {
    var field: Bar = {x: 0, y: null};
    eq(1, field.x);

    var direct: Bar = {x: 0};
    eq(0, direct.x);
  }
}

private typedef Foo = {x: Int, ?y: Int, ?z: Int};

@:forward private abstract Bar(Foo) from Foo {
  @:from static inline function fromX(value: {x: Int, ?y: Int}): Bar
    return {x: 1, y: 0, z: 0};
}
