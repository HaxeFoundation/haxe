package unit.issues;
using Issue9680.Issue9680_IntTools;

class Issue9680 extends Test {
  function test() {
    var int: Int = 0;
    eq('Int using', int.ext());

    var foo: Foo = int;
    eq('Foo using', foo.ext());
    eq('Foo resolve', foo.res);

    var bar: Bar = foo;
    eq('Bar using', bar.ext());
    eq('Bar resolve', bar.res);

    var baz: Baz = bar;
    eq('Bar using', bar.ext());
    eq('Bar resolve', bar.res);
  }
}

@:using(Issue9680.Issue9680_FooTools)
private abstract Foo(Int) from Int to Int {
  @:op(a.b) function resolve(name:String)
    return 'Foo resolve';
}

@:using(Issue9680.Issue9680_BarTools)
@:forward
private abstract Bar(Foo) from Foo to Foo from Int to Int {
  @:op(a.b) function resolve(name:String)
    return 'Bar resolve';
}

@:forward
private abstract Baz(Foo) from Bar to Bar from Int to Int {
}

class Issue9680_IntTools {
  public static function ext(that: Int)
    return 'Int using';
}

class Issue9680_FooTools {
  public static function ext(that: Foo)
    return 'Foo using';
}

class Issue9680_BarTools {
  public static function ext(that: Bar)
    return 'Bar using';
}
