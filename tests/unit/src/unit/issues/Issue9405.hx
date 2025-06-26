package unit.issues;

class Issue9405 extends Test {
  function test() {
    var ct = macro : {> Foo, > Bar, baz: Int, qux: Int};
    var printer = new haxe.macro.Printer();
    var s = printer.printComplexType(ct);
    eq("{> Foo, > Bar, var baz : Int; var qux : Int; }", s);
  }
}
