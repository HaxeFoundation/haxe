package unit.issues;

class Issue9565 extends Test {
  function test() {
    var td = macro class Foo<@:foo T> {};
    var printer = new haxe.macro.Printer();
    var s = printer.printTypeDefinition(td);
    s = ~/[\t\n\r]/g.replace(s, "");
    eq("class Foo<@:foo T> {}", s);
  }
}
