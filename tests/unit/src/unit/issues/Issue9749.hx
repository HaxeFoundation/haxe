package unit.issues;
using Issue9749.Issue9749_XTools;

class Issue9749 extends Test {
  function test() {
    var x = {x: 0};
    var y: TypeY = x;
    var a: TypeA = x;
    var b: TypeB = x;
    var c: TypeC = x;
    var d: TypeD = x;
    eq('X', x.tool());
    eq('X', y.tool());
    eq('A', a.tool());
    eq('B', b.tool());
    eq('C', c.tool());
    eq('C', d.tool());
  }
}

private typedef TypeY = {x: Int};

@:using(Issue9749.Issue9749_TypeATools) private typedef TypeA = {x: Int};

@:using(Issue9749.Issue9749_TypeBTools) private typedef TypeB = {x: Int};

@:using(Issue9749.Issue9749_TypeCTools) private typedef TypeC = TypeB;

private typedef TypeD = TypeC;

class Issue9749_XTools {
  public static function tool(x: {x: Int}) return 'X';
}

class Issue9749_TypeATools {
  public static function tool(a: {x: Int}) return 'A';
}

class Issue9749_TypeBTools {
  public static function tool(b: {x: Int}) return 'B';
}

class Issue9749_TypeCTools {
  public static function tool(c: {x: Int}) return 'C';
}
