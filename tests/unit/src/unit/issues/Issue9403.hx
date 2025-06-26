package unit.issues;

import haxe.macro.Expr;

class Issue9403 extends Test {
  #if eval
  function test() {
    var v = macro $v{9403};
    t(v.expr.match(EConst(CInt('9403'))));
  }
  #end
}
