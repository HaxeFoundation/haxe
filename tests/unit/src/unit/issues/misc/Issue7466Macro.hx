package unit.issues.misc;

import haxe.macro.Expr;
import haxe.macro.Context as C;

class Issue7466Macro {
  macro public static function canType (e:Expr) {
    return try {
    	C.typeof(e);
    	macro true;
    }
    catch (e:Dynamic) {
      macro false;
    }
  }

  macro public static function getCheckType () {
    return macro ({ name : "foo" } : { var name : Int; });
  }
}