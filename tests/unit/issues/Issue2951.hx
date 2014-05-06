
package unit.issues;

import unit.Test;

class Issue2951 extends Test{

  public static inline function foo (x) {

  }

  public static function setCwd(x:Dynamic) {

  }

  public static function getLast(x:Dynamic) {
    return true;
  }

  public static function exists(x:Dynamic) {
    return true;
  }

  public function test ()
  {

      var last = null;
      var args = [];
      var HxString = {
        charCodeAt : function (a:Array<Dynamic>, i:Int) {
          return 1;
        }
      };
      if ((if ((if ((exists(last)))getLast(last) else false)){
          if ((! (args.length < 2))){
              var _g = HxString.charCodeAt(args[args.length - 2],0);
              var _g1 = 45;
              _g != _g1;
          } else true;
      } else false)){
          if ((args.length == 0)) true else false;
          setCwd(last);
      };
  }
}