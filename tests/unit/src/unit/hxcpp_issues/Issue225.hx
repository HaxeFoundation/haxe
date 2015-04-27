package unit.hxcpp_issues;

import haxe.Int64;
using haxe.Int64;

interface InterfaceWithDefaultI64 {
   public function optionalI64(?i:Int64) : Int;
}

class Issue225 extends Test implements InterfaceWithDefaultI64 {
	function test() {
      var i:InterfaceWithDefaultI64 = new Issue225();
      eq(i.optionalI64(), -1);
      eq(i.optionalI64(Int64.make(0,1)), 1);
   }
   public function optionalI64(?i:Int64)
       return i==null ? -1 : i.toInt();
}

