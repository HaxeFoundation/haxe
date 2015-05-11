package unit.hxcpp_issues;

class String extends Test {
   @:keep public static var x = 1;
   @:keep public var y:Float;
   @:keep public function z() return 1;
	function test() {
      y = 1.0;
		t( Reflect.hasField(String, "x") );
		eq( Reflect.field(this, "y"),1.0);
		eq( Reflect.field(this, "z")(),1);
	}
}
