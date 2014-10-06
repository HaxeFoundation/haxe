package unit.issues;
import unit.Test;

class Issue3447 extends Test
{
	function test()
	{
		var f:Float = getValue();
		var val:Float = f % 100;
		var f2:Dynamic = getValue();
		var val2:Dynamic = f2 % 100;
		eq(f,f2);
	}

	static function getValue() {
			return 101.5;
	}
}
