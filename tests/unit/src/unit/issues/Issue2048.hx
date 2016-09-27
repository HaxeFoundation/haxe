package unit.issues;

class Issue2048 extends unit.Test
{
	public function testNullableInt()
	{
		var x = function ():Null<Int> {
			return null;
		}
		eq( null, x() );
	}
}
