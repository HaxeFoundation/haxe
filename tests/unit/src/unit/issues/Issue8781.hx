package unit.issues;

private class ComplexEnum
{

	public static final FIRST:ComplexEnum = new ComplexEnum("first", 0);
	public static final SECOND:ComplexEnum = new ComplexEnum( "second", 1);
	public static final THIRD:ComplexEnum = new ComplexEnum( "third", 2);

	var name(null, default):String;
	var value(null, default):Int;

	public function new (name:String, value:Int )
	{
		this.name = name;
		this.value = value;
	}

	public function getPrefix():String
	{
		switch (this)
		{
			case FIRST:
				return "first_";
			case SECOND:
				return "second_";
			case THIRD:
				return "third_";
			default: throw "Unknown";
		}
	}

}

class Issue8781 extends unit.Test {
	function test() {
		eq("value: second_", "value: "+ComplexEnum.SECOND.getPrefix());
	}
}