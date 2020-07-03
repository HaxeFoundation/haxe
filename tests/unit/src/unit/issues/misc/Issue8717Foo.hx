package unit.issues.misc;

class Base
{
	public var name(default, null):String;

	public function new(name:String)
	{
		this.name = name;
	}
}

@:keep
class Issue8717Foo extends Base
{
	public function new()
	{
		super(createName());
	}

	private function createName():String
	{
		return "foo";
	}
}