package unit.issues;

class Issue3575 extends Test {
	function test() {
		eq('ChildBase!',new Child().toString());
		eq('ChildBase!',new Child() + '');
	}
}


@:nativeGen private class Base
{
	public function getName()
	{
		return "Base!";
	}
}

class Child extends Base
{
	public function new()
	{
	}

	override public function getName()
	{
		return "Something Else";
	}

	public function toString()
	{
		return 'Child' + super.getName();
	}
}
