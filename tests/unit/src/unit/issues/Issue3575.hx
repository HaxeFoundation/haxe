package unit.issues;

class Issue3575 extends Test {
	function test() {
		eq('ChildBase!',new Child().toString());
		eq('ChildBase!',new Child() + '');
	}
}


#if !cpp @:nativeGen #end private class Base
{
#if (cs || java) @:overload #end
	public function getName()
	{
		return "Base!";
	}

#if (cs || java)
	@:overload public function getName(s:String)
	{
		return 'Base!:$s';
	}
#end
}

class DirectDescendant extends Base
{

}

class Child extends DirectDescendant
{
	public function new()
	{
	}

#if (cs || java) @:overload #end
	override public function getName()
	{
		return "Something Else";
	}

	public function toString()
	{
		return 'Child' + super.getName();
	}
}


