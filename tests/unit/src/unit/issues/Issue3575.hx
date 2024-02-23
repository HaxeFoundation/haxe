package unit.issues;

class Issue3575 extends Test {
	function test() {
		eq('ChildBase!',new Child().toString());
		eq('ChildBase!',new Child() + '');
	}
}


#if !cpp @:nativeGen #end private class Base
{
#if jvm @:overload #end
	public function getName()
	{
		return "Base!";
	}

#if jvm
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

#if jvm @:overload #end
	override public function getName()
	{
		return "Something Else";
	}

	public function toString()
	{
		return 'Child' + super.getName();
	}
}


