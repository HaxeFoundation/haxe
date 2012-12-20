package unit;

private class EmptyClass {
	public function new() { }
}

private class ClassWithToString {
	public function new() { }
	public function toString() return "ClassWithToString.toString()"
}

private class ClassWithToStringChild extends ClassWithToString {
	
}

private class ClassWithToStringChild2 extends ClassWithToString {
	public override function toString() return "ClassWithToStringChild2.toString()"
}

private enum SomeEnum<T> {
	NoArguments;
	OneArgument(t:T);
}

#if !macro
@:build(unit.UnitBuilder.build("unitstd"))
#end
class TestSpecification extends Test 
{

}
