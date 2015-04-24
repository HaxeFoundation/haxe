package unit.issues;

class Issue3658 extends Test
{
	public function test()
	{
		eq("testStatic", Type.getClassFields(MyClass)[0]);
		eq("testStatic", Type.getClassFields(Type.resolveClass(Type.getClassName(MyClass)))[0]);
	}
}

@:keep private class MyClass<T>
{
	public static function testStatic()
	{
	}
}
