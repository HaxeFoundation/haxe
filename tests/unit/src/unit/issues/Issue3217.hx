package unit.issues;

class Issue3217 extends Test
{
	public function test()
	{
      #if (cpp && !cppia)
		var t:NativeClassStruct = null;
      #else
		var t:{ test:Int } = new NativeClass();
      #end
		t.test = 10;
		eq(t.test,10);
	}
}

#if (cpp && !cppia)
@:native("cpp::Struct< ::unit::issues::_Issue3217::NativeClass>")
@:include("unit/issues/_Issue3217/NativeClass.h")
private extern class NativeClassStruct extends NativeClass { }
#end

@:nativeGen
private class NativeClass
{
  public var test:Int;
  public function new() {}
}
