package unit;

class TestUnserializeResolver extends Test {

	function test() {

		// TypeResolver is useful when package structure or class names
    // have been changed. Below we've simulated that with an
    // old class name and a new class name.

    var v = new OldClass();
    v.some_data = OldEnum.ThingB;
    var ser = haxe.Serializer.run(v);

    var u = new haxe.Unserializer(ser);
    u.setResolver(new MyUnserResolver());
		var v2:NewClass = u.unserialize();

    // Check that the correct (new) class and enum deserialized
    t( Std.is(v2, NewClass) );
    t( Type.enumEq(v2.some_data, NewEnum.ThingB) );
	}

}

enum OldEnum {
  ThingA;
  ThingB;
}

class OldClass
{
  public var some_data:OldEnum;
  public function new() { }
}

enum NewEnum {
  ThingA;
  ThingB;
}

class NewClass
{
  public var some_data:NewEnum;
  public function new() { }
}

class MyUnserResolver
{
	public function new() {}
	@:final public inline function resolveClass(name:String):Class<Dynamic>
  {
    // Account for changes in class name
    if (name=='unit.OldClass') name = 'unit.NewClass';

    return Type.resolveClass(name);
  }
	@:final public inline function resolveEnum(name:String):Enum<Dynamic>
  {
    // Account for changes in enum name
    if (name=='unit.OldEnum') name = 'unit.NewEnum';

    return Type.resolveEnum(name);
  }
}
