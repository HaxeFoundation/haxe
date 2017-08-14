package unit;

class TestUnserializeInstantiator extends Test {

	function test() {

		// TypeInstantiator is useful when data members of serializable
    // classes / enums have been changed. For a complete list of
    // motivations, see: https://github.com/HaxeFoundation/haxe/pull/6497

    // Below we've simulated an "old" version of a class and enum
    // and a "new" version of the same. We have a TypeResolver and
    // TypeInstantiator which know how to upgrade old serializations
    // into the "new" codebase.

    var v = new OldClass();
    v.some_data = OldEnum.ThingB;
    var ser = haxe.Serializer.run(v);

    var u = new haxe.Unserializer(ser);
    u.setResolver(new MyUnserResolver());
    u.setInstantiator(new MyUnserInstantiator());
		var v2:NewClass = u.unserialize();

    // Check that the correct (new) class and enum deserialized
    t( Std.is(v2, NewClass) );
    t( Type.enumEq(v2.some_data, NewEnum.ThingB(false)) );
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// OldEnum / OldClass represents the codebase at a some older point in time
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enum OldEnum {
  ThingA;
  ThingB;
}

class OldClass
{
  public var some_data:OldEnum;
  public function new() { }
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// - - NewEnum / NewClass represents the latest state of the codebase - -
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enum NewEnum {
  ThingA;
  ThingB(modifier:Bool); // changed B constructor
  ThingC;
}

class NewClass
{
  // Changed some_data access / parameter name
  private var _some_data:NewEnum;
  public var some_data(get,never):NewEnum;
  public function get_some_data():NewEnum return _some_data;

  public function new() { }
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// This MyUnserResolver and MyUnserInstantiator comprise the ability to
// upgrade older serializations to the new codebase.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

class MyUnserInstantiator
{
	public function new() {}
	@:final public inline function createEnum<T>( e:Enum<T>, constructor:String, ?params : Array<Dynamic> ) : T
  {
    // Upgrade changed enum constructors

    // Fix old versions of ThingB params, though note that the
    // resolver has already resolved NewEnum.
    var is_newenum = Type.getEnumName(e)==Type.getEnumName(NewEnum);
    if (is_newenum && constructor=='ThingB' && (params==null || params.length==0)) {
      // Use a default value for the modifier:Bool
      params = [false];
    }

		return Type.createEnum(e,constructor,params);
  }

	@:final public inline function createEmptyInstance<T>( cl : Class<T> ) : T
  {
    // Could possibly want to do something wrt your serialization
    // schema, versioning, etc here.
		return Type.createEmptyInstance(cl);
  }

	@:final public inline function applyInstanceValues( instance : Dynamic, iterator ) : Void
  {
    // Here we need to know that OldClass.some_name --> NewClass._some_name
    if (Std.is(instance, NewClass)) { applyNewClassValues(instance, iterator); }
    else applyDefaultInstanceValues(instance, iterator);
  }

	@:final public inline function applyDynamicValues( obj : Dynamic, iterator ) : Void
  {
    // We didn't change any dynamic values' schema.
    applyDefaultInstanceValues(obj, iterator);
  }

  @:final public inline function applyNewClassValues( instance : NewClass, iterator ) : Void
  {
    // A sophisticated schema might use a serialization_version member
    // to detect and upgrade as appropriate. In our simple testcase,
    // we can just fix the some_data member variable name change.
    while( iterator.hasNext() ) {
      var key = iterator.key;
      if (key=='some_data') key = '_some_data';
      Reflect.setField(instance, key, iterator.value);
    }
  }


  // Helper, default apply behavior (same as the DefaultTypeInstantiator)
  @:final public inline function applyDefaultInstanceValues( instance : Dynamic, iterator ) : Void
  {
    while( iterator.hasNext() ) {
      Reflect.setField(instance, iterator.key, iterator.value);
    }
  }

}
