package cs.system;

/** Contains methods to create types of objects locally or remotely, or obtain references to existing remote objects. This class cannot be inherited. */
@:native("System.Activator")
extern class Activator {
	@:overload(function<T>():T {})
	@:overload(function(type:cs.system.Type):Dynamic {})
	@:overload(function(type:cs.system.Type, nonPublic:Bool):Dynamic {})
	@:overload(function(type:cs.system.Type, args:cs.NativeArray<Dynamic>):Dynamic {})
	@:overload(function(type:cs.system.Type, args:cs.NativeArray<Dynamic>, activationAttributes:cs.NativeArray<Dynamic>):Dynamic {})
	@:overload(function(type:cs.system.Type, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, args:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic {})
	/**
	 * Creates an instance of the specified type using that type's parameterless
	 * constructor.
	 * @param type The type of object to create.
	 * @return A reference to the newly created object.
	 */
	static function CreateInstance(type:cs.system.Type, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, args:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo, activationAttributes:cs.NativeArray<Dynamic>):Dynamic;
}
