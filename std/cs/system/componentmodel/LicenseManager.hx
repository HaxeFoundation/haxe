package cs.system.componentmodel;

/** Provides properties and methods to add a license to a component and to manage a . This class cannot be inherited. */
@:native("System.ComponentModel.LicenseManager")
extern class LicenseManager {
	/**
	 * Gets or sets the current , which specifies when you can use the licensed object.
	 * @return A  that specifies when you can use the licensed object.
	 */
	static var CurrentContext(default, default):cs.system.componentmodel.LicenseContext;
	/**
	 * Gets the  which specifies when you can use the licensed object for the .
	 * @return One of the  values, as specified in the  property.
	 */
	static var UsageMode(default, never):cs.system.componentmodel.LicenseUsageMode;
	@:overload(function(type:cs.system.Type, creationContext:cs.system.componentmodel.LicenseContext):Dynamic {})
	/**
	 * Creates an instance of the specified type, given a context in which you can use
	 * the licensed instance.
	 * @param type A  that represents the type to create.
	 * @param creationContext A  that specifies when you can use the licensed instance.
	 * @return An instance of the specified type.
	 */
	static function CreateWithContext(type:cs.system.Type, creationContext:cs.system.componentmodel.LicenseContext, args:cs.NativeArray<Dynamic>):Dynamic;
	/**
	 * Returns whether the given type has a valid license.
	 * @param type The  to find a valid license for.
	 * @return if the given type is licensed; otherwise, .
	 */
	static function IsLicensed(type:cs.system.Type):Bool;
	@:overload(function(type:cs.system.Type):Bool {})
	/**
	 * Determines whether a valid license can be granted for the specified type.
	 * @param type A  that represents the type of object that requests the .
	 * @return if a valid license can be granted; otherwise, .
	 */
	static function IsValid(type:cs.system.Type, instance:Dynamic, license:cs.Ref<cs.system.componentmodel.License>):Bool;
	/**
	 * Prevents changes being made to the current  of the given object.
	 * @param contextUser The object whose current context you want to lock.
	 */
	static function LockContext(contextUser:Dynamic):Void;
	/**
	 * Allows changes to be made to the current  of the given object.
	 * @param contextUser The object whose current context you want to unlock.
	 */
	static function UnlockContext(contextUser:Dynamic):Void;
	@:overload(function(type:cs.system.Type):Void {})
	/**
	 * Determines whether a license can be granted for the specified type.
	 * @param type A  that represents the type of object that requests the license.
	 */
	static function Validate(type:cs.system.Type, instance:Dynamic):cs.system.componentmodel.License;
}
