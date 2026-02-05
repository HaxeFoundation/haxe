package cs.system.reflection;

/** Discovers the attributes of a class constructor and provides access to constructor metadata. */
@:native("System.Reflection.ConstructorInfo")
extern class ConstructorInfo extends cs.system.reflection.MethodBase {
	/** Represents the name of the class constructor method as it is stored in metadata. This name is always ".ctor". This field is read-only. */
	static var ConstructorName(default, never):String;
	/** Represents the name of the type constructor method as it is stored in metadata. This name is always ".cctor". This property is read-only. */
	static var TypeConstructorName(default, never):String;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return if  is equal to ; otherwise .
	 */
	static function op_Equality(left:cs.system.reflection.ConstructorInfo, right:cs.system.reflection.ConstructorInfo):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The first  to compare.
	 * @param right The second  to compare.
	 * @return if  is not equal to ; otherwise .
	 */
	static function op_Inequality(left:cs.system.reflection.ConstructorInfo, right:cs.system.reflection.ConstructorInfo):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function(parameters:cs.NativeArray<Dynamic>):Dynamic {})
	/**
	 * Invokes the constructor reflected by the instance that has the specified
	 * parameters, providing default values for the parameters not commonly used.
	 * @param parameters An array of values that matches the number, order and type
	 * (under the constraints of the default binder) of the parameters for this
	 * constructor. If this constructor takes no parameters, then use either an array
	 * with zero elements or , as in Object[] parameters = new Object[0]. Any object in
	 * this array that is not explicitly initialized with a value will contain the
	 * default value for that object type. For reference-type elements, this value is .
	 * For value-type elements, this value is 0, 0.0, or , depending on the specific
	 * element type.
	 * @return An instance of the class associated with the constructor.
	 */
	function Invoke(invokeAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, parameters:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo):Dynamic;
}
