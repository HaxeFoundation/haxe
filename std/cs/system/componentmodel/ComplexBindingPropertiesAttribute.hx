package cs.system.componentmodel;

/** Specifies the data source and data member properties for a component that supports complex data binding. This class cannot be inherited. */
@:native("System.ComponentModel.ComplexBindingPropertiesAttribute")
extern class ComplexBindingPropertiesAttribute extends cs.system.Attribute {
	/** Represents the default value for the  class. */
	static var Default(default, never):cs.system.componentmodel.ComplexBindingPropertiesAttribute;
	/**
	 * Gets the name of the data member property for the component to which the  is
	 * bound.
	 * @return The name of the data member property for the component to which  is
	 * bound
	 */
	var DataMember(default, never):String;
	/**
	 * Gets the name of the data source property for the component to which the  is
	 * bound.
	 * @return The name of the data source property for the component to which  is
	 * bound.
	 */
	var DataSource(default, never):String;
	@:overload(function():Void {})
	@:overload(function(dataSource:String):Void {})
	function new(dataSource:String, dataMember:String):Void;
	/**
	 * Determines whether the specified  is equal to the current  instance.
	 * @param obj The  to compare with the current  instance
	 * @return if the object is equal to the current instance; otherwise, , indicating
	 * they are not equal.
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
