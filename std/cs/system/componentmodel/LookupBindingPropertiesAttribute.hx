package cs.system.componentmodel;

/** Specifies the properties that support lookup-based binding. This class cannot be inherited. */
@:native("System.ComponentModel.LookupBindingPropertiesAttribute")
extern class LookupBindingPropertiesAttribute extends cs.system.Attribute {
	/** Represents the default value for the  class. */
	static var Default(default, never):cs.system.componentmodel.LookupBindingPropertiesAttribute;
	/**
	 * Gets the name of the data source property for the component to which the  is
	 * bound.
	 * @return The data source property for the component to which the  is bound.
	 */
	var DataSource(default, never):String;
	/**
	 * Gets the name of the display member property for the component to which the  is
	 * bound.
	 * @return The name of the display member property for the component to which the 
	 * is bound.
	 */
	var DisplayMember(default, never):String;
	/**
	 * Gets the name of the lookup member for the component to which this attribute is
	 * bound.
	 * @return The name of the lookup member for the component to which the  is bound.
	 */
	var LookupMember(default, never):String;
	/**
	 * Gets the name of the value member property for the component to which the  is
	 * bound.
	 * @return The name of the value member property for the component to which the  is
	 * bound.
	 */
	var ValueMember(default, never):String;
	@:overload(function():Void {})
	function new(dataSource:String, displayMember:String, valueMember:String, lookupMember:String):Void;
	/**
	 * Determines whether the specified  is equal to the current  instance.
	 * @param obj The  to compare with the current  instance
	 * @return if the object is equal to the current instance; otherwise, , indicating
	 * they are not equal.
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
