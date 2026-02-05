package cs.system.componentmodel;

/** Provides metadata for a property representing a data field. This class cannot be inherited. */
@:native("System.ComponentModel.DataObjectFieldAttribute")
extern class DataObjectFieldAttribute extends cs.system.Attribute {
	/**
	 * Gets a value indicating whether a property represents an identity field in the
	 * underlying data.
	 * @return if the property represents an identity field in the underlying data;
	 * otherwise, . The default value is .
	 */
	var IsIdentity(default, never):Bool;
	/**
	 * Gets a value indicating whether a property represents a field that can be null
	 * in the underlying data store.
	 * @return if the property represents a field that can be null in the underlying
	 * data store; otherwise, .
	 */
	var IsNullable(default, never):Bool;
	/**
	 * Gets the length of the property in bytes.
	 * @return The length of the property in bytes, or -1 if not set.
	 */
	var Length(default, never):Int;
	/**
	 * Gets a value indicating whether a property is in the primary key in the
	 * underlying data.
	 * @return if the property is in the primary key of the data store; otherwise, .
	 */
	var PrimaryKey(default, never):Bool;
	@:overload(function(primaryKey:Bool):Void {})
	@:overload(function(primaryKey:Bool, isIdentity:Bool):Void {})
	@:overload(function(primaryKey:Bool, isIdentity:Bool, isNullable:Bool):Void {})
	function new(primaryKey:Bool, isIdentity:Bool, isNullable:Bool, length:Int):Void;
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param obj An object to compare with this instance of .
	 * @return if this instance is the same as the instance specified by the 
	 * parameter; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
