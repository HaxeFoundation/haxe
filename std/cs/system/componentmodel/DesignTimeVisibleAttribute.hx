package cs.system.componentmodel;

/** marks a component's visibility. If  is present, a visual designer can show this component on a designer. */
@:native("System.ComponentModel.DesignTimeVisibleAttribute")
extern class DesignTimeVisibleAttribute extends cs.system.Attribute {
	/** The default visibility which is . */
	static var Default(default, never):cs.system.componentmodel.DesignTimeVisibleAttribute;
	/** Marks a component as not visible in a visual designer. */
	static var No(default, never):cs.system.componentmodel.DesignTimeVisibleAttribute;
	/** Marks a component as visible in a visual designer. */
	static var Yes(default, never):cs.system.componentmodel.DesignTimeVisibleAttribute;
	/**
	 * Gets or sets whether the component should be shown at design time.
	 * @return if this component should be shown at design time, or  if it shouldn't.
	 */
	var Visible(default, never):Bool;
	@:overload(function():Void {})
	function new(visible:Bool):Void;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An Object to compare with this instance or a null reference ( in
	 * Visual Basic).
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a value indicating if this instance is equal to the  value.
	 * @return , if this instance is equal to the  value; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
