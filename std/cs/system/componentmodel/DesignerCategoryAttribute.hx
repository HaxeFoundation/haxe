package cs.system.componentmodel;

/** Specifies that the designer for a class belongs to a certain category. */
@:native("System.ComponentModel.DesignerCategoryAttribute")
extern class DesignerCategoryAttribute extends cs.system.Attribute {
	/** Specifies that a component marked with this category use a component designer. This field is read-only. */
	static var Component(default, never):cs.system.componentmodel.DesignerCategoryAttribute;
	/** Specifies that a component marked with this category cannot use a visual designer. This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DesignerCategoryAttribute;
	/** Specifies that a component marked with this category use a form designer. This  field is read-only. */
	static var Form(default, never):cs.system.componentmodel.DesignerCategoryAttribute;
	/** Specifies that a component marked with this category use a generic designer. This  field is read-only. */
	static var Generic(default, never):cs.system.componentmodel.DesignerCategoryAttribute;
	/**
	 * Gets the name of the category.
	 * @return The name of the category.
	 */
	var Category(default, never):String;
	@:overload(function():Void {})
	function new(category:String):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
