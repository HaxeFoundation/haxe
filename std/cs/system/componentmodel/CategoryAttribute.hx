package cs.system.componentmodel;

/** Specifies the name of the category in which to group the property or event when displayed in a  control set to Categorized mode. */
@:native("System.ComponentModel.CategoryAttribute")
extern class CategoryAttribute extends cs.system.Attribute {
	/**
	 * Gets a  representing the Action category.
	 * @return A  for the action category.
	 */
	static var Action(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Appearance category.
	 * @return A  for the appearance category.
	 */
	static var Appearance(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Asynchronous category.
	 * @return A  for the asynchronous category.
	 */
	static var Asynchronous(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Behavior category.
	 * @return A  for the behavior category.
	 */
	static var Behavior(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Data category.
	 * @return A  for the data category.
	 */
	static var Data(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Default category.
	 * @return A  for the default category.
	 */
	static var Default(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Design category.
	 * @return A  for the design category.
	 */
	static var Design(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the DragDrop category.
	 * @return A  for the drag-and-drop category.
	 */
	static var DragDrop(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Focus category.
	 * @return A  for the focus category.
	 */
	static var Focus(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Format category.
	 * @return A  for the format category.
	 */
	static var Format(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Key category.
	 * @return A  for the key category.
	 */
	static var Key(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Layout category.
	 * @return A  for the layout category.
	 */
	static var Layout(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the Mouse category.
	 * @return A  for the mouse category.
	 */
	static var Mouse(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets a  representing the WindowStyle category.
	 * @return A  for the window style category.
	 */
	static var WindowStyle(default, never):cs.system.componentmodel.CategoryAttribute;
	/**
	 * Gets the name of the category for the property or event that this attribute is
	 * applied to.
	 * @return The name of the category for the property or event that this attribute
	 * is applied to.
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
	 * Returns the hash code for this attribute.
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
