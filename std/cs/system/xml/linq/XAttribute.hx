package cs.system.xml.linq;

/** Represents an XML attribute. */
@:native("System.Xml.Linq.XAttribute")
extern class XAttribute extends cs.system.xml.linq.XObject {
	/**
	 * Gets an empty collection of attributes.
	 * @return An  of  containing an empty collection.
	 */
	static var EmptySequence(default, never):cs.system.collections.generic.IEnumerable<cs.system.xml.linq.XAttribute>;
	/**
	 * Determines if this attribute is a namespace declaration.
	 * @return if this attribute is a namespace declaration; otherwise .
	 */
	var IsNamespaceDeclaration(default, never):Bool;
	/**
	 * Gets the expanded name of this attribute.
	 * @return An  containing the name of this attribute.
	 */
	var Name(default, never):cs.system.xml.linq.XName;
	/**
	 * Gets the next attribute of the parent element.
	 * @return An  containing the next attribute of the parent element.
	 */
	var NextAttribute(default, never):cs.system.xml.linq.XAttribute;
	/**
	 * Gets the previous attribute of the parent element.
	 * @return An  containing the previous attribute of the parent element.
	 */
	var PreviousAttribute(default, never):cs.system.xml.linq.XAttribute;
	/**
	 * Gets or sets the value of this attribute.
	 * @return A  containing the value of this attribute.
	 */
	var Value(default, default):String;
	@:overload(function(other:cs.system.xml.linq.XAttribute):Void {})
	function new(name:cs.system.xml.linq.XName, value:Dynamic):Void;
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Bool {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.system.DateTime {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.system.DateTimeOffset {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.system.Decimal {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Float {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.system.Guid {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Int {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):haxe.Int64 {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<Bool> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.system.DateTimeOffset> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.system.DateTime> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.system.Decimal> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<Float> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.system.Guid> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<Int> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<haxe.Int64> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<Single> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.system.TimeSpan> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.UInt> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Null<cs.UInt64> {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):Single {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):String {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.system.TimeSpan {})
	@:overload(function(attribute:cs.system.xml.linq.XAttribute):cs.UInt {})
	/**
	 * Cast the value of this  to a .
	 * @param attribute The  to cast to .
	 * @return A  that contains the content of this .
	 */
	static function op_Explicit(attribute:cs.system.xml.linq.XAttribute):cs.UInt64;
	/** Removes this attribute from its parent element. */
	function Remove():Void;
	/**
	 * Sets the value of this attribute.
	 * @param value The value to assign to this attribute.
	 */
	function SetValue(value:Dynamic):Void;
	/**
	 * Converts the current  object to a string representation.
	 * @return A  containing the XML text representation of an attribute and its value.
	 */
	function ToString():String;
}
