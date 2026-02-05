package cs.system.drawing;

/** Represents an ordered pair of integer x- and y-coordinates that defines a point in a two-dimensional plane. */
@:native("System.Drawing.Point")
extern class Point extends cs.system.ValueType {
	/** Represents a  that has  and  values set to zero. */
	static var Empty(default, never):cs.system.drawing.Point;
	/**
	 * Gets a value indicating whether this  is empty.
	 * @return if both  and  are 0; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets or sets the x-coordinate of this .
	 * @return The x-coordinate of this .
	 */
	var X(default, default):Int;
	/**
	 * Gets or sets the y-coordinate of this .
	 * @return The y-coordinate of this .
	 */
	var Y(default, default):Int;
	@:overload(function(sz:cs.system.drawing.Size):Void {})
	@:overload(function(dw:Int):Void {})
	function new(x:Int, y:Int):Void;
	/**
	 * Adds the specified  to the specified .
	 * @param pt The  to add.
	 * @param sz The  to add
	 * @return The  that is the result of the addition operation.
	 */
	static function Add(pt:cs.system.drawing.Point, sz:cs.system.drawing.Size):cs.system.drawing.Point;
	/**
	 * Converts the specified  to a  by rounding the values of the  to the next higher
	 * integer values.
	 * @param value The  to convert.
	 * @return The  this method converts to.
	 */
	static function Ceiling(value:cs.system.drawing.PointF):cs.system.drawing.Point;
	/**
	 * Translates a  by a given .
	 * @param pt The  to translate.
	 * @param sz A  that specifies the pair of numbers to add to the coordinates of .
	 * @return The translated .
	 */
	static function op_Addition(pt:cs.system.drawing.Point, sz:cs.system.drawing.Size):cs.system.drawing.Point;
	/**
	 * Compares two  objects. The result specifies whether the values of the  and 
	 * properties of the two  objects are equal.
	 * @param left A  to compare.
	 * @param right A  to compare.
	 * @return if the  and  values of  and  are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.drawing.Point, right:cs.system.drawing.Point):Bool;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param p The  to be converted.
	 * @return The  that results from the conversion.
	 */
	static function op_Explicit(p:cs.system.drawing.Point):cs.system.drawing.Size;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param p The  to be converted.
	 * @return The  that results from the conversion.
	 */
	static function op_Implicit(p:cs.system.drawing.Point):cs.system.drawing.PointF;
	/**
	 * Compares two  objects. The result specifies whether the values of the  or 
	 * properties of the two  objects are unequal.
	 * @param left A  to compare.
	 * @param right A  to compare.
	 * @return if the values of either the  properties or the  properties of  and 
	 * differ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.drawing.Point, right:cs.system.drawing.Point):Bool;
	/**
	 * Translates a  by the negative of a given .
	 * @param pt The  to translate.
	 * @param sz A  that specifies the pair of numbers to subtract from the coordinates
	 * of .
	 * @return A  structure that is translated by the negative of a given  structure.
	 */
	static function op_Subtraction(pt:cs.system.drawing.Point, sz:cs.system.drawing.Size):cs.system.drawing.Point;
	/**
	 * Converts the specified  to a  object by rounding the  values to the nearest
	 * integer.
	 * @param value The  to convert.
	 * @return The  this method converts to.
	 */
	static function Round(value:cs.system.drawing.PointF):cs.system.drawing.Point;
	/**
	 * Returns the result of subtracting specified  from the specified .
	 * @param pt The  to be subtracted from.
	 * @param sz The  to subtract from the .
	 * @return The  that is the result of the subtraction operation.
	 */
	static function Subtract(pt:cs.system.drawing.Point, sz:cs.system.drawing.Size):cs.system.drawing.Point;
	/**
	 * Converts the specified  to a  by truncating the values of the .
	 * @param value The  to convert.
	 * @return The  this method converts to.
	 */
	static function Truncate(value:cs.system.drawing.PointF):cs.system.drawing.Point;
	@:overload(function(other:cs.system.drawing.Point):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for this .
	 * @return An integer value that specifies a hash value for this .
	 */
	function GetHashCode():Int;
	@:overload(function(p:cs.system.drawing.Point):Void {})
	/**
	 * Translates this  by the specified .
	 * @param p The  used offset this .
	 */
	function Offset(dx:Int, dy:Int):Void;
	/**
	 * Converts this  to a human-readable string.
	 * @return A string that represents this .
	 */
	function ToString():String;
}
