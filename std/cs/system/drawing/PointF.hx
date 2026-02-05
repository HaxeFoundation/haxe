package cs.system.drawing;

/** Represents an ordered pair of floating-point x- and y-coordinates that defines a point in a two-dimensional plane. */
@:native("System.Drawing.PointF")
extern class PointF extends cs.system.ValueType {
	/** Represents a new instance of the  class with member data left uninitialized. */
	static var Empty(default, never):cs.system.drawing.PointF;
	/**
	 * Gets a value indicating whether this  is empty.
	 * @return if both  and  are 0; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets or sets the x-coordinate of this .
	 * @return The x-coordinate of this .
	 */
	var X(default, default):Single;
	/**
	 * Gets or sets the y-coordinate of this .
	 * @return The y-coordinate of this .
	 */
	var Y(default, default):Single;
	function new(x:Single, y:Single):Void;
	@:overload(function(pt:cs.system.drawing.PointF, sz:cs.system.drawing.Size):cs.system.drawing.PointF {})
	/**
	 * Translates a given  by the specified .
	 * @param pt The  to translate.
	 * @param sz The  that specifies the numbers to add to the coordinates of .
	 * @return The translated .
	 */
	static function Add(pt:cs.system.drawing.PointF, sz:cs.system.drawing.SizeF):cs.system.drawing.PointF;
	@:overload(function(pt:cs.system.drawing.PointF, sz:cs.system.drawing.Size):cs.system.drawing.PointF {})
	/**
	 * Translates a  by a given .
	 * @param pt The  to translate.
	 * @param sz A  that specifies the pair of numbers to add to the coordinates of .
	 * @return The translated .
	 */
	static function op_Addition(pt:cs.system.drawing.PointF, sz:cs.system.drawing.SizeF):cs.system.drawing.PointF;
	/**
	 * Compares two  structures. The result specifies whether the values of the  and 
	 * properties of the two  structures are equal.
	 * @param left A  to compare.
	 * @param right A  to compare.
	 * @return if the  and  values of the left and right  structures are equal;
	 * otherwise, .
	 */
	static function op_Equality(left:cs.system.drawing.PointF, right:cs.system.drawing.PointF):Bool;
	/**
	 * Determines whether the coordinates of the specified points are not equal.
	 * @param left A  to compare.
	 * @param right A  to compare.
	 * @return to indicate the  and  values of  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.drawing.PointF, right:cs.system.drawing.PointF):Bool;
	@:overload(function(pt:cs.system.drawing.PointF, sz:cs.system.drawing.Size):cs.system.drawing.PointF {})
	/**
	 * Translates a  by the negative of a given .
	 * @param pt The  to translate.
	 * @param sz The  that specifies the numbers to subtract from the coordinates of .
	 * @return The translated .
	 */
	static function op_Subtraction(pt:cs.system.drawing.PointF, sz:cs.system.drawing.SizeF):cs.system.drawing.PointF;
	@:overload(function(pt:cs.system.drawing.PointF, sz:cs.system.drawing.Size):cs.system.drawing.PointF {})
	/**
	 * Translates a  by the negative of a specified size.
	 * @param pt The  to translate.
	 * @param sz The  that specifies the numbers to subtract from the coordinates of .
	 * @return The translated .
	 */
	static function Subtract(pt:cs.system.drawing.PointF, sz:cs.system.drawing.SizeF):cs.system.drawing.PointF;
	@:overload(function(other:cs.system.drawing.PointF):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for this  structure.
	 * @return An integer value that specifies a hash value for this  structure.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  to a human readable string.
	 * @return A string that represents this .
	 */
	function ToString():String;
}
