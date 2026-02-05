package cs.system.drawing;

/** Stores an ordered pair of floating-point numbers, typically the width and height of a rectangle. */
@:native("System.Drawing.SizeF")
extern class SizeF extends cs.system.ValueType {
	/** Gets a  structure that has a  and  value of 0. */
	static var Empty(default, never):cs.system.drawing.SizeF;
	/**
	 * Gets or sets the vertical component of this  structure.
	 * @return The vertical component of this  structure, typically measured in pixels.
	 */
	var Height(default, default):Single;
	/**
	 * Gets a value that indicates whether this  structure has zero width and height.
	 * @return when this  structure has both a width and height of zero; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets or sets the horizontal component of this  structure.
	 * @return The horizontal component of this  structure, typically measured in
	 * pixels.
	 */
	var Width(default, default):Single;
	@:overload(function(pt:cs.system.drawing.PointF):Void {})
	@:overload(function(size:cs.system.drawing.SizeF):Void {})
	function new(width:Single, height:Single):Void;
	/**
	 * Adds the width and height of one  structure to the width and height of another 
	 * structure.
	 * @param sz1 The first  structure to add.
	 * @param sz2 The second  structure to add.
	 * @return A  structure that is the result of the addition operation.
	 */
	static function Add(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):cs.system.drawing.SizeF;
	/**
	 * Adds the width and height of one  structure to the width and height of another 
	 * structure.
	 * @param sz1 The first  structure to add.
	 * @param sz2 The second  structure to add.
	 * @return A  structure that is the result of the addition operation.
	 */
	static function op_Addition(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):cs.system.drawing.SizeF;
	/**
	 * Divides the specified  by the specified single-precision floating-point number.
	 * @param left The dividend.
	 * @param right The divisor.
	 * @return The result of dividing 's width and height by .
	 */
	static function op_Division(left:cs.system.drawing.SizeF, right:Single):cs.system.drawing.SizeF;
	/**
	 * Tests whether two  structures are equal.
	 * @param sz1 The  structure on the left side of the equality operator.
	 * @param sz2 The  structure on the right of the equality operator.
	 * @return if  and  have equal width and height; otherwise, .
	 */
	static function op_Equality(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):Bool;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param size The  structure to be converted
	 * @return The  structure to which this operator converts.
	 */
	static function op_Explicit(size:cs.system.drawing.SizeF):cs.system.drawing.PointF;
	/**
	 * Tests whether two  structures are different.
	 * @param sz1 The  structure on the left of the inequality operator.
	 * @param sz2 The  structure on the right of the inequality operator.
	 * @return if  and  differ either in width or height;  if  and  are equal.
	 */
	static function op_Inequality(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):Bool;
	@:overload(function(left:cs.system.drawing.SizeF, right:Single):cs.system.drawing.SizeF {})
	/**
	 * Multiplies the specified  by the specified single-precision floating-point
	 * number.
	 * @param left The multiplicand.
	 * @param right The multiplier.
	 * @return The result of multiplying 's width and height by .
	 */
	static function op_Multiply(left:Single, right:cs.system.drawing.SizeF):cs.system.drawing.SizeF;
	/**
	 * Subtracts the width and height of one  structure from the width and height of
	 * another  structure.
	 * @param sz1 The  structure on the left side of the subtraction operator.
	 * @param sz2 The  structure on the right side of the subtraction operator.
	 * @return A  that is the result of the subtraction operation.
	 */
	static function op_Subtraction(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):cs.system.drawing.SizeF;
	/**
	 * Subtracts the width and height of one  structure from the width and height of
	 * another  structure.
	 * @param sz1 The  structure on the left side of the subtraction operator.
	 * @param sz2 The  structure on the right side of the subtraction operator.
	 * @return A  structure that is a result of the subtraction operation.
	 */
	static function Subtract(sz1:cs.system.drawing.SizeF, sz2:cs.system.drawing.SizeF):cs.system.drawing.SizeF;
	@:overload(function(other:cs.system.drawing.SizeF):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for this  structure.
	 * @return An integer value that specifies a hash value for this  structure.
	 */
	function GetHashCode():Int;
	/**
	 * Converts a  structure to a  structure.
	 * @return A  structure.
	 */
	function ToPointF():cs.system.drawing.PointF;
	/**
	 * Converts a  structure to a  structure.
	 * @return A  structure.
	 */
	function ToSize():cs.system.drawing.Size;
	/**
	 * Creates a human-readable string that represents this  structure.
	 * @return A string that represents this  structure.
	 */
	function ToString():String;
}
