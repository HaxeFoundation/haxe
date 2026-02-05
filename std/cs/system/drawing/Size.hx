package cs.system.drawing;

/** Stores an ordered pair of integers, which specify a  and . */
@:native("System.Drawing.Size")
extern class Size extends cs.system.ValueType {
	/** Gets a  structure that has a  and  value of 0. */
	static var Empty(default, never):cs.system.drawing.Size;
	/**
	 * Gets or sets the vertical component of this  structure.
	 * @return The vertical component of this  structure, typically measured in pixels.
	 */
	var Height(default, default):Int;
	/**
	 * Tests whether this  structure has width and height of 0.
	 * @return This property returns  when this  structure has both a width and height
	 * of 0; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets or sets the horizontal component of this  structure.
	 * @return The horizontal component of this  structure, typically measured in
	 * pixels.
	 */
	var Width(default, default):Int;
	@:overload(function(pt:cs.system.drawing.Point):Void {})
	function new(width:Int, height:Int):Void;
	/**
	 * Adds the width and height of one  structure to the width and height of another 
	 * structure.
	 * @param sz1 The first  structure to add.
	 * @param sz2 The second  structure to add.
	 * @return A  structure that is the result of the addition operation.
	 */
	static function Add(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):cs.system.drawing.Size;
	/**
	 * Converts the specified  structure to a  structure by rounding the values of the 
	 * structure to the next higher integer values.
	 * @param value The  structure to convert.
	 * @return The  structure this method converts to.
	 */
	static function Ceiling(value:cs.system.drawing.SizeF):cs.system.drawing.Size;
	/**
	 * Adds the width and height of one  structure to the width and height of another 
	 * structure.
	 * @param sz1 The first  to add.
	 * @param sz2 The second  to add.
	 * @return A  structure that is the result of the addition operation.
	 */
	static function op_Addition(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):cs.system.drawing.Size;
	@:overload(function(left:cs.system.drawing.Size, right:Int):cs.system.drawing.Size {})
	/**
	 * Divides the specified  by the specified integer.
	 * @param left The dividend.
	 * @param right The divisor.
	 * @return A new , which contains the result of dividing 's height by  and 's width
	 * by , respectively.
	 */
	static function op_Division(left:cs.system.drawing.Size, right:Single):cs.system.drawing.SizeF;
	/**
	 * Tests whether two  structures are equal.
	 * @param sz1 The  structure on the left side of the equality operator.
	 * @param sz2 The  structure on the right of the equality operator.
	 * @return if  and  have equal width and height; otherwise, .
	 */
	static function op_Equality(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):Bool;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param size The  structure to convert.
	 * @return The  structure to which this operator converts.
	 */
	static function op_Explicit(size:cs.system.drawing.Size):cs.system.drawing.Point;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param p The  structure to convert.
	 * @return The  structure to which this operator converts.
	 */
	static function op_Implicit(p:cs.system.drawing.Size):cs.system.drawing.SizeF;
	/**
	 * Tests whether two  structures are different.
	 * @param sz1 The  structure on the left of the inequality operator.
	 * @param sz2 The  structure on the right of the inequality operator.
	 * @return if  and  differ either in width or height;  if  and  are equal.
	 */
	static function op_Inequality(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):Bool;
	@:overload(function(left:cs.system.drawing.Size, right:Int):cs.system.drawing.Size {})
	@:overload(function(left:cs.system.drawing.Size, right:Single):cs.system.drawing.SizeF {})
	@:overload(function(left:Int, right:cs.system.drawing.Size):cs.system.drawing.Size {})
	/**
	 * Multiplies the specified  by the specified integer.
	 * @param left The multiplicand.
	 * @param right The multiplier.
	 * @return The result of multiplying 's width and height by .
	 */
	static function op_Multiply(left:Single, right:cs.system.drawing.Size):cs.system.drawing.SizeF;
	/**
	 * Subtracts the width and height of one  structure from the width and height of
	 * another  structure.
	 * @param sz1 The  structure on the left side of the subtraction operator.
	 * @param sz2 The  structure on the right side of the subtraction operator.
	 * @return A  structure that is the result of the subtraction operation.
	 */
	static function op_Subtraction(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):cs.system.drawing.Size;
	/**
	 * Converts the specified  structure to a  structure by rounding the values of the 
	 * structure to the nearest integer values.
	 * @param value The  structure to convert.
	 * @return The  structure this method converts to.
	 */
	static function Round(value:cs.system.drawing.SizeF):cs.system.drawing.Size;
	/**
	 * Subtracts the width and height of one  structure from the width and height of
	 * another  structure.
	 * @param sz1 The  structure on the left side of the subtraction operator.
	 * @param sz2 The  structure on the right side of the subtraction operator.
	 * @return A  structure that is a result of the subtraction operation.
	 */
	static function Subtract(sz1:cs.system.drawing.Size, sz2:cs.system.drawing.Size):cs.system.drawing.Size;
	/**
	 * Converts the specified  structure to a  structure by truncating the values of
	 * the  structure to the next lower integer values.
	 * @param value The  structure to convert.
	 * @return The  structure this method converts to.
	 */
	static function Truncate(value:cs.system.drawing.SizeF):cs.system.drawing.Size;
	@:overload(function(other:cs.system.drawing.Size):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for this  structure.
	 * @return An integer value that specifies a hash value for this  structure.
	 */
	function GetHashCode():Int;
	/**
	 * Creates a human-readable string that represents this  structure.
	 * @return A string that represents this .
	 */
	function ToString():String;
}
