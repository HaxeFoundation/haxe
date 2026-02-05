package cs.system.drawing;

/** Stores a set of four floating-point numbers that represent the location and size of a rectangle. For more advanced region functions, use a  object. */
@:native("System.Drawing.RectangleF")
extern class RectangleF extends cs.system.ValueType {
	/** Represents an instance of the  class with its members uninitialized. */
	static var Empty(default, never):cs.system.drawing.RectangleF;
	/**
	 * Gets the y-coordinate that is the sum of  and  of this  structure.
	 * @return The y-coordinate that is the sum of  and  of this  structure.
	 */
	var Bottom(default, never):Single;
	/**
	 * Gets or sets the height of this  structure.
	 * @return The height of this  structure. The default is 0.
	 */
	var Height(default, default):Single;
	/**
	 * Gets a value that indicates whether the  or  property of this  has a value of
	 * zero.
	 * @return if the  or  property of this  has a value of zero; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets the x-coordinate of the left edge of this  structure.
	 * @return The x-coordinate of the left edge of this  structure.
	 */
	var Left(default, never):Single;
	/**
	 * Gets or sets the coordinates of the upper-left corner of this  structure.
	 * @return A  that represents the upper-left corner of this  structure.
	 */
	var Location(default, default):cs.system.drawing.PointF;
	/**
	 * Gets the x-coordinate that is the sum of  and  of this  structure.
	 * @return The x-coordinate that is the sum of  and  of this  structure.
	 */
	var Right(default, never):Single;
	/**
	 * Gets or sets the size of this .
	 * @return A  that represents the width and height of this  structure.
	 */
	var Size(default, default):cs.system.drawing.SizeF;
	/**
	 * Gets the y-coordinate of the top edge of this  structure.
	 * @return The y-coordinate of the top edge of this  structure.
	 */
	var Top(default, never):Single;
	/**
	 * Gets or sets the width of this  structure.
	 * @return The width of this  structure. The default is 0.
	 */
	var Width(default, default):Single;
	/**
	 * Gets or sets the x-coordinate of the upper-left corner of this  structure.
	 * @return The x-coordinate of the upper-left corner of this  structure. The
	 * default is 0.
	 */
	var X(default, default):Single;
	/**
	 * Gets or sets the y-coordinate of the upper-left corner of this  structure.
	 * @return The y-coordinate of the upper-left corner of this  structure. The
	 * default is 0.
	 */
	var Y(default, default):Single;
	@:overload(function(location:cs.system.drawing.PointF, size:cs.system.drawing.SizeF):Void {})
	function new(x:Single, y:Single, width:Single, height:Single):Void;
	/**
	 * Creates a  structure with upper-left corner and lower-right corner at the
	 * specified locations.
	 * @param left The x-coordinate of the upper-left corner of the rectangular region.
	 * @param top The y-coordinate of the upper-left corner of the rectangular region.
	 * @param right The x-coordinate of the lower-right corner of the rectangular
	 * region.
	 * @param bottom The y-coordinate of the lower-right corner of the rectangular
	 * region.
	 * @return The new  that this method creates.
	 */
	static function FromLTRB(left:Single, top:Single, right:Single, bottom:Single):cs.system.drawing.RectangleF;
	/**
	 * Creates and returns an enlarged copy of the specified  structure. The copy is
	 * enlarged by the specified amount and the original rectangle remains unmodified.
	 * @param rect The  to be copied. This rectangle is not modified.
	 * @param x The amount to enlarge the copy of the rectangle horizontally.
	 * @param y The amount to enlarge the copy of the rectangle vertically.
	 * @return The enlarged .
	 */
	static function Inflate(rect:cs.system.drawing.RectangleF, x:Single, y:Single):cs.system.drawing.RectangleF;
	/**
	 * Replaces this  structure with the intersection of itself and the specified 
	 * structure.
	 * @param rect The rectangle to intersect.
	 */
	static function Intersect(a:cs.system.drawing.RectangleF, b:cs.system.drawing.RectangleF):cs.system.drawing.RectangleF;
	/**
	 * Tests whether two  structures have equal location and size.
	 * @param left The  structure that is to the left of the equality operator.
	 * @param right The  structure that is to the right of the equality operator.
	 * @return if the two specified  structures have equal , , , and  properties;
	 * otherwise, .
	 */
	static function op_Equality(left:cs.system.drawing.RectangleF, right:cs.system.drawing.RectangleF):Bool;
	/**
	 * Converts the specified  structure to a  structure.
	 * @param r The  structure to convert.
	 * @return The  structure that is converted from the specified  structure.
	 */
	static function op_Implicit(r:cs.system.drawing.Rectangle):cs.system.drawing.RectangleF;
	/**
	 * Tests whether two  structures differ in location or size.
	 * @param left The  structure that is to the left of the inequality operator.
	 * @param right The  structure that is to the right of the inequality operator.
	 * @return if any of the  , , , or  properties of the two  structures are unequal;
	 * otherwise, .
	 */
	static function op_Inequality(left:cs.system.drawing.RectangleF, right:cs.system.drawing.RectangleF):Bool;
	/**
	 * Creates the smallest possible third rectangle that can contain both of two
	 * rectangles that form a union.
	 * @param a A rectangle to union.
	 * @param b A rectangle to union.
	 * @return A third  structure that contains both of the two rectangles that form
	 * the union.
	 */
	static function Union(a:cs.system.drawing.RectangleF, b:cs.system.drawing.RectangleF):cs.system.drawing.RectangleF;
	@:overload(function(pt:cs.system.drawing.PointF):Bool {})
	@:overload(function(rect:cs.system.drawing.RectangleF):Bool {})
	/**
	 * Determines if the specified point is contained within this  structure.
	 * @param pt The  to test.
	 * @return if the point represented by the  parameter is contained within this 
	 * structure; otherwise, .
	 */
	function Contains(x:Single, y:Single):Bool;
	@:overload(function(other:cs.system.drawing.RectangleF):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Gets the hash code for this  structure. For information about the use of hash
	 * codes, see .
	 * @return The hash code for this .
	 */
	function GetHashCode():Int;
	@:overload(function(size:cs.system.drawing.SizeF):Void {})
	/**
	 * Creates and returns an enlarged copy of the specified  structure. The copy is
	 * enlarged by the specified amount and the original rectangle remains unmodified.
	 * @param rect The  to be copied. This rectangle is not modified.
	 * @param x The amount to enlarge the copy of the rectangle horizontally.
	 * @param y The amount to enlarge the copy of the rectangle vertically.
	 * @return The enlarged .
	 */
	function Inflate(x:Single, y:Single):Void;
	/**
	 * Replaces this  structure with the intersection of itself and the specified 
	 * structure.
	 * @param rect The rectangle to intersect.
	 */
	function Intersect(rect:cs.system.drawing.RectangleF):Void;
	/**
	 * Determines if this rectangle intersects with .
	 * @param rect The rectangle to test.
	 * @return if there is any intersection; otherwise, .
	 */
	function IntersectsWith(rect:cs.system.drawing.RectangleF):Bool;
	@:overload(function(pos:cs.system.drawing.PointF):Void {})
	/**
	 * Adjusts the location of this rectangle by the specified amount.
	 * @param pos The amount to offset the location.
	 */
	function Offset(x:Single, y:Single):Void;
	/**
	 * Converts the  and  of this  to a human-readable string.
	 * @return A string that contains the position, width, and height of this 
	 * structure. For example, "{X=20, Y=20, Width=100, Height=50}".
	 */
	function ToString():String;
}
