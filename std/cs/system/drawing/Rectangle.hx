package cs.system.drawing;

/** Stores a set of four integers that represent the location and size of a rectangle */
@:native("System.Drawing.Rectangle")
extern class Rectangle extends cs.system.ValueType {
	/** Represents a  structure with its properties left uninitialized. */
	static var Empty(default, never):cs.system.drawing.Rectangle;
	/**
	 * Gets the y-coordinate that is the sum of the  and  property values of this 
	 * structure.
	 * @return The y-coordinate that is the sum of  and  of this .
	 */
	var Bottom(default, never):Int;
	/**
	 * Gets or sets the height of this  structure.
	 * @return The height of this  structure. The default is 0.
	 */
	var Height(default, default):Int;
	/**
	 * Tests whether all numeric properties of this  have values of zero.
	 * @return This property returns  if the , , , and  properties of this  all have
	 * values of zero; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets the x-coordinate of the left edge of this  structure.
	 * @return The x-coordinate of the left edge of this  structure.
	 */
	var Left(default, never):Int;
	/**
	 * Gets or sets the coordinates of the upper-left corner of this  structure.
	 * @return A  that represents the upper-left corner of this  structure.
	 */
	var Location(default, default):cs.system.drawing.Point;
	/**
	 * Gets the x-coordinate that is the sum of  and  property values of this 
	 * structure.
	 * @return The x-coordinate that is the sum of  and  of this .
	 */
	var Right(default, never):Int;
	/**
	 * Gets or sets the size of this .
	 * @return A  that represents the width and height of this  structure.
	 */
	var Size(default, default):cs.system.drawing.Size;
	/**
	 * Gets the y-coordinate of the top edge of this  structure.
	 * @return The y-coordinate of the top edge of this  structure.
	 */
	var Top(default, never):Int;
	/**
	 * Gets or sets the width of this  structure.
	 * @return The width of this  structure. The default is 0.
	 */
	var Width(default, default):Int;
	/**
	 * Gets or sets the x-coordinate of the upper-left corner of this  structure.
	 * @return The x-coordinate of the upper-left corner of this  structure. The
	 * default is 0.
	 */
	var X(default, default):Int;
	/**
	 * Gets or sets the y-coordinate of the upper-left corner of this  structure.
	 * @return The y-coordinate of the upper-left corner of this  structure. The
	 * default is 0.
	 */
	var Y(default, default):Int;
	@:overload(function(location:cs.system.drawing.Point, size:cs.system.drawing.Size):Void {})
	function new(x:Int, y:Int, width:Int, height:Int):Void;
	/**
	 * Converts the specified  structure to a  structure by rounding the  values to the
	 * next higher integer values.
	 * @param value The  structure to be converted.
	 * @return Returns a .
	 */
	static function Ceiling(value:cs.system.drawing.RectangleF):cs.system.drawing.Rectangle;
	/**
	 * Creates a  structure with the specified edge locations.
	 * @param left The x-coordinate of the upper-left corner of this  structure.
	 * @param top The y-coordinate of the upper-left corner of this  structure.
	 * @param right The x-coordinate of the lower-right corner of this  structure.
	 * @param bottom The y-coordinate of the lower-right corner of this  structure.
	 * @return The new  that this method creates.
	 */
	static function FromLTRB(left:Int, top:Int, right:Int, bottom:Int):cs.system.drawing.Rectangle;
	/**
	 * Creates and returns an enlarged copy of the specified  structure. The copy is
	 * enlarged by the specified amount. The original  structure remains unmodified.
	 * @param rect The  with which to start. This rectangle is not modified.
	 * @param x The amount to inflate this  horizontally.
	 * @param y The amount to inflate this  vertically.
	 * @return The enlarged .
	 */
	static function Inflate(rect:cs.system.drawing.Rectangle, x:Int, y:Int):cs.system.drawing.Rectangle;
	/**
	 * Replaces this  with the intersection of itself and the specified .
	 * @param rect The  with which to intersect.
	 */
	static function Intersect(a:cs.system.drawing.Rectangle, b:cs.system.drawing.Rectangle):cs.system.drawing.Rectangle;
	/**
	 * Tests whether two  structures have equal location and size.
	 * @param left The  structure that is to the left of the equality operator.
	 * @param right The  structure that is to the right of the equality operator.
	 * @return This operator returns  if the two  structures have equal , , , and 
	 * properties.
	 */
	static function op_Equality(left:cs.system.drawing.Rectangle, right:cs.system.drawing.Rectangle):Bool;
	/**
	 * Tests whether two  structures differ in location or size.
	 * @param left The  structure that is to the left of the inequality operator.
	 * @param right The  structure that is to the right of the inequality operator.
	 * @return This operator returns  if any of the , ,  or  properties of the two 
	 * structures are unequal; otherwise .
	 */
	static function op_Inequality(left:cs.system.drawing.Rectangle, right:cs.system.drawing.Rectangle):Bool;
	/**
	 * Converts the specified  to a  by rounding the  values to the nearest integer
	 * values.
	 * @param value The  to be converted.
	 * @return The rounded integer value of the .
	 */
	static function Round(value:cs.system.drawing.RectangleF):cs.system.drawing.Rectangle;
	/**
	 * Converts the specified  to a  by truncating the  values.
	 * @param value The  to be converted.
	 * @return The truncated value of the  .
	 */
	static function Truncate(value:cs.system.drawing.RectangleF):cs.system.drawing.Rectangle;
	/**
	 * Gets a  structure that contains the union of two  structures.
	 * @param a A rectangle to union.
	 * @param b A rectangle to union.
	 * @return A  structure that bounds the union of the two  structures.
	 */
	static function Union(a:cs.system.drawing.Rectangle, b:cs.system.drawing.Rectangle):cs.system.drawing.Rectangle;
	@:overload(function(pt:cs.system.drawing.Point):Bool {})
	@:overload(function(rect:cs.system.drawing.Rectangle):Bool {})
	/**
	 * Determines if the specified point is contained within this  structure.
	 * @param pt The  to test.
	 * @return This method returns  if the point represented by  is contained within
	 * this  structure; otherwise .
	 */
	function Contains(x:Int, y:Int):Bool;
	@:overload(function(other:cs.system.drawing.Rectangle):Bool {})
	/** @param other  */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this  structure. For information about the use of hash
	 * codes, see  .
	 * @return An integer that represents the hash code for this rectangle.
	 */
	function GetHashCode():Int;
	@:overload(function(size:cs.system.drawing.Size):Void {})
	/**
	 * Creates and returns an enlarged copy of the specified  structure. The copy is
	 * enlarged by the specified amount. The original  structure remains unmodified.
	 * @param rect The  with which to start. This rectangle is not modified.
	 * @param x The amount to inflate this  horizontally.
	 * @param y The amount to inflate this  vertically.
	 * @return The enlarged .
	 */
	function Inflate(width:Int, height:Int):Void;
	/**
	 * Replaces this  with the intersection of itself and the specified .
	 * @param rect The  with which to intersect.
	 */
	function Intersect(rect:cs.system.drawing.Rectangle):Void;
	/**
	 * Determines if this rectangle intersects with .
	 * @param rect The rectangle to test.
	 * @return This method returns  if there is any intersection, otherwise .
	 */
	function IntersectsWith(rect:cs.system.drawing.Rectangle):Bool;
	@:overload(function(pos:cs.system.drawing.Point):Void {})
	/**
	 * Adjusts the location of this rectangle by the specified amount.
	 * @param pos Amount to offset the location.
	 */
	function Offset(x:Int, y:Int):Void;
	/**
	 * Converts the attributes of this  to a human-readable string.
	 * @return A string that contains the position, width, and height of this 
	 * structure ¾ for example, {X=20, Y=20, Width=100, Height=50}
	 */
	function ToString():String;
}
