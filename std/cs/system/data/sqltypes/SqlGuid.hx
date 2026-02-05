package cs.system.data.sqltypes;

/** Represents a GUID to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlGuid")
extern class SqlGuid extends cs.system.ValueType {
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlGuid;
	/**
	 * Gets a Boolean value that indicates whether this  structure is null.
	 * @return if . Otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the value of the  structure. This property is read-only.
	 * @return A  structure.
	 */
	var Value(default, never):cs.system.Guid;
	@:overload(function(value:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(g:cs.system.Guid):Void {})
	@:overload(function(s:String):Void {})
	function new(a:Int, b:cs.Int16, c:cs.Int16, d:cs.UInt8, e:cs.UInt8, f:cs.UInt8, g:cs.UInt8, h:cs.UInt8, i:cs.UInt8, j:cs.UInt8, k:cs.UInt8):Void;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet A .
	 * @return A  value that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison on two  structures to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlGuid {})
	@:overload(function(x:cs.system.data.sqltypes.SqlGuid):cs.system.Guid {})
	/**
	 * Converts the  parameter to .
	 * @param x A  object.
	 * @return A new  whose  is equal to the  of the  parameter.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlString):cs.system.data.sqltypes.SqlGuid;
	/**
	 * Compares two instances of  to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is greater than or
	 * equal to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the supplied  parameter to .
	 * @param x A .
	 * @return A new  whose  is equal to the  parameter.
	 */
	static function op_Implicit(x:cs.system.Guid):cs.system.data.sqltypes.SqlGuid;
	/**
	 * Performs a logical comparison on two  structures to determine whether they are
	 * not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two instances of  to determine whether the first is less than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise, . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlGuid, y:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts the specified  structure to .
	 * @param s The  to be parsed.
	 * @return A  equivalent to the value that is contained in the specified .
	 */
	static function Parse(s:String):cs.system.data.sqltypes.SqlGuid;
	@:overload(function(value:cs.system.data.sqltypes.SqlGuid):Int {})
	/**
	 * Compares this  structure to the supplied  and returns an indication of their
	 * relative values. Compares more than the last 6 bytes, but treats the last 6
	 * bytes as the most significant ones in comparisons.
	 * @param value The  to be compared.
	 * @return A signed number that indicates the relative values of the instance and
	 * the object. Return Value Condition Less than zero This instance is less than
	 * object. Zero This instance is the same as object. Greater than zero This
	 * instance is greater than object -or- object is a null reference ().
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Performs a logical comparison of two  structures to determine whether they are
	 * equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code of this  structure.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this  structure to a byte array.
	 * @return An array of bytes representing the  of this  structure.
	 */
	function ToByteArray():cs.NativeArray<cs.UInt8>;
	/**
	 * Converts this  structure to .
	 * @return A  structure that contains the bytes in the  structure.
	 */
	function ToSqlBinary():cs.system.data.sqltypes.SqlBinary;
	/**
	 * Converts this  structure to .
	 * @return A  structure that contains the string representation of the  structure.
	 */
	function ToSqlString():cs.system.data.sqltypes.SqlString;
	/**
	 * Converts this  structure to a .
	 * @return A  that contains the string representation of the  structure.
	 */
	function ToString():String;
}
