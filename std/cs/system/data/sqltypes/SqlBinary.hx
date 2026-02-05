package cs.system.data.sqltypes;

/** Represents a variable-length stream of binary data to be stored in or retrieved from a database. */
@:native("System.Data.SqlTypes.SqlBinary")
extern class SqlBinary extends cs.system.ValueType {
	/** Represents a  that can be assigned to this instance of the  structure. */
	static var Null(default, never):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Indicates whether this  structure is null. This property is read-only.
	 * @return if ; otherwise, .
	 */
	var IsNull(default, never):Bool;
	/**
	 * Gets the length in bytes of the  property. This property is read-only.
	 * @return The length of the binary data in the  property.
	 */
	var Length(default, never):Int;
	/**
	 * Gets the value of the  structure. This property is read-only.
	 * @return The value of the  structure.
	 */
	var Value(default, never):cs.NativeArray<cs.UInt8>;
	@:native("get_Item")
	function get_Item(index0:Int):cs.UInt8;
	function new(value:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Concatenates two specified  values to create a new  structure.
	 * @param x A .
	 * @param y A .
	 * @return A  that is the concatenated value of x and y.
	 */
	static function Add(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Concatenates two  structures to create a new  structure.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return The concatenated values of the  and  parameters.
	 */
	static function Concat(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	static function Equals(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Returns the XML Schema definition language (XSD) of the specified .
	 * @param schemaSet An .
	 * @return A  that indicates the XSD of the specified .
	 */
	static function GetXsdType(schemaSet:cs.system.xml.schema.XmlSchemaSet):cs.system.xml.XmlQualifiedName;
	/**
	 * Compares two  structures to determine whether the first is greater than the
	 * second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThan(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function LessThan(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than or equal to
	 * the second.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function LessThanOrEqual(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether they are not equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function NotEquals(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Concatenates the two  parameters to create a new  structure.
	 * @param x A  object.
	 * @param y A  object.
	 * @return The concatenated values of the  and  parameters.
	 */
	static function op_Addition(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the two instances are equal or  if the two instances are
	 * not equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Equality(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(x:cs.system.data.sqltypes.SqlBinary):cs.NativeArray<cs.UInt8> {})
	/**
	 * Converts a  structure to a  array.
	 * @param x The  structure to be converted.
	 * @return A  array.
	 */
	static function op_Explicit(x:cs.system.data.sqltypes.SqlGuid):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Compares two  structures to determine whether the first is greater than the
	 * second.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the first instance is greater than the second instance.
	 * Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThan(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is greater than or equal
	 * to the second.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the first instance is greater than or equal to the second
	 * instance. Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function op_GreaterThanOrEqual(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Converts an array of bytes to a  structure.
	 * @param x The array of bytes to be converted.
	 * @return A  structure that represents the converted array of bytes.
	 */
	static function op_Implicit(x:cs.NativeArray<cs.UInt8>):cs.system.data.sqltypes.SqlBinary;
	/**
	 * Compares two  structures to determine whether they are not equal.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the two instances are not equal or  if the two instances
	 * are equal. If either instance of  is null, the  of the  will be .
	 */
	static function op_Inequality(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than the second.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the first instance is less than the second instance.
	 * Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThan(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	/**
	 * Compares two  structures to determine whether the first is less than or equal to
	 * the second.
	 * @param x A  object.
	 * @param y A  object.
	 * @return A  that is  if the first instance is less than or equal to the second
	 * instance. Otherwise . If either instance of  is null, the  of the  will be .
	 */
	static function op_LessThanOrEqual(x:cs.system.data.sqltypes.SqlBinary, y:cs.system.data.sqltypes.SqlBinary):cs.system.data.sqltypes.SqlBoolean;
	@:overload(function(value:cs.system.data.sqltypes.SqlBinary):Int {})
	/**
	 * Compares this  object to the supplied  object and returns an indication of their
	 * relative values.
	 * @param value The  object to be compared to this  structure.
	 * @return A signed number that indicates the relative values of this  structure
	 * and the object. Return value Condition Less than zero The value of this  object
	 * is less than the object. Zero This  object is the same as object. Greater than
	 * zero This  object is greater than object. -or- The object is a null reference.
	 */
	function CompareTo(value:Dynamic):Int;
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param x A  structure.
	 * @param y A  structure.
	 * @return if the two values are equal. Otherwise, . If either instance is null,
	 * then the  will be null.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this  structure.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Converts this instance of  to .
	 * @return A  structure.
	 */
	function ToSqlGuid():cs.system.data.sqltypes.SqlGuid;
	/**
	 * Converts this  object to a string.
	 * @return A string that contains the  of the . If the  is null the string will
	 * contain "null".
	 */
	function ToString():String;
}
