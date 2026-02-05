package cs.system;

/** Supports a value type that can be assigned . This class cannot be inherited. */
@:native("System.Nullable")
extern class Nullable {
	/**
	 * Compares the relative values of two  objects.
	 * @param T The underlying value type of the  and  parameters.
	 * @param n1 A  object.
	 * @param n2 A  object.
	 * @return An integer that indicates the relative values of the  and  parameters.
	 * Return Value Description Less than zero The  property for  is , and the 
	 * property for  is . -or- The  properties for  and  are , and the value of the 
	 * property for  is less than the value of the  property for . Zero The  properties
	 * for  and  are . -or- The  properties for  and  are , and the value of the 
	 * property for  is equal to the value of the  property for . Greater than zero The
	 * property for  is , and the  property for  is . -or- The  properties for  and 
	 * are , and the value of the  property for  is greater than the value of the 
	 * property for .
	 */
	static function Compare<T>(n1:Null<T>, n2:Null<T>):Int;
	/**
	 * Indicates whether two specified  objects are equal.
	 * @param T The underlying value type of the  and  parameters.
	 * @param n1 A  object.
	 * @param n2 A  object.
	 * @return if the  parameter is equal to the  parameter; otherwise, . The return
	 * value depends on the  and  properties of the two parameters that are compared.
	 * Return Value Description The  properties for  and  are . -or- The  properties
	 * for  and  are , and the  properties of the parameters are equal. The  property
	 * is  for one parameter and  for the other parameter. -or- The  properties for 
	 * and  are , and the  properties of the parameters are unequal.
	 */
	static function Equals<T>(n1:Null<T>, n2:Null<T>):Bool;
	/**
	 * Returns the underlying type argument of the specified nullable type.
	 * @param nullableType A  object that describes a closed generic nullable type.
	 * @return The type argument of the  parameter, if the  parameter is a closed
	 * generic nullable type; otherwise, .
	 */
	static function GetUnderlyingType(nullableType:cs.system.Type):cs.system.Type;
}
