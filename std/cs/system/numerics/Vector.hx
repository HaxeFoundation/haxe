package cs.system.numerics;

/** Provides a collection of static convenience methods for creating, manipulating, combining, and converting generic vectors. */
@:native("System.Numerics.Vector")
extern class Vector {
	/**
	 * Gets a value that indicates whether vector operations are subject to hardware
	 * acceleration through JIT intrinsic support.
	 * @return if vector operations are subject to hardware acceleration; otherwise, .
	 */
	static var IsHardwareAccelerated(default, never):Bool;
	/**
	 * Returns a new vector whose elements are the absolute values of the given
	 * vector's elements.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The absolute value vector.
	 */
	static function Abs<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector whose values are the sum of each pair of elements from two
	 * given vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The summed vector.
	 */
	static function Add<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector by performing a bitwise And Not operation on each pair of
	 * corresponding elements in two vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The resulting vector.
	 */
	static function AndNot<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of unsigned
	 * bytes.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorByte<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt8>;
	/**
	 * Reinterprets the bits of a specified vector into those of a double-precision
	 * floating-point vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorDouble<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Float>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of 16-bit
	 * integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorInt16<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.Int16>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorInt32<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Int>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of long
	 * integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorInt64<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<haxe.Int64>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of signed
	 * bytes.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorSByte<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.Int8>;
	/**
	 * Reinterprets the bits of a specified vector into those of a single-precision
	 * floating-point vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorSingle<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<Single>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of unsigned
	 * 16-bit integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorUInt16<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt16>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of unsigned
	 * integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorUInt32<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt>;
	/**
	 * Reinterprets the bits of a specified vector into those of a vector of unsigned
	 * long integers.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The reinterpreted vector.
	 */
	static function AsVectorUInt64<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<cs.UInt64>;
	/**
	 * Returns a new vector by performing a bitwise  operation on each pair of elements
	 * in two vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The resulting vector.
	 */
	static function BitwiseAnd<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector by performing a bitwise  operation on each pair of elements
	 * in two vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The resulting vector.
	 */
	static function BitwiseOr<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function(condition:cs.system.numerics.Vector_1<Int>, left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Single> {})
	@:overload(function(condition:cs.system.numerics.Vector_1<haxe.Int64>, left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<Float> {})
	/**
	 * Creates a new single-precision vector with elements selected between two
	 * specified single-precision source vectors based on an integral mask vector.
	 * @param condition The integral mask vector used to drive selection.
	 * @param left The first source vector.
	 * @param right The second source vector.
	 * @return The new vector with elements selected based on the mask.
	 */
	static function ConditionalSelect<T>(condition:cs.system.numerics.Vector_1<T>, left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function(value:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<Float> {})
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToDouble(value:cs.system.numerics.Vector_1<cs.UInt64>):cs.system.numerics.Vector_1<Float>;
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToInt32(value:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int>;
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToInt64(value:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64>;
	@:overload(function(value:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Single> {})
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToSingle(value:cs.system.numerics.Vector_1<cs.UInt>):cs.system.numerics.Vector_1<Single>;
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToUInt32(value:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<cs.UInt>;
	/**
	 * Converts a  to a .
	 * @param value The source vector.
	 * @return The converted vector.
	 */
	static function ConvertToUInt64(value:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<cs.UInt64>;
	/**
	 * Returns a new vector whose values are the result of dividing the first vector's
	 * elements by the corresponding elements in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The divided vector.
	 */
	static function Divide<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns the dot product of two vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The dot product.
	 */
	static function Dot<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):T;
	@:overload(function(left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Int>, right:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(left:cs.system.numerics.Vector_1<haxe.Int64>, right:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int> {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in two
	 * specified double-precision vectors are equal.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	static function Equals<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a value that indicates whether each pair of elements in the given
	 * vectors is equal.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if all elements in  and  are equal; otherwise, .
	 */
	static function EqualsAll<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a value that indicates whether any single pair of elements in the given
	 * vectors is equal.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if any element pair in  and  is equal; otherwise, .
	 */
	static function EqualsAny<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	@:overload(function(left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Int>, right:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(left:cs.system.numerics.Vector_1<haxe.Int64>, right:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int> {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in one
	 * double-precision floating-point vector are greater than their corresponding
	 * elements in a second double-precision floating-point vector.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	static function GreaterThan<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a value that indicates whether all elements in the first vector are
	 * greater than the corresponding elements in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if all elements in  are greater than the corresponding elements in ;
	 * otherwise, .
	 */
	static function GreaterThanAll<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a value that indicates whether any element in the first vector is
	 * greater than the corresponding element in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if any element in  is greater than the corresponding element in ;
	 * otherwise,  .
	 */
	static function GreaterThanAny<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	@:overload(function(left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Int>, right:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(left:cs.system.numerics.Vector_1<haxe.Int64>, right:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int> {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in one
	 * vector are greater than or equal to their corresponding elements in the second
	 * double-precision floating-point vector.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	static function GreaterThanOrEqual<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a value that indicates whether all elements in the first vector are
	 * greater than or equal to all the corresponding elements in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if all elements in  are greater than or equal to the corresponding
	 * elements in ; otherwise, .
	 */
	static function GreaterThanOrEqualAll<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a value that indicates whether any element in the first vector is
	 * greater than or equal to the corresponding element in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if any element in  is greater than or equal to the corresponding element
	 * in ; otherwise,  .
	 */
	static function GreaterThanOrEqualAny<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	@:overload(function(left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Int>, right:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(left:cs.system.numerics.Vector_1<haxe.Int64>, right:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int> {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in one
	 * double-precision floating-point vector are less than their corresponding
	 * elements in a second double-precision floating-point vector.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	static function LessThan<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a value that indicates whether all of the elements in the first vector
	 * are less than their corresponding elements in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if all of the elements in  are less than the corresponding elements in ;
	 * otherwise,  .
	 */
	static function LessThanAll<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a value that indicates whether any element in the first vector is less
	 * than the corresponding element in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if any element in  is less than the corresponding element in ;
	 * otherwise,  .
	 */
	static function LessThanAny<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	@:overload(function(left:cs.system.numerics.Vector_1<Float>, right:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Int>, right:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(left:cs.system.numerics.Vector_1<haxe.Int64>, right:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<haxe.Int64> {})
	@:overload(function(left:cs.system.numerics.Vector_1<Single>, right:cs.system.numerics.Vector_1<Single>):cs.system.numerics.Vector_1<Int> {})
	/**
	 * Returns a new integral vector whose elements signal whether the elements in one
	 * double-precision floating-point vector are less than or equal to their
	 * corresponding elements in a second double-precision floating-point vector.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The resulting integral vector.
	 */
	static function LessThanOrEqual<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a value that indicates whether all elements in the first vector are less
	 * than or equal to their corresponding elements in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if all of the elements in  are less than or equal to the corresponding
	 * elements in ; otherwise,  .
	 */
	static function LessThanOrEqualAll<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a value that indicates whether any element in the first vector is less
	 * than or equal to the corresponding element in the second vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return if any element in  is less than or equal to the corresponding element in
	 * ; otherwise,  .
	 */
	static function LessThanOrEqualAny<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):Bool;
	/**
	 * Returns a new vector whose elements are the maximum of each pair of elements in
	 * the two given vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The maximum vector.
	 */
	static function Max<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector whose elements are the minimum of each pair of elements in
	 * the two given vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector to compare.
	 * @param right The second vector to compare.
	 * @return The minimum vector.
	 */
	static function Min<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T> {})
	@:overload(function<T>(left:cs.system.numerics.Vector_1<T>, right:T):cs.system.numerics.Vector_1<T> {})
	/**
	 * Returns a new vector whose values are a scalar value multiplied by each of the
	 * values of a specified vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The scalar value.
	 * @param right The vector.
	 * @return The scaled vector.
	 */
	static function Multiply<T>(left:T, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function(source1:cs.system.numerics.Vector_1<Float>, source2:cs.system.numerics.Vector_1<Float>):cs.system.numerics.Vector_1<Single> {})
	@:overload(function(source1:cs.system.numerics.Vector_1<cs.Int16>, source2:cs.system.numerics.Vector_1<cs.Int16>):cs.system.numerics.Vector_1<cs.Int8> {})
	@:overload(function(source1:cs.system.numerics.Vector_1<Int>, source2:cs.system.numerics.Vector_1<Int>):cs.system.numerics.Vector_1<cs.Int16> {})
	@:overload(function(source1:cs.system.numerics.Vector_1<haxe.Int64>, source2:cs.system.numerics.Vector_1<haxe.Int64>):cs.system.numerics.Vector_1<Int> {})
	@:overload(function(source1:cs.system.numerics.Vector_1<cs.UInt16>, source2:cs.system.numerics.Vector_1<cs.UInt16>):cs.system.numerics.Vector_1<cs.UInt8> {})
	@:overload(function(source1:cs.system.numerics.Vector_1<cs.UInt>, source2:cs.system.numerics.Vector_1<cs.UInt>):cs.system.numerics.Vector_1<cs.UInt16> {})
	/**
	 * Narrows two  instances into one .
	 * @param source1 The first source vector, whose elements become the lower-index
	 * elements of the return value.
	 * @param source2 The second source vector, whose elements become the higher-index
	 * elements of the return value.
	 * @return A  containing elements narrowed from the source vectors.
	 */
	static function Narrow(source1:cs.system.numerics.Vector_1<cs.UInt64>, source2:cs.system.numerics.Vector_1<cs.UInt64>):cs.system.numerics.Vector_1<cs.UInt>;
	/**
	 * Returns a new vector whose elements are the negation of the corresponding
	 * element in the specified vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The negated vector.
	 */
	static function Negate<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector whose elements are obtained by taking the one's complement
	 * of a specified vector's elements.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The resulting vector.
	 */
	static function OnesComplement<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector whose elements are the square roots of a specified vector's
	 * elements.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param value The source vector.
	 * @return The square root vector.
	 */
	static function SquareRoot<T>(value:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	/**
	 * Returns a new vector whose values are the difference between the elements in the
	 * second vector and their corresponding elements in the first vector.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The difference vector.
	 */
	static function Subtract<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
	@:overload(function(source:cs.system.numerics.Vector_1<cs.UInt8>, dest1:cs.Ref<cs.system.numerics.Vector_1<cs.UInt16>>, dest2:cs.Ref<cs.system.numerics.Vector_1<cs.UInt16>>):Void {})
	@:overload(function(source:cs.system.numerics.Vector_1<cs.Int16>, dest1:cs.Ref<cs.system.numerics.Vector_1<Int>>, dest2:cs.Ref<cs.system.numerics.Vector_1<Int>>):Void {})
	@:overload(function(source:cs.system.numerics.Vector_1<Int>, dest1:cs.Ref<cs.system.numerics.Vector_1<haxe.Int64>>, dest2:cs.Ref<cs.system.numerics.Vector_1<haxe.Int64>>):Void {})
	@:overload(function(source:cs.system.numerics.Vector_1<cs.Int8>, dest1:cs.Ref<cs.system.numerics.Vector_1<cs.Int16>>, dest2:cs.Ref<cs.system.numerics.Vector_1<cs.Int16>>):Void {})
	@:overload(function(source:cs.system.numerics.Vector_1<Single>, dest1:cs.Ref<cs.system.numerics.Vector_1<Float>>, dest2:cs.Ref<cs.system.numerics.Vector_1<Float>>):Void {})
	@:overload(function(source:cs.system.numerics.Vector_1<cs.UInt16>, dest1:cs.Ref<cs.system.numerics.Vector_1<cs.UInt>>, dest2:cs.Ref<cs.system.numerics.Vector_1<cs.UInt>>):Void {})
	/**
	 * Widens a  into two  instances.
	 * @param source The source vector whose elements are widened into the outputs.
	 * @param dest1 The first output vector, whose elements will contain the widened
	 * elements from lower indices in the source vector.
	 * @param dest2 The second output vector, whose elements will contain the widened
	 * elements from higher indices in the source vector.
	 */
	static function Widen(source:cs.system.numerics.Vector_1<cs.UInt>, dest1:cs.Ref<cs.system.numerics.Vector_1<cs.UInt64>>, dest2:cs.Ref<cs.system.numerics.Vector_1<cs.UInt64>>):Void;
	/**
	 * Returns a new vector by performing a bitwise exclusive Or () operation on each
	 * pair of elements in two vectors.
	 * @param T The vector type. T can be any primitive numeric type.
	 * @param left The first vector.
	 * @param right The second vector.
	 * @return The resulting vector.
	 */
	static function Xor<T>(left:cs.system.numerics.Vector_1<T>, right:cs.system.numerics.Vector_1<T>):cs.system.numerics.Vector_1<T>;
}
