package cs.system;

/** Provides methods for creating, manipulating, searching, and sorting arrays, thereby serving as the base class for all arrays in the common language runtime. */
@:native("System.Array")
extern class Array {
	/**
	 * Gets a value indicating whether the  has a fixed size.
	 * @return This property is always  for all arrays.
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return This property is always  for all arrays.
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return This property is always  for all arrays.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets the total number of elements in all the dimensions of the .
	 * @return The total number of elements in all the dimensions of the ; zero if
	 * there are no elements in the array.
	 */
	var Length(default, never):Int;
	/**
	 * Gets a 64-bit integer that represents the total number of elements in all the
	 * dimensions of the .
	 * @return A 64-bit integer that represents the total number of elements in all the
	 * dimensions of the .
	 */
	var LongLength(default, never):haxe.Int64;
	/**
	 * Gets the rank (number of dimensions) of the . For example, a one-dimensional
	 * array returns 1, a two-dimensional array returns 2, and so on.
	 * @return The rank (number of dimensions) of the .
	 */
	var Rank(default, never):Int;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Returns a read-only wrapper for the specified array.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based array to wrap in a read-only 
	 * wrapper.
	 * @return A read-only  wrapper for the specified array.
	 */
	static function AsReadOnly<T>(array:cs.NativeArray<T>):cs.system.collections.objectmodel.ReadOnlyCollection<T>;
	@:overload(function(array:cs.system.Array, value:Dynamic):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T):Int {})
	@:overload(function(array:cs.system.Array, value:Dynamic, comparer:cs.system.collections.IComparer):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T, comparer:cs.system.collections.generic.IComparer<T>):Int {})
	@:overload(function(array:cs.system.Array, index:Int, length:Int, value:Dynamic):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, index:Int, length:Int, value:T):Int {})
	@:overload(function(array:cs.system.Array, index:Int, length:Int, value:Dynamic, comparer:cs.system.collections.IComparer):Int {})
	/**
	 * Searches a range of elements in a one-dimensional sorted array for a value,
	 * using the  interface implemented by each element of the array and by the
	 * specified value.
	 * @param array The sorted one-dimensional  to search.
	 * @param index The starting index of the range to search.
	 * @param length The length of the range to search.
	 * @param value The object to search for.
	 * @return The index of the specified  in the specified , if  is found; otherwise,
	 * a negative number. If  is not found and  is less than one or more elements in ,
	 * the negative number returned is the bitwise complement of the index of the first
	 * element that is larger than . If  is not found and  is greater than all elements
	 * in , the negative number returned is the bitwise complement of (the index of the
	 * last element plus 1). If this method is called with a non-sorted , the return
	 * value can be incorrect and a negative number could be returned, even if  is
	 * present in .
	 */
	static function BinarySearch<T>(array:cs.NativeArray<T>, index:Int, length:Int, value:T, comparer:cs.system.collections.generic.IComparer<T>):Int;
	/**
	 * Sets a range of elements in an array to the default value of each element type.
	 * @param array The array whose elements need to be cleared.
	 * @param index The starting index of the range of elements to clear.
	 * @param length The number of elements to clear.
	 */
	static function Clear(array:cs.system.Array, index:Int, length:Int):Void;
	/**
	 * Copies a range of elements from an  starting at the specified source index and
	 * pastes them to another  starting at the specified destination index.  Guarantees
	 * that all changes are undone if the copy does not succeed completely.
	 * @param sourceArray The  that contains the data to copy.
	 * @param sourceIndex A 32-bit integer that represents the index in the  at which
	 * copying begins.
	 * @param destinationArray The  that receives the data.
	 * @param destinationIndex A 32-bit integer that represents the index in the  at
	 * which storing begins.
	 * @param length A 32-bit integer that represents the number of elements to copy.
	 */
	static function ConstrainedCopy(sourceArray:cs.system.Array, sourceIndex:Int, destinationArray:cs.system.Array, destinationIndex:Int, length:Int):Void;
	/**
	 * Converts an array of one type to an array of another type.
	 * @param TInput The type of the elements of the source array.
	 * @param TOutput The type of the elements of the target array.
	 * @param array The one-dimensional, zero-based  to convert to a target type.
	 * @param converter A  that converts each element from one type to another type.
	 * @return An array of the target type containing the converted elements from the
	 * source array.
	 */
	static function ConvertAll<TInput, TOutput>(array:cs.NativeArray<TInput>, converter:cs.system.Converter<TInput, TOutput>):cs.NativeArray<TOutput>;
	@:overload(function(sourceArray:cs.system.Array, destinationArray:cs.system.Array, length:Int):Void {})
	@:overload(function(sourceArray:cs.system.Array, destinationArray:cs.system.Array, length:haxe.Int64):Void {})
	@:overload(function(sourceArray:cs.system.Array, sourceIndex:Int, destinationArray:cs.system.Array, destinationIndex:Int, length:Int):Void {})
	/**
	 * Copies a range of elements from an  starting at the first element and pastes
	 * them into another  starting at the first element. The length is specified as a
	 * 32-bit integer.
	 * @param sourceArray The  that contains the data to copy.
	 * @param destinationArray The  that receives the data.
	 * @param length A 32-bit integer that represents the number of elements to copy.
	 */
	static function Copy(sourceArray:cs.system.Array, sourceIndex:haxe.Int64, destinationArray:cs.system.Array, destinationIndex:haxe.Int64, length:haxe.Int64):Void;
	@:overload(function(elementType:cs.system.Type, length:Int):cs.system.Array {})
	@:overload(function(elementType:cs.system.Type, lengths:cs.NativeArray<Int>):cs.system.Array {})
	@:overload(function(elementType:cs.system.Type, lengths:cs.NativeArray<haxe.Int64>):cs.system.Array {})
	@:overload(function(elementType:cs.system.Type, length1:Int, length2:Int):cs.system.Array {})
	@:overload(function(elementType:cs.system.Type, lengths:cs.NativeArray<Int>, lowerBounds:cs.NativeArray<Int>):cs.system.Array {})
	/**
	 * Creates a one-dimensional  of the specified  and length, with zero-based
	 * indexing.
	 * @param elementType The  of the  to create.
	 * @param length The size of the  to create.
	 * @return A new one-dimensional  of the specified  with the specified length,
	 * using zero-based indexing.
	 */
	static function CreateInstance(elementType:cs.system.Type, length1:Int, length2:Int, length3:Int):cs.system.Array;
	/**
	 * Returns an empty array.
	 * @param T The type of the elements of the array.
	 * @return An empty array.
	 */
	static function Empty<T>():cs.NativeArray<T>;
	/**
	 * Determines whether the specified array contains elements that match the
	 * conditions defined by the specified predicate.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to search.
	 * @param match The  that defines the conditions of the elements to search for.
	 * @return if  contains one or more elements that match the conditions defined by
	 * the specified predicate; otherwise, .
	 */
	static function Exists<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):Bool;
	@:overload(function<T>(array:cs.NativeArray<T>, value:T):Void {})
	/**
	 * @param T 
	 * @param array 
	 * @param value 
	 */
	static function Fill<T>(array:cs.NativeArray<T>, value:T, startIndex:Int, count:Int):Void;
	/**
	 * Searches for an element that matches the conditions defined by the specified
	 * predicate, and returns the first occurrence within the entire .
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based array to search.
	 * @param match The predicate that defines the conditions of the element to search
	 * for.
	 * @return The first element that matches the conditions defined by the specified
	 * predicate, if found; otherwise, the default value for type .
	 */
	static function Find<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):T;
	/**
	 * Retrieves all the elements that match the conditions defined by the specified
	 * predicate.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to search.
	 * @param match The  that defines the conditions of the elements to search for.
	 * @return An  containing all the elements that match the conditions defined by the
	 * specified predicate, if found; otherwise, an empty .
	 */
	static function FindAll<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):cs.NativeArray<T>;
	@:overload(function<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, startIndex:Int, match:cs.system.Predicate<T>):Int {})
	/**
	 * Searches for an element that matches the conditions defined by the specified
	 * predicate, and returns the zero-based index of the first occurrence within the
	 * range of elements in the  that starts at the specified index and contains the
	 * specified number of elements.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to search.
	 * @param startIndex The zero-based starting index of the search.
	 * @param count The number of elements in the section to search.
	 * @param match The  that defines the conditions of the element to search for.
	 * @return The zero-based index of the first occurrence of an element that matches
	 * the conditions defined by , if found; otherwise, -1.
	 */
	static function FindIndex<T>(array:cs.NativeArray<T>, startIndex:Int, count:Int, match:cs.system.Predicate<T>):Int;
	/**
	 * Searches for an element that matches the conditions defined by the specified
	 * predicate, and returns the last occurrence within the entire .
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to search.
	 * @param match The  that defines the conditions of the element to search for.
	 * @return The last element that matches the conditions defined by the specified
	 * predicate, if found; otherwise, the default value for type .
	 */
	static function FindLast<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):T;
	@:overload(function<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, startIndex:Int, match:cs.system.Predicate<T>):Int {})
	/**
	 * Searches for an element that matches the conditions defined by the specified
	 * predicate, and returns the zero-based index of the last occurrence within the
	 * range of elements in the  that contains the specified number of elements and
	 * ends at the specified index.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to search.
	 * @param startIndex The zero-based starting index of the backward search.
	 * @param count The number of elements in the section to search.
	 * @param match The  that defines the conditions of the element to search for.
	 * @return The zero-based index of the last occurrence of an element that matches
	 * the conditions defined by , if found; otherwise, -1.
	 */
	static function FindLastIndex<T>(array:cs.NativeArray<T>, startIndex:Int, count:Int, match:cs.system.Predicate<T>):Int;
	/**
	 * Performs the specified action on each element of the specified array.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  on whose elements the action is to
	 * be performed.
	 * @param action The  to perform on each element of .
	 */
	static function ForEach<T>(array:cs.NativeArray<T>, action:cs.system.Action_1<T>):Void;
	@:overload(function(array:cs.system.Array, value:Dynamic):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T):Int {})
	@:overload(function(array:cs.system.Array, value:Dynamic, startIndex:Int):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T, startIndex:Int):Int {})
	@:overload(function(array:cs.system.Array, value:Dynamic, startIndex:Int, count:Int):Int {})
	/**
	 * Searches for the specified object and returns the index of its first occurrence
	 * in a one-dimensional array.
	 * @param array The one-dimensional array to search.
	 * @param value The object to locate in .
	 * @return The index of the first occurrence of  in , if found; otherwise, the
	 * lower bound of the array minus 1.
	 */
	static function IndexOf<T>(array:cs.NativeArray<T>, value:T, startIndex:Int, count:Int):Int;
	@:overload(function(array:cs.system.Array, value:Dynamic):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T):Int {})
	@:overload(function(array:cs.system.Array, value:Dynamic, startIndex:Int):Int {})
	@:overload(function<T>(array:cs.NativeArray<T>, value:T, startIndex:Int):Int {})
	@:overload(function(array:cs.system.Array, value:Dynamic, startIndex:Int, count:Int):Int {})
	/**
	 * Searches for the specified object and returns the index of the last occurrence
	 * within the entire one-dimensional .
	 * @param array The one-dimensional  to search.
	 * @param value The object to locate in .
	 * @return The index of the last occurrence of  within the entire , if found;
	 * otherwise, the lower bound of the array minus 1.
	 */
	static function LastIndexOf<T>(array:cs.NativeArray<T>, value:T, startIndex:Int, count:Int):Int;
	/**
	 * Changes the number of elements of a one-dimensional array to the specified new
	 * size.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based array to resize, or  to create a
	 * new array with the specified size.
	 * @param newSize The size of the new array.
	 */
	static function Resize<T>(array:cs.Ref<cs.NativeArray<T>>, newSize:Int):Void;
	@:overload(function(array:cs.system.Array):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>):Void {})
	@:overload(function(array:cs.system.Array, index:Int, length:Int):Void {})
	/**
	 * Reverses the sequence of the elements in the entire one-dimensional .
	 * @param array The one-dimensional  to reverse.
	 */
	static function Reverse<T>(array:cs.NativeArray<T>, index:Int, length:Int):Void;
	@:overload(function(array:cs.system.Array):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>):Void {})
	@:overload(function(keys:cs.system.Array, items:cs.system.Array):Void {})
	@:overload(function(array:cs.system.Array, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>, comparer:cs.system.collections.generic.IComparer<T>):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>, comparison:cs.system.Comparison<T>):Void {})
	@:overload(function<TKey, TValue>(keys:cs.NativeArray<TKey>, items:cs.NativeArray<TValue>):Void {})
	@:overload(function(keys:cs.system.Array, items:cs.system.Array, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function(array:cs.system.Array, index:Int, length:Int):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>, index:Int, length:Int):Void {})
	@:overload(function<TKey, TValue>(keys:cs.NativeArray<TKey>, items:cs.NativeArray<TValue>, comparer:cs.system.collections.generic.IComparer<TKey>):Void {})
	@:overload(function(keys:cs.system.Array, items:cs.system.Array, index:Int, length:Int):Void {})
	@:overload(function(array:cs.system.Array, index:Int, length:Int, comparer:cs.system.collections.IComparer):Void {})
	@:overload(function<T>(array:cs.NativeArray<T>, index:Int, length:Int, comparer:cs.system.collections.generic.IComparer<T>):Void {})
	@:overload(function<TKey, TValue>(keys:cs.NativeArray<TKey>, items:cs.NativeArray<TValue>, index:Int, length:Int):Void {})
	@:overload(function(keys:cs.system.Array, items:cs.system.Array, index:Int, length:Int, comparer:cs.system.collections.IComparer):Void {})
	/**
	 * Sorts the elements in an entire one-dimensional  using the  implementation of
	 * each element of the .
	 * @param array The one-dimensional  to sort.
	 */
	static function Sort<TKey, TValue>(keys:cs.NativeArray<TKey>, items:cs.NativeArray<TValue>, index:Int, length:Int, comparer:cs.system.collections.generic.IComparer<TKey>):Void;
	/**
	 * Determines whether every element in the array matches the conditions defined by
	 * the specified predicate.
	 * @param T The type of the elements of the array.
	 * @param array The one-dimensional, zero-based  to check against the conditions.
	 * @param match The predicate that defines the conditions to check against the
	 * elements.
	 * @return if every element in  matches the conditions defined by the specified
	 * predicate; otherwise, . If there are no elements in the array, the return value
	 * is .
	 */
	static function TrueForAll<T>(array:cs.NativeArray<T>, match:cs.system.Predicate<T>):Bool;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	@:overload(function(array:cs.system.Array, index:Int):Void {})
	/**
	 * Copies all the elements of the current one-dimensional array to the specified
	 * one-dimensional array starting at the specified destination array index. The
	 * index is specified as a 32-bit integer.
	 * @param array The one-dimensional array that is the destination of the elements
	 * copied from the current array.
	 * @param index A 32-bit integer that represents the index in  at which copying
	 * begins.
	 */
	function CopyTo(array:cs.system.Array, index:haxe.Int64):Void;
	/**
	 * Returns an  for the .
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Gets a 32-bit integer that represents the number of elements in the specified
	 * dimension of the .
	 * @param dimension A zero-based dimension of the  whose length needs to be
	 * determined.
	 * @return A 32-bit integer that represents the number of elements in the specified
	 * dimension.
	 */
	function GetLength(dimension:Int):Int;
	/**
	 * Gets a 64-bit integer that represents the number of elements in the specified
	 * dimension of the .
	 * @param dimension A zero-based dimension of the  whose length needs to be
	 * determined.
	 * @return A 64-bit integer that represents the number of elements in the specified
	 * dimension.
	 */
	function GetLongLength(dimension:Int):haxe.Int64;
	/**
	 * Gets the index of the first element of the specified dimension in the array.
	 * @param dimension A zero-based dimension of the array whose starting index needs
	 * to be determined.
	 * @return The index of the first element of the specified dimension in the array.
	 */
	function GetLowerBound(dimension:Int):Int;
	/**
	 * Gets the index of the last element of the specified dimension in the array.
	 * @param dimension A zero-based dimension of the array whose upper bound needs to
	 * be determined.
	 * @return The index of the last element of the specified dimension in the array,
	 * or -1 if the specified dimension is empty.
	 */
	function GetUpperBound(dimension:Int):Int;
	@:overload(function(index:Int):Dynamic {})
	@:overload(function(indices:cs.NativeArray<Int>):Dynamic {})
	@:overload(function(index:haxe.Int64):Dynamic {})
	@:overload(function(indices:cs.NativeArray<haxe.Int64>):Dynamic {})
	@:overload(function(index1:Int, index2:Int):Dynamic {})
	@:overload(function(index1:haxe.Int64, index2:haxe.Int64):Dynamic {})
	@:overload(function(index1:Int, index2:Int, index3:Int):Dynamic {})
	/**
	 * Gets the value at the specified position in the one-dimensional . The index is
	 * specified as a 32-bit integer.
	 * @param index A 32-bit integer that represents the position of the  element to
	 * get.
	 * @return The value at the specified position in the one-dimensional .
	 */
	function GetValue(index1:haxe.Int64, index2:haxe.Int64, index3:haxe.Int64):Dynamic;
	/** Initializes every element of the value-type  by calling the parameterless constructor of the value type. */
	function Initialize():Void;
	@:overload(function(value:Dynamic, index:Int):Void {})
	@:overload(function(value:Dynamic, indices:cs.NativeArray<Int>):Void {})
	@:overload(function(value:Dynamic, index:haxe.Int64):Void {})
	@:overload(function(value:Dynamic, indices:cs.NativeArray<haxe.Int64>):Void {})
	@:overload(function(value:Dynamic, index1:Int, index2:Int):Void {})
	@:overload(function(value:Dynamic, index1:haxe.Int64, index2:haxe.Int64):Void {})
	@:overload(function(value:Dynamic, index1:Int, index2:Int, index3:Int):Void {})
	/**
	 * Sets a value to the element at the specified position in the one-dimensional .
	 * The index is specified as a 32-bit integer.
	 * @param value The new value for the specified element.
	 * @param index A 32-bit integer that represents the position of the  element to
	 * set.
	 */
	function SetValue(value:Dynamic, index1:haxe.Int64, index2:haxe.Int64, index3:haxe.Int64):Void;
}
