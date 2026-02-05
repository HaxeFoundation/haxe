package cs.system;

/** Provides extension methods for for the memory- and span-related types, such as , , , and . */
@:native("System.MemoryExtensions")
extern class MemoryExtensions {
	@:overload(function(text:String):cs.system.ReadOnlyMemory<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>):cs.system.Memory<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>):cs.system.Memory<T> {})
	@:overload(function(text:String, startIndex:cs.system.Index):cs.system.ReadOnlyMemory<cs.Char16> {})
	@:overload(function(text:String, start:Int):cs.system.ReadOnlyMemory<cs.Char16> {})
	@:overload(function(text:String, range:cs.system.Range):cs.system.ReadOnlyMemory<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, start:Int):cs.system.Memory<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, startIndex:cs.system.Index):cs.system.Memory<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, start:Int):cs.system.Memory<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, range:cs.system.Range):cs.system.Memory<T> {})
	@:overload(function(text:String, start:Int, length:Int):cs.system.ReadOnlyMemory<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, start:Int, length:Int):cs.system.Memory<T> {})
	/**
	 * Creates a new  over the portion of the target string.
	 * @param text The target string.
	 * @return The read-only character memory representation of the string, or  if  is
	 * .
	 */
	static function AsMemory<T>(array:cs.NativeArray<T>, start:Int, length:Int):cs.system.Memory<T>;
	@:overload(function(text:String):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>):cs.system.Span<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>):cs.system.Span<T> {})
	@:overload(function(text:String, start:Int):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, startIndex:cs.system.Index):cs.system.Span<T> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, start:Int):cs.system.Span<T> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, range:cs.system.Range):cs.system.Span<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, startIndex:cs.system.Index):cs.system.Span<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, start:Int):cs.system.Span<T> {})
	@:overload(function<T>(array:cs.NativeArray<T>, range:cs.system.Range):cs.system.Span<T> {})
	@:overload(function(text:String, start:Int, length:Int):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function<T>(segment:cs.system.ArraySegment<T>, start:Int, length:Int):cs.system.Span<T> {})
	/**
	 * Creates a new read-only span over a portion of the target string from a
	 * specified position for a specified number of characters.
	 * @param text The target string.
	 * @return The read-only span representation of the string.
	 */
	static function AsSpan<T>(array:cs.NativeArray<T>, start:Int, length:Int):cs.system.Span<T>;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, comparable:cs.system.IComparable_1<T>):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, comparable:cs.system.IComparable_1<T>):Int {})
	@:overload(function<T, TComparable>(span:cs.system.ReadOnlySpan<T>, comparable:TComparable):Int {})
	@:overload(function<T, TComparable>(span:cs.system.Span<T>, comparable:TComparable):Int {})
	@:overload(function<T, TComparer>(span:cs.system.ReadOnlySpan<T>, value:T, comparer:TComparer):Int {})
	/**
	 * Searches an entire sorted  for a value using the specified  generic interface.
	 * @param T The element type of the span.
	 * @param span The sorted  to search.
	 * @param comparable The  to use when comparing.
	 * @return The zero-based index of  in the sorted , if  is found; otherwise, a
	 * negative number that is the bitwise complement of the index of the next element
	 * that is larger than  or, if there is no larger element, the bitwise complement
	 * of .
	 */
	static function BinarySearch<T, TComparer>(span:cs.system.Span<T>, value:T, comparer:TComparer):Int;
	/**
	 * Compares one character span with another using a specified string comparison,
	 * and returns an integer that indicates their relative position in the sort order.
	 * @param span The source span.
	 * @param other The value to compare with the source span.
	 * @param comparisonType An enumeration value that determines how  and  are
	 * compared.
	 * @return A signed integer that indicates the relative order of  and :   - If less
	 * than 0,  precedes than .   - If 0,  equals .   - If greater than 0,  follows .
	 */
	static function CompareTo(span:cs.system.ReadOnlySpan<cs.Char16>, other:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Int;
	/**
	 * Indicates whether a specified value occurs within a read-only character span.
	 * @param span The source span.
	 * @param value The value to seek within the source span.
	 * @param comparisonType An enumeration value that determines how the characters in
	 * and  are compared.
	 * @return if  occurs within the span,  otherwise.
	 */
	static function Contains(span:cs.system.ReadOnlySpan<cs.Char16>, value:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Bool;
	@:overload(function<T>(source:cs.NativeArray<T>, destination:cs.system.Memory<T>):Void {})
	/**
	 * Copies the contents of the array into a memory region.
	 * @param T The type of the array.
	 * @param source The array to copy items from.
	 * @param destination The memory to copy items into.
	 */
	static function CopyTo<T>(source:cs.NativeArray<T>, destination:cs.system.Span<T>):Void;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:cs.system.ReadOnlySpan<T>):Bool {})
	@:overload(function<T>(span:cs.system.Span<T>, value:cs.system.ReadOnlySpan<T>):Bool {})
	/**
	 * Determines whether the end of the  matches the specified  when compared using
	 * the specified  option.
	 * @param span The source span.
	 * @param value The sequence to compare to the end of the source span.
	 * @param comparisonType An enumeration value that determines how  and  are
	 * compared.
	 * @return if  matches the end of ; otherwise, .
	 */
	static function EndsWith(span:cs.system.ReadOnlySpan<cs.Char16>, value:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Bool;
	/**
	 * Determines whether this  and the specified  span have the same characters when
	 * compared using the specified  option.
	 * @param span The source span.
	 * @param other The value to compare with the source span.
	 * @param comparisonType An enumeration value that determines how  and  are
	 * compared.
	 * @return if equal,  otherwise.
	 */
	static function Equals(span:cs.system.ReadOnlySpan<cs.Char16>, other:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Bool;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:T):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, value:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, value:T):Int {})
	/**
	 * Reports the zero-based index of the first occurrence of the specified  in the
	 * current .
	 * @param span The source span.
	 * @param value The value to seek within the source span.
	 * @param comparisonType An enumeration value that determines how  and  are
	 * compared.
	 * @return The index of the occurrence of the value in the span.
	 */
	static function IndexOf(span:cs.system.ReadOnlySpan<cs.Char16>, value:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Int;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, values:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, values:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value0:T, value1:T):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, value0:T, value1:T):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value0:T, value1:T, value2:T):Int {})
	/**
	 * Searches for the first index of any of the specified values similar to calling
	 * IndexOf several times with the logical OR operator.
	 * @param T The type of the span and values.
	 * @param span The span to search.
	 * @param value0 One of the values to search for.
	 * @param value1 One of the values to search for.
	 * @return The first index of the occurrence of any of the values in the span. If
	 * not found, returns -1.
	 */
	static function IndexOfAny<T>(span:cs.system.Span<T>, value0:T, value1:T, value2:T):Int;
	/**
	 * Indicates whether the specified span contains only whitespace characters.
	 * @param span The source span.
	 * @return if the span contains only whitespace characters,  otherwise.
	 */
	static function IsWhiteSpace(span:cs.system.ReadOnlySpan<cs.Char16>):Bool;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:T):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, value:cs.system.ReadOnlySpan<T>):Int {})
	/**
	 * Searches for the specified value and returns the index of its last occurrence.
	 * Values are compared using IEquatable{T}.Equals(T).
	 * @param T The type of the span and value.
	 * @param span The span to search.
	 * @param value The value to search for.
	 * @return The index of the last occurrence of the value in the span. If not found,
	 * returns -1.
	 */
	static function LastIndexOf<T>(span:cs.system.Span<T>, value:T):Int;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, values:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, values:cs.system.ReadOnlySpan<T>):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value0:T, value1:T):Int {})
	@:overload(function<T>(span:cs.system.Span<T>, value0:T, value1:T):Int {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value0:T, value1:T, value2:T):Int {})
	/**
	 * Searches for the last index of any of the specified values similar to calling
	 * LastIndexOf several times with the logical OR operator.
	 * @param T The type of the span and values.
	 * @param span The span to search.
	 * @param value0 One of the values to search for.
	 * @param value1 One of the values to search for.
	 * @return The index of the last occurrence of any of the values in the span. If
	 * not found, returns -1.
	 */
	static function LastIndexOfAny<T>(span:cs.system.Span<T>, value0:T, value1:T, value2:T):Int;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, other:cs.system.ReadOnlySpan<T>):Bool {})
	@:overload(function<T>(span:cs.system.Span<T>, other:cs.system.ReadOnlySpan<T>):Bool {})
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, other:cs.system.ReadOnlySpan<T>, elementOffset:cs.Ref<Int>):Bool {})
	/**
	 * Determines whether two read-only sequences overlap in memory.
	 * @param T The type of elmeents in the read-only sequence.
	 * @param span The first sequence.
	 * @param other The second sequence.
	 * @return if the two sequences overlap; otherwise, .
	 */
	static function Overlaps<T>(span:cs.system.Span<T>, other:cs.system.ReadOnlySpan<T>, elementOffset:cs.Ref<Int>):Bool;
	/**
	 * Reverses the sequence of the elements in the entire span.
	 * @param T The type of elements in the span.
	 * @param span The span to reverse.
	 */
	static function Reverse<T>(span:cs.system.Span<T>):Void;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, other:cs.system.ReadOnlySpan<T>):Int {})
	/**
	 * Determines the relative order of two read-only sequences by comparing their
	 * elements using IComparable{T}.CompareTo(T).
	 * @param T The type of elements in the sequence.
	 * @param span The first sequence to compare.
	 * @param other The second sequence to compare.
	 * @return A signed integer that indicates the relative order of  and :   - If less
	 * than 0,  precedes than .   - If 0,  equals .   - If greater than 0,  follows .
	 */
	static function SequenceCompareTo<T>(span:cs.system.Span<T>, other:cs.system.ReadOnlySpan<T>):Int;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, other:cs.system.ReadOnlySpan<T>):Bool {})
	/**
	 * Determines whether two read-only sequences are equal by comparing the elements
	 * using IEquatable{T}.Equals(T).
	 * @param T The type of elements in the sequence.
	 * @param span The first sequence to compare.
	 * @param other The second sequence to compare.
	 * @return if the two sequences are equal; otherwise, .
	 */
	static function SequenceEqual<T>(span:cs.system.Span<T>, other:cs.system.ReadOnlySpan<T>):Bool;
	@:overload(function<T>(span:cs.system.ReadOnlySpan<T>, value:cs.system.ReadOnlySpan<T>):Bool {})
	@:overload(function<T>(span:cs.system.Span<T>, value:cs.system.ReadOnlySpan<T>):Bool {})
	/**
	 * Determines whether a read-only character span begins with a specified value when
	 * compared using a specified  value.
	 * @param span The source span.
	 * @param value The sequence to compare to the beginning of the source span.
	 * @param comparisonType An enumeration value that determines how  and  are
	 * compared.
	 * @return if  matches the beginning of ; otherwise, .
	 */
	static function StartsWith(span:cs.system.ReadOnlySpan<cs.Char16>, value:cs.system.ReadOnlySpan<cs.Char16>, comparisonType:cs.system.StringComparison):Bool;
	/**
	 * Copies the characters from the source span into the destination, converting each
	 * character to lowercase, using the casing rules of the specified culture.
	 * @param source The source span.
	 * @param destination The destination span which contains the transformed
	 * characters.
	 * @param culture An object that supplies culture-specific casing rules.
	 * @return The number of characters written into the destination span. If the
	 * destination is too small, returns -1.
	 */
	static function ToLower(source:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>, culture:cs.system.globalization.CultureInfo):Int;
	/**
	 * Copies the characters from the source span into the destination, converting each
	 * character to lowercase, using the casing rules of the invariant culture.
	 * @param source The source span.
	 * @param destination The destination span which contains the transformed
	 * characters.
	 * @return The number of characters written into the destination span. If the
	 * destination is too small, returns -1.
	 */
	static function ToLowerInvariant(source:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>):Int;
	/**
	 * Copies the characters from the source span into the destination, converting each
	 * character to uppercase, using the casing rules of the specified culture.
	 * @param source The source span.
	 * @param destination The destination span which contains the transformed
	 * characters.
	 * @param culture An object that supplies culture-specific casing rules.
	 * @return The number of characters written into the destination span. If the
	 * destination is too small, returns -1.
	 */
	static function ToUpper(source:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>, culture:cs.system.globalization.CultureInfo):Int;
	/**
	 * Copies the characters from the source span into the destination, converting each
	 * character to uppercase using the casing rules of the invariant culture.
	 * @param source The source span.
	 * @param destination The destination span which contains the transformed
	 * characters.
	 * @return The number of characters written into the destination span. If the
	 * destination is too small, returns -1.
	 */
	static function ToUpperInvariant(source:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>):Int;
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>, trimChar:cs.Char16):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Removes all leading and trailing whitespace characters from a read-only
	 * character span.
	 * @param span The source span from which the characters are removed.
	 * @return The trimmed read-only character span.
	 */
	static function Trim(span:cs.system.ReadOnlySpan<cs.Char16>, trimChars:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16>;
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>, trimChar:cs.Char16):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Removes all trailing whitespace characters from a read-only character span.
	 * @param span The source span from which the characters are removed.
	 * @return The trimmed read-only character span.
	 */
	static function TrimEnd(span:cs.system.ReadOnlySpan<cs.Char16>, trimChars:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16>;
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	@:overload(function(span:cs.system.ReadOnlySpan<cs.Char16>, trimChar:cs.Char16):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Removes all leading whitespace characters from a read-only span.
	 * @param span The source span from which the characters are removed.
	 * @return The trimmed read-only character span.
	 */
	static function TrimStart(span:cs.system.ReadOnlySpan<cs.Char16>, trimChars:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16>;
}
