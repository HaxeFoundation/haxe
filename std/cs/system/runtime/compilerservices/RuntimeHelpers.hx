package cs.system.runtime.compilerservices;

/** Provides a set of static methods and properties that provide support for compilers. This class cannot be inherited. */
@:native("System.Runtime.CompilerServices.RuntimeHelpers")
extern class RuntimeHelpers {
	/**
	 * Gets the offset, in bytes, to the data in the given string.
	 * @return The byte offset, from the start of the  object to the first character in
	 * the string.
	 */
	static var OffsetToStringData(default, never):Int;
	/** Ensures that the remaining stack space is large enough to execute the average .NET Framework function. */
	static function EnsureSufficientExecutionStack():Void;
	/**
	 * Determines whether the specified  instances are considered equal.
	 * @param o1 The first object to compare.
	 * @param o2 The second object to compare.
	 * @return if the  parameter is the same instance as the  parameter, or if both are
	 * , or if o1.Equals(o2) returns ; otherwise, .
	 */
	static function Equals(o1:Dynamic, o2:Dynamic):Bool;
	/**
	 * Executes code using a  while using another  to execute additional code in case
	 * of an exception.
	 * @param code A delegate to the code to try.
	 * @param backoutCode A delegate to the code to run if an exception occurs.
	 * @param userData The data to pass to  and .
	 */
	static function ExecuteCodeWithGuaranteedCleanup(code:cs.system.runtime.compilerservices.RuntimeHelpers_TryCode, backoutCode:cs.system.runtime.compilerservices.RuntimeHelpers_CleanupCode, userData:Dynamic):Void;
	/**
	 * Serves as a hash function for a particular object, and is suitable for use in
	 * algorithms and data structures that use hash codes, such as a hash table.
	 * @param o An object to retrieve the hash code for.
	 * @return A hash code for the object identified by the  parameter.
	 */
	static function GetHashCode(o:Dynamic):Int;
	/**
	 * Boxes a value type.
	 * @param obj The value type to be boxed.
	 * @return A boxed copy of  if it is a value class; otherwise,  itself.
	 */
	static function GetObjectValue(obj:Dynamic):Dynamic;
	/**
	 * Slices the specified array using the specified range.
	 * @param T The type of the elements in the array.
	 * @param array The array to slice.
	 * @param range An object that determines the portion of  to include in the slice.
	 * @return The subarray defined by .
	 */
	static function GetSubArray<T>(array:cs.NativeArray<T>, range:cs.system.Range):cs.NativeArray<T>;
	/** @param type  */
	static function GetUninitializedObject(type:cs.system.Type):Dynamic;
	/**
	 * Provides a fast way to initialize an array from data that is stored in a module.
	 * @param array The array to be initialized.
	 * @param fldHandle A field handle that specifies the location of the data used to
	 * initialize the array.
	 */
	static function InitializeArray(array:cs.system.Array, fldHandle:cs.system.RuntimeFieldHandle):Void;
	/**
	 * Returns a value that indicates whether the specified type is a reference type or
	 * a value type that contains references.
	 * @param T The type.
	 * @return if the given type is reference type or value type that contains
	 * references; otherwise, .
	 */
	static function IsReferenceOrContainsReferences<T>():Bool;
	/** Designates a body of code as a constrained execution region (CER). */
	static function PrepareConstrainedRegions():Void;
	/** Designates a body of code as a constrained execution region (CER) without performing any probing. */
	static function PrepareConstrainedRegionsNoOP():Void;
	/**
	 * Provides a way for applications to dynamically prepare  event delegates.
	 * @param d The event delegate to prepare.
	 */
	static function PrepareContractedDelegate(d:cs.system.Delegate):Void;
	/**
	 * Indicates that the specified delegate should be prepared for inclusion in a
	 * constrained execution region (CER).
	 * @param d The delegate type to prepare.
	 */
	static function PrepareDelegate(d:cs.system.Delegate):Void;
	@:overload(function(method:cs.system.RuntimeMethodHandle):Void {})
	/**
	 * Prepares a method for inclusion in a constrained execution region (CER).
	 * @param method A handle to the method to prepare.
	 */
	static function PrepareMethod(method:cs.system.RuntimeMethodHandle, instantiation:cs.NativeArray<cs.system.RuntimeTypeHandle>):Void;
	/** Probes for a certain amount of stack space to ensure that a stack overflow cannot happen within a subsequent block of code (assuming that your code uses only a finite and moderate amount of stack space). We recommend that you use a constrained execution region (CER) instead of this method. */
	static function ProbeForSufficientStack():Void;
	/**
	 * Ensures that the type initializer (also known as a static constructor) for the
	 * specified type has been run.
	 * @param type A type handle that specifies the type for which a type initializer
	 * should be run.
	 */
	static function RunClassConstructor(type:cs.system.RuntimeTypeHandle):Void;
	/**
	 * Runs a specified module constructor method.
	 * @param module A handle that specifies the module constructor method to run.
	 */
	static function RunModuleConstructor(module:cs.system.ModuleHandle):Void;
	static function TryEnsureSufficientExecutionStack():Bool;
}
