package cs.system;

/** Represents a delegate, which is a data structure that refers to a static method or to a class instance and an instance method of that class. */
@:native("System.Delegate")
extern class Delegate {
	/**
	 * Gets the method represented by the delegate.
	 * @return A  describing the method represented by the delegate.
	 */
	var Method(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the class instance on which the current delegate invokes the instance
	 * method.
	 * @return The object on which the current delegate invokes the instance method, if
	 * the delegate represents an instance method;  if the delegate represents a static
	 * method.
	 */
	var Target(default, never):Dynamic;
	@:overload(function(delegates:cs.NativeArray<cs.system.Delegate>):cs.system.Delegate {})
	/**
	 * Concatenates the invocation lists of two delegates.
	 * @param a The delegate whose invocation list comes first.
	 * @param b The delegate whose invocation list comes last.
	 * @return A new delegate with an invocation list that concatenates the invocation
	 * lists of  and  in that order. Returns  if  is , returns  if  is a null
	 * reference, and returns a null reference if both  and  are null references.
	 */
	static function Combine(a:cs.system.Delegate, b:cs.system.Delegate):cs.system.Delegate;
	@:overload(function(type:cs.system.Type, method:cs.system.reflection.MethodInfo):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, firstArgument:Dynamic, method:cs.system.reflection.MethodInfo):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, target:Dynamic, method:String):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, method:cs.system.reflection.MethodInfo, throwOnBindFailure:Bool):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, target:cs.system.Type, method:String):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, firstArgument:Dynamic, method:cs.system.reflection.MethodInfo, throwOnBindFailure:Bool):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, target:Dynamic, method:String, ignoreCase:Bool):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, target:cs.system.Type, method:String, ignoreCase:Bool):cs.system.Delegate {})
	@:overload(function(type:cs.system.Type, target:Dynamic, method:String, ignoreCase:Bool, throwOnBindFailure:Bool):cs.system.Delegate {})
	/**
	 * Creates a delegate of the specified type that represents the specified static or
	 * instance method, with the specified first argument.
	 * @param type The  of delegate to create.
	 * @param firstArgument The object to which the delegate is bound, or  to treat  as
	 * ( in Visual Basic).
	 * @param method The  describing the static or instance method the delegate is to
	 * represent.
	 * @return A delegate of the specified type that represents the specified static or
	 * instance method.
	 */
	static function CreateDelegate(type:cs.system.Type, target:cs.system.Type, method:String, ignoreCase:Bool, throwOnBindFailure:Bool):cs.system.Delegate;
	/**
	 * Determines whether the specified delegates are equal.
	 * @param d1 The first delegate to compare.
	 * @param d2 The second delegate to compare.
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(d1:cs.system.Delegate, d2:cs.system.Delegate):Bool;
	/**
	 * Determines whether the specified delegates are not equal.
	 * @param d1 The first delegate to compare.
	 * @param d2 The second delegate to compare.
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(d1:cs.system.Delegate, d2:cs.system.Delegate):Bool;
	/**
	 * Removes the last occurrence of the invocation list of a delegate from the
	 * invocation list of another delegate.
	 * @param source The delegate from which to remove the invocation list of .
	 * @param value The delegate that supplies the invocation list to remove from the
	 * invocation list of .
	 * @return A new delegate with an invocation list formed by taking the invocation
	 * list of  and removing the last occurrence of the invocation list of , if the
	 * invocation list of  is found within the invocation list of . Returns  if  is  or
	 * if the invocation list of  is not found within the invocation list of . Returns
	 * a null reference if the invocation list of  is equal to the invocation list of 
	 * or if  is a null reference.
	 */
	static function Remove(source:cs.system.Delegate, value:cs.system.Delegate):cs.system.Delegate;
	/**
	 * Removes all occurrences of the invocation list of a delegate from the invocation
	 * list of another delegate.
	 * @param source The delegate from which to remove the invocation list of .
	 * @param value The delegate that supplies the invocation list to remove from the
	 * invocation list of .
	 * @return A new delegate with an invocation list formed by taking the invocation
	 * list of  and removing all occurrences of the invocation list of , if the
	 * invocation list of  is found within the invocation list of . Returns  if  is  or
	 * if the invocation list of  is not found within the invocation list of . Returns
	 * a null reference if the invocation list of  is equal to the invocation list of ,
	 * if  contains only a series of invocation lists that are equal to the invocation
	 * list of , or if  is a null reference.
	 */
	static function RemoveAll(source:cs.system.Delegate, value:cs.system.Delegate):cs.system.Delegate;
	/**
	 * Creates a shallow copy of the delegate.
	 * @return A shallow copy of the delegate.
	 */
	function Clone():Dynamic;
	/**
	 * Dynamically invokes (late-bound) the method represented by the current delegate.
	 * @param args An array of objects that are the arguments to pass to the method
	 * represented by the current delegate. -or- , if the method represented by the
	 * current delegate does not require arguments.
	 * @return The object returned by the method represented by the delegate.
	 */
	function DynamicInvoke(args:cs.NativeArray<Dynamic>):Dynamic;
	/**
	 * Determines whether the specified object and the current delegate are of the same
	 * type and share the same targets, methods, and invocation list.
	 * @param obj The object to compare with the current delegate.
	 * @return if  and the current delegate have the same targets, methods, and
	 * invocation list; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns a hash code for the delegate.
	 * @return A hash code for the delegate.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the invocation list of the delegate.
	 * @return An array of delegates representing the invocation list of the current
	 * delegate.
	 */
	function GetInvocationList():cs.NativeArray<cs.system.Delegate>;
	/**
	 * Not supported.
	 * @param info Not supported.
	 * @param context Not supported.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
