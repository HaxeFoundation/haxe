package cs.system.reflection.emit;

/** Provides methods for building signatures. */
@:native("System.Reflection.Emit.SignatureHelper")
extern class SignatureHelper {
	/**
	 * Returns a signature helper for a field.
	 * @param mod The dynamic module that contains the field for which the  is
	 * requested.
	 * @return The  object for a field.
	 */
	static function GetFieldSigHelper(mod:cs.system.reflection.Module):cs.system.reflection.emit.SignatureHelper;
	@:overload(function():cs.system.reflection.emit.SignatureHelper {})
	/**
	 * Returns a signature helper for a local variable.
	 * @return A  for a local variable.
	 */
	static function GetLocalVarSigHelper(mod:cs.system.reflection.Module):cs.system.reflection.emit.SignatureHelper;
	@:overload(function(callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type):cs.system.reflection.emit.SignatureHelper {})
	@:overload(function(mod:cs.system.reflection.Module, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type):cs.system.reflection.emit.SignatureHelper {})
	/**
	 * Returns a signature helper for a method given the method's calling convention
	 * and return type.
	 * @param callingConvention The calling convention of the method.
	 * @param returnType The return type of the method, or  for a void return type (
	 * procedure in Visual Basic).
	 * @return The  object for a method.
	 */
	static function GetMethodSigHelper(mod:cs.system.reflection.Module, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.SignatureHelper;
	@:overload(function(mod:cs.system.reflection.Module, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):cs.system.reflection.emit.SignatureHelper {})
	@:overload(function(mod:cs.system.reflection.Module, returnType:cs.system.Type, requiredReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, optionalReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, requiredParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, optionalParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.SignatureHelper {})
	/**
	 * Returns a signature helper for a property, given the dynamic module that
	 * contains the property, the calling convention, the property type, the property
	 * arguments, and custom modifiers for the return type and arguments.
	 * @param mod The  that contains the property for which the  is requested.
	 * @param callingConvention The calling convention of the property accessors.
	 * @param returnType The property type.
	 * @param requiredReturnTypeCustomModifiers An array of types representing the
	 * required custom modifiers for the return type, such as  or . If the return type
	 * has no required custom modifiers, specify .
	 * @param optionalReturnTypeCustomModifiers An array of types representing the
	 * optional custom modifiers for the return type, such as  or . If the return type
	 * has no optional custom modifiers, specify .
	 * @param parameterTypes The types of the property's arguments, or  if the property
	 * has no arguments.
	 * @param requiredParameterTypeCustomModifiers An array of arrays of types. Each
	 * array of types represents the required custom modifiers for the corresponding
	 * argument of the property. If a particular argument has no required custom
	 * modifiers, specify  instead of an array of types. If the property has no
	 * arguments, or if none of the arguments have required custom modifiers, specify 
	 * instead of an array of arrays.
	 * @param optionalParameterTypeCustomModifiers An array of arrays of types. Each
	 * array of types represents the optional custom modifiers for the corresponding
	 * argument of the property. If a particular argument has no optional custom
	 * modifiers, specify  instead of an array of types. If the property has no
	 * arguments, or if none of the arguments have optional custom modifiers, specify 
	 * instead of an array of arrays.
	 * @return A  object for a property.
	 */
	static function GetPropertySigHelper(mod:cs.system.reflection.Module, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, requiredReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, optionalReturnTypeCustomModifiers:cs.NativeArray<cs.system.Type>, parameterTypes:cs.NativeArray<cs.system.Type>, requiredParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, optionalParameterTypeCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):cs.system.reflection.emit.SignatureHelper;
	@:overload(function(clsArgument:cs.system.Type):Void {})
	@:overload(function(argument:cs.system.Type, pinned:Bool):Void {})
	/**
	 * Adds an argument to the signature.
	 * @param clsArgument The type of the argument.
	 */
	function AddArgument(argument:cs.system.Type, requiredCustomModifiers:cs.NativeArray<cs.system.Type>, optionalCustomModifiers:cs.NativeArray<cs.system.Type>):Void;
	/**
	 * Adds a set of arguments to the signature, with the specified custom modifiers.
	 * @param arguments The types of the arguments to be added.
	 * @param requiredCustomModifiers An array of arrays of types. Each array of types
	 * represents the required custom modifiers for the corresponding argument, such as
	 * or . If a particular argument has no required custom modifiers, specify  instead
	 * of an array of types. If none of the arguments have required custom modifiers,
	 * specify  instead of an array of arrays.
	 * @param optionalCustomModifiers An array of arrays of types. Each array of types
	 * represents the optional custom modifiers for the corresponding argument, such as
	 * or . If a particular argument has no optional custom modifiers, specify  instead
	 * of an array of types. If none of the arguments have optional custom modifiers,
	 * specify  instead of an array of arrays.
	 */
	function AddArguments(arguments:cs.NativeArray<cs.system.Type>, requiredCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>, optionalCustomModifiers:cs.NativeArray<cs.NativeArray<cs.system.Type>>):Void;
	/** Marks the end of a vararg fixed part. This is only used if the caller is creating a vararg signature call site. */
	function AddSentinel():Void;
	/**
	 * Checks if this instance is equal to the given object.
	 * @param obj The object with which this instance should be compared.
	 * @return if the given object is a  and represents the same signature; otherwise,
	 * .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Creates and returns a hash code for this instance.
	 * @return The hash code based on the name.
	 */
	function GetHashCode():Int;
	/**
	 * Adds the end token to the signature and marks the signature as finished, so no
	 * further tokens can be added.
	 * @return A byte array made up of the full signature.
	 */
	function GetSignature():cs.NativeArray<cs.UInt8>;
	/**
	 * Returns a string representing the signature arguments.
	 * @return A string representing the arguments of this signature.
	 */
	function ToString():String;
}
