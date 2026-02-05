package cs.system.dynamic;

/** Provides a base class for specifying dynamic behavior at run time. This class must be inherited from; you cannot instantiate it directly. */
@:native("System.Dynamic.DynamicObject")
extern class DynamicObject {
	/**
	 * Returns the enumeration of all dynamic member names.
	 * @return A sequence that contains dynamic member names.
	 */
	function GetDynamicMemberNames():cs.system.collections.generic.IEnumerable<String>;
	/**
	 * Provides a  that dispatches to the dynamic virtual methods. The object can be
	 * encapsulated inside another  to provide custom behavior for individual actions.
	 * This method supports the Dynamic Language Runtime infrastructure for language
	 * implementers and it is not intended to be used directly from your code.
	 * @param parameter The expression that represents  to dispatch to the dynamic
	 * virtual methods.
	 * @return An object of the  type.
	 */
	function GetMetaObject(parameter:cs.system.linq.expressions.Expression):cs.system.dynamic.DynamicMetaObject;
	/**
	 * Provides implementation for binary operations. Classes derived from the  class
	 * can override this method to specify dynamic behavior for operations such as
	 * addition and multiplication.
	 * @param binder Provides information about the binary operation. The
	 * binder.Operation property returns an  object. For example, for the sum = first +
	 * second statement, where first and second are derived from the  class,
	 * binder.Operation returns ExpressionType.Add.
	 * @param arg The right operand for the binary operation. For example, for the sum
	 * = first + second statement, where first and second are derived from the  class, 
	 * is equal to second.
	 * @param result The result of the binary operation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryBinaryOperation(binder:cs.system.dynamic.BinaryOperationBinder, arg:Dynamic, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides implementation for type conversion operations. Classes derived from the
	 * class can override this method to specify dynamic behavior for operations that
	 * convert an object from one type to another.
	 * @param binder Provides information about the conversion operation. The
	 * binder.Type property provides the type to which the object must be converted.
	 * For example, for the statement (String)sampleObject in C# (CType(sampleObject,
	 * Type) in Visual Basic), where sampleObject is an instance of the class derived
	 * from the  class, binder.Type returns the  type. The binder.Explicit property
	 * provides information about the kind of conversion that occurs. It returns  for
	 * explicit conversion and  for implicit conversion.
	 * @param result The result of the type conversion operation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryConvert(binder:cs.system.dynamic.ConvertBinder, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that initialize a new instance of a
	 * dynamic object. This method is not intended for use in C# or Visual Basic.
	 * @param binder Provides information about the initialization operation.
	 * @param args The arguments that are passed to the object during initialization.
	 * For example, for the new SampleType(100) operation, where SampleType is the type
	 * derived from the  class, args[0] is equal to 100.
	 * @param result The result of the initialization.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryCreateInstance(binder:cs.system.dynamic.CreateInstanceBinder, args:cs.NativeArray<Dynamic>, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that delete an object by index. This
	 * method is not intended for use in C# or Visual Basic.
	 * @param binder Provides information about the deletion.
	 * @param indexes The indexes to be deleted.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryDeleteIndex(binder:cs.system.dynamic.DeleteIndexBinder, indexes:cs.NativeArray<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that delete an object member. This
	 * method is not intended for use in C# or Visual Basic.
	 * @param binder Provides information about the deletion.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryDeleteMember(binder:cs.system.dynamic.DeleteMemberBinder):Bool;
	/**
	 * Provides the implementation for operations that get a value by index. Classes
	 * derived from the  class can override this method to specify dynamic behavior for
	 * indexing operations.
	 * @param binder Provides information about the operation.
	 * @param indexes The indexes that are used in the operation. For example, for the
	 * sampleObject[3] operation in C# (sampleObject(3) in Visual Basic), where
	 * sampleObject is derived from the  class, indexes[0] is equal to 3.
	 * @param result The result of the index operation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * run-time exception is thrown.)
	 */
	function TryGetIndex(binder:cs.system.dynamic.GetIndexBinder, indexes:cs.NativeArray<Dynamic>, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that get member values. Classes
	 * derived from the  class can override this method to specify dynamic behavior for
	 * operations such as getting a value for a property.
	 * @param binder Provides information about the object that called the dynamic
	 * operation. The binder.Name property provides the name of the member on which the
	 * dynamic operation is performed. For example, for the
	 * Console.WriteLine(sampleObject.SampleProperty) statement, where sampleObject is
	 * an instance of the class derived from the  class, binder.Name returns
	 * "SampleProperty". The binder.IgnoreCase property specifies whether the member
	 * name is case-sensitive.
	 * @param result The result of the get operation. For example, if the method is
	 * called for a property, you can assign the property value to .
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * run-time exception is thrown.)
	 */
	function TryGetMember(binder:cs.system.dynamic.GetMemberBinder, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that invoke an object. Classes
	 * derived from the  class can override this method to specify dynamic behavior for
	 * operations such as invoking an object or a delegate.
	 * @param binder Provides information about the invoke operation.
	 * @param args The arguments that are passed to the object during the invoke
	 * operation. For example, for the sampleObject(100) operation, where sampleObject
	 * is derived from the  class, args[0] is equal to 100.
	 * @param result The result of the object invocation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.
	 */
	function TryInvoke(binder:cs.system.dynamic.InvokeBinder, args:cs.NativeArray<Dynamic>, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that invoke a member. Classes derived
	 * from the  class can override this method to specify dynamic behavior for
	 * operations such as calling a method.
	 * @param binder Provides information about the dynamic operation. The binder.Name
	 * property provides the name of the member on which the dynamic operation is
	 * performed. For example, for the statement sampleObject.SampleMethod(100), where
	 * sampleObject is an instance of the class derived from the  class, binder.Name
	 * returns "SampleMethod". The binder.IgnoreCase property specifies whether the
	 * member name is case-sensitive.
	 * @param args The arguments that are passed to the object member during the invoke
	 * operation. For example, for the statement sampleObject.SampleMethod(100), where
	 * sampleObject is derived from the  class, args[0] is equal to 100.
	 * @param result The result of the member invocation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryInvokeMember(binder:cs.system.dynamic.InvokeMemberBinder, args:cs.NativeArray<Dynamic>, result:cs.Ref<Dynamic>):Bool;
	/**
	 * Provides the implementation for operations that set a value by index. Classes
	 * derived from the  class can override this method to specify dynamic behavior for
	 * operations that access objects by a specified index.
	 * @param binder Provides information about the operation.
	 * @param indexes The indexes that are used in the operation. For example, for the
	 * sampleObject[3] = 10 operation in C# (sampleObject(3) = 10 in Visual Basic),
	 * where sampleObject is derived from the  class, indexes[0] is equal to 3.
	 * @param value The value to set to the object that has the specified index. For
	 * example, for the sampleObject[3] = 10 operation in C# (sampleObject(3) = 10 in
	 * Visual Basic), where sampleObject is derived from the  class,  is equal to 10.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.
	 */
	function TrySetIndex(binder:cs.system.dynamic.SetIndexBinder, indexes:cs.NativeArray<Dynamic>, value:Dynamic):Bool;
	/**
	 * Provides the implementation for operations that set member values. Classes
	 * derived from the  class can override this method to specify dynamic behavior for
	 * operations such as setting a value for a property.
	 * @param binder Provides information about the object that called the dynamic
	 * operation. The binder.Name property provides the name of the member to which the
	 * value is being assigned. For example, for the statement
	 * sampleObject.SampleProperty = "Test", where sampleObject is an instance of the
	 * class derived from the  class, binder.Name returns "SampleProperty". The
	 * binder.IgnoreCase property specifies whether the member name is case-sensitive.
	 * @param value The value to set to the member. For example, for
	 * sampleObject.SampleProperty = "Test", where sampleObject is an instance of the
	 * class derived from the  class, the  is "Test".
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TrySetMember(binder:cs.system.dynamic.SetMemberBinder, value:Dynamic):Bool;
	/**
	 * Provides implementation for unary operations. Classes derived from the  class
	 * can override this method to specify dynamic behavior for operations such as
	 * negation, increment, or decrement.
	 * @param binder Provides information about the unary operation. The
	 * binder.Operation property returns an  object. For example, for the
	 * negativeNumber = -number statement, where number is derived from the  class,
	 * binder.Operation returns "Negate".
	 * @param result The result of the unary operation.
	 * @return if the operation is successful; otherwise, . If this method returns ,
	 * the run-time binder of the language determines the behavior. (In most cases, a
	 * language-specific run-time exception is thrown.)
	 */
	function TryUnaryOperation(binder:cs.system.dynamic.UnaryOperationBinder, result:cs.Ref<Dynamic>):Bool;
}
