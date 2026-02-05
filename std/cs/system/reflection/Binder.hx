package cs.system.reflection;

/** Selects a member from a list of candidates, and performs type conversion from actual argument type to formal argument type. */
@:native("System.Reflection.Binder")
extern class Binder {
	/**
	 * Selects a field from the given set of fields, based on the specified criteria.
	 * @param bindingAttr A bitwise combination of  values.
	 * @param match The set of fields that are candidates for matching. For example,
	 * when a  object is used by , this parameter specifies the set of fields that
	 * reflection has determined to be possible matches, typically because they have
	 * the correct member name. The default implementation provided by  changes the
	 * order of this array.
	 * @param value The field value used to locate a matching field.
	 * @param culture An instance of  that is used to control the coercion of data
	 * types, in binder implementations that coerce types. If  is , the  for the
	 * current thread is used. Note   For example, if a binder implementation allows
	 * coercion of string values to numeric types, this parameter is necessary to
	 * convert a  that represents 1000 to a  value, because 1000 is represented
	 * differently by different cultures. The default binder does not do such string
	 * coercions.
	 * @return The matching field.
	 */
	function BindToField(bindingAttr:cs.system.reflection.BindingFlags, match:cs.NativeArray<cs.system.reflection.FieldInfo>, value:Dynamic, culture:cs.system.globalization.CultureInfo):cs.system.reflection.FieldInfo;
	/**
	 * Selects a method to invoke from the given set of methods, based on the supplied
	 * arguments.
	 * @param bindingAttr A bitwise combination of  values.
	 * @param match The set of methods that are candidates for matching. For example,
	 * when a  object is used by , this parameter specifies the set of methods that
	 * reflection has determined to be possible matches, typically because they have
	 * the correct member name. The default implementation provided by  changes the
	 * order of this array.
	 * @param args The arguments that are passed in. The binder can change the order of
	 * the arguments in this array; for example, the default binder changes the order
	 * of arguments if the  parameter is used to specify an order other than positional
	 * order. If a binder implementation coerces argument types, the types and values
	 * of the arguments can be changed as well.
	 * @param modifiers An array of parameter modifiers that enable binding to work
	 * with parameter signatures in which the types have been modified. The default
	 * binder implementation does not use this parameter.
	 * @param culture An instance of  that is used to control the coercion of data
	 * types, in binder implementations that coerce types. If  is , the  for the
	 * current thread is used. Note   For example, if a binder implementation allows
	 * coercion of string values to numeric types, this parameter is necessary to
	 * convert a  that represents 1000 to a  value, because 1000 is represented
	 * differently by different cultures. The default binder does not do such string
	 * coercions.
	 * @param names The parameter names, if parameter names are to be considered when
	 * matching, or  if arguments are to be treated as purely positional. For example,
	 * parameter names must be used if arguments are not supplied in positional order.
	 * @param state After the method returns,  contains a binder-provided object that
	 * keeps track of argument reordering. The binder creates this object, and the
	 * binder is the sole consumer of this object. If  is not  when  returns, you must
	 * pass  to the  method if you want to restore  to its original order, for example,
	 * so that you can retrieve the values of  parameters ( parameters in Visual
	 * Basic).
	 * @return The matching method.
	 */
	function BindToMethod(bindingAttr:cs.system.reflection.BindingFlags, match:cs.NativeArray<cs.system.reflection.MethodBase>, args:cs.Ref<cs.NativeArray<Dynamic>>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>, culture:cs.system.globalization.CultureInfo, names:cs.NativeArray<String>, state:cs.Ref<Dynamic>):cs.system.reflection.MethodBase;
	/**
	 * Changes the type of the given  to the given .
	 * @param value The object to change into a new .
	 * @param type The new  that  will become.
	 * @param culture An instance of  that is used to control the coercion of data
	 * types. If  is , the  for the current thread is used. Note   For example, this
	 * parameter is necessary to convert a  that represents 1000 to a  value, because
	 * 1000 is represented differently by different cultures.
	 * @return An object that contains the given value as the new type.
	 */
	function ChangeType(value:Dynamic, type:cs.system.Type, culture:cs.system.globalization.CultureInfo):Dynamic;
	/**
	 * Upon returning from , restores the  argument to what it was when it came from .
	 * @param args The actual arguments that are passed in. Both the types and values
	 * of the arguments can be changed.
	 * @param state A binder-provided object that keeps track of argument reordering.
	 */
	function ReorderArgumentArray(args:cs.Ref<cs.NativeArray<Dynamic>>, state:Dynamic):Void;
	/**
	 * Selects a method from the given set of methods, based on the argument type.
	 * @param bindingAttr A bitwise combination of  values.
	 * @param match The set of methods that are candidates for matching. For example,
	 * when a  object is used by , this parameter specifies the set of methods that
	 * reflection has determined to be possible matches, typically because they have
	 * the correct member name. The default implementation provided by  changes the
	 * order of this array.
	 * @param types The parameter types used to locate a matching method.
	 * @param modifiers An array of parameter modifiers that enable binding to work
	 * with parameter signatures in which the types have been modified.
	 * @return The matching method, if found; otherwise, .
	 */
	function SelectMethod(bindingAttr:cs.system.reflection.BindingFlags, match:cs.NativeArray<cs.system.reflection.MethodBase>, types:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.MethodBase;
	/**
	 * Selects a property from the given set of properties, based on the specified
	 * criteria.
	 * @param bindingAttr A bitwise combination of  values.
	 * @param match The set of properties that are candidates for matching. For
	 * example, when a  object is used by , this parameter specifies the set of
	 * properties that reflection has determined to be possible matches, typically
	 * because they have the correct member name. The default implementation provided
	 * by  changes the order of this array.
	 * @param returnType The return value the matching property must have.
	 * @param indexes The index types of the property being searched for. Used for
	 * index properties such as the indexer for a class.
	 * @param modifiers An array of parameter modifiers that enable binding to work
	 * with parameter signatures in which the types have been modified.
	 * @return The matching property.
	 */
	function SelectProperty(bindingAttr:cs.system.reflection.BindingFlags, match:cs.NativeArray<cs.system.reflection.PropertyInfo>, returnType:cs.system.Type, indexes:cs.NativeArray<cs.system.Type>, modifiers:cs.NativeArray<cs.system.reflection.ParameterModifier>):cs.system.reflection.PropertyInfo;
}
