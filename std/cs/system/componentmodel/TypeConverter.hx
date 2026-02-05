package cs.system.componentmodel;

/** Provides a unified way of converting types of values to other types, as well as for accessing standard values and subproperties. */
@:native("System.ComponentModel.TypeConverter")
extern class TypeConverter {
	function new():Void;
	@:overload(function(sourceType:cs.system.Type):Bool {})
	/**
	 * Returns whether this converter can convert an object of the given type to the
	 * type of this converter, using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you want to convert from.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	@:overload(function(destinationType:cs.system.Type):Bool {})
	/**
	 * Returns whether this converter can convert the object to the specified type,
	 * using the specified context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type you want to convert to.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	@:overload(function(value:Dynamic):Dynamic {})
	/**
	 * Converts the given object to the type of this converter, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture The  to use as the current culture.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	@:overload(function(text:String):Dynamic {})
	/**
	 * Converts the given string to the type of this converter, using the invariant
	 * culture and the specified context.
	 * @param context An  that provides a format context.
	 * @param text The  to convert.
	 * @return An  that represents the converted text.
	 */
	function ConvertFromInvariantString(context:cs.system.componentmodel.ITypeDescriptorContext, text:String):Dynamic;
	@:overload(function(text:String):Dynamic {})
	@:overload(function(context:cs.system.componentmodel.ITypeDescriptorContext, text:String):Dynamic {})
	/**
	 * Converts the given text to an object, using the specified context and culture
	 * information.
	 * @param context An  that provides a format context.
	 * @param culture A . If  is passed, the current culture is assumed.
	 * @param text The  to convert.
	 * @return An  that represents the converted text.
	 */
	function ConvertFromString(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, text:String):Dynamic;
	@:overload(function(value:Dynamic, destinationType:cs.system.Type):Dynamic {})
	/**
	 * Converts the given value object to the specified type, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture A . If  is passed, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the  parameter to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	@:overload(function(value:Dynamic):String {})
	/**
	 * Converts the specified value to a culture-invariant string representation, using
	 * the specified context.
	 * @param context An  that provides a format context.
	 * @param value The  to convert.
	 * @return A  that represents the converted value.
	 */
	function ConvertToInvariantString(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):String;
	@:overload(function(value:Dynamic):String {})
	@:overload(function(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):String {})
	/**
	 * Converts the given value to a string representation, using the specified context
	 * and culture information.
	 * @param context An  that provides a format context.
	 * @param culture A . If  is passed, the current culture is assumed.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertToString(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):String;
	@:overload(function(propertyValues:cs.system.collections.IDictionary):Dynamic {})
	/**
	 * Re-creates an  given a set of property values for the object.
	 * @param propertyValues An  that represents a dictionary of new property values.
	 * @return An  representing the given , or  if the object cannot be created. This
	 * method always returns .
	 */
	function CreateInstance(context:cs.system.componentmodel.ITypeDescriptorContext, propertyValues:cs.system.collections.IDictionary):Dynamic;
	@:overload(function():Bool {})
	/**
	 * Returns whether changing a value on this object requires a call to the  method
	 * to create a new value.
	 * @return if changing a property on this object requires a call to  to create a
	 * new value; otherwise, .
	 */
	function GetCreateInstanceSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	@:overload(function(value:Dynamic):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Returns a collection of properties for the type of array specified by the value
	 * parameter, using the specified context.
	 * @param context An  that provides a format context.
	 * @param value An  that specifies the type of array for which to get properties.
	 * @return A  with the properties that are exposed for this data type, or  if there
	 * are no properties.
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	@:overload(function():Bool {})
	/**
	 * Returns whether this object supports properties.
	 * @return if  should be called to find the properties of this object; otherwise, .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	@:overload(function():cs.system.collections.ICollection {})
	/**
	 * Returns a collection of standard values from the default context for the data
	 * type this type converter is designed for.
	 * @return A  containing a standard set of valid values, or  if the data type does
	 * not support a standard set of values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	@:overload(function():Bool {})
	/**
	 * Returns whether the collection of standard values returned from  is an exclusive
	 * list.
	 * @return if the  returned from  is an exhaustive list of possible values;  if
	 * other values are possible.
	 */
	function GetStandardValuesExclusive(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	@:overload(function():Bool {})
	/**
	 * Returns whether this object supports a standard set of values that can be picked
	 * from a list.
	 * @return if  should be called to find a common set of values the object supports;
	 * otherwise, .
	 */
	function GetStandardValuesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Returns whether the given value object is valid for this type and for the
	 * specified context.
	 * @param context An  that provides a format context.
	 * @param value The  to test for validity.
	 * @return if the specified value is valid for this object; otherwise, .
	 */
	function IsValid(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):Bool;
}
