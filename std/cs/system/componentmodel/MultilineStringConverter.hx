package cs.system.componentmodel;

/** Provides a type converter to convert multiline strings to a simple string. */
@:native("System.ComponentModel.MultilineStringConverter")
extern class MultilineStringConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Converts the given value object to the specified type, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture A . If  is passed, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value parameter to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Returns a collection of properties for the type of array specified by the 
	 * parameter, using the specified context and attributes.
	 * @param context An  that provides a format context.
	 * @param value An  that specifies the type of array for which to get properties.
	 * @param attributes An array of type  that is used as a filter.
	 * @return A  with the properties that are exposed for this data type, or  if there
	 * are no properties.
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns whether this object supports properties, using the specified context.
	 * @param context An  that provides a format context.
	 * @return if  should be called to find the properties of this object; otherwise, .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
