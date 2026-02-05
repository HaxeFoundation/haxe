package cs.system.componentmodel;

/** Provides a type converter to convert collection objects to and from various other representations. */
@:native("System.ComponentModel.CollectionConverter")
extern class CollectionConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Converts the given value object to the specified destination type.
	 * @param context An  that provides a format context.
	 * @param culture The culture to which  will be converted.
	 * @param value The  to convert. This parameter must inherit from .
	 * @param destinationType The  to convert the value to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Gets a collection of properties for the type of array specified by the value
	 * parameter using the specified context and attributes.
	 * @param context An  that provides a format context.
	 * @param value An  that specifies the type of array to get the properties for.
	 * @param attributes An array of type  that will be used as a filter.
	 * @return A  with the properties that are exposed for this data type, or  if there
	 * are no properties. This method always returns .
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Gets a value indicating whether this object supports properties.
	 * @param context An  that provides a format context.
	 * @return because  should not be called to find the properties of this object.
	 * This method never returns .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
