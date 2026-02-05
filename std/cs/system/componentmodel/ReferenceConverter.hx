package cs.system.componentmodel;

/** Provides a type converter to convert object references to and from other representations. */
@:native("System.ComponentModel.ReferenceConverter")
extern class ReferenceConverter extends cs.system.componentmodel.TypeConverter {
	function new(type:cs.system.Type):Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a reference object using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you wish to convert from.
	 * @return if this object can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Converts the given object to the reference type.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture used to represent the font.
	 * @param value The  to convert.
	 * @return An  that represents the converted .
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to the reference type using the specified
	 * context and arguments.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture used to represent the font.
	 * @param value The  to convert.
	 * @param destinationType The type to convert the object to.
	 * @return The converted object.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Gets a collection of standard values for the reference data type.
	 * @param context An  that provides a format context.
	 * @return A  that holds a standard set of valid values, or  if the data type does
	 * not support a standard set of values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Gets a value indicating whether the list of standard values returned from  is an
	 * exclusive list.
	 * @param context An  that provides a format context.
	 * @return because the  returned from  is an exhaustive list of possible values.
	 * This method never returns .
	 */
	function GetStandardValuesExclusive(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
	/**
	 * Gets a value indicating whether this object supports a standard set of values
	 * that can be picked from a list.
	 * @param context An  that provides a format context.
	 * @return because  can be called to find a common set of values the object
	 * supports. This method never returns .
	 */
	function GetStandardValuesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
