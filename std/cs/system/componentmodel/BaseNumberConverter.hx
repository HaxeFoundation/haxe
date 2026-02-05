package cs.system.componentmodel;

/** Provides a base type converter for nonfloating-point numerical types. */
@:native("System.ComponentModel.BaseNumberConverter")
extern class BaseNumberConverter extends cs.system.componentmodel.TypeConverter {
	/**
	 * Determines if this converter can convert an object in the given source type to
	 * the native type of the converter.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type from which you want to convert.
	 * @return if this converter can perform the operation; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context An  that provides a format context.
	 * @param t A  that represents the type to which you want to convert.
	 * @return if this converter can perform the operation; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given object to the converter's native type.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture to represent the number.
	 * @param value The object to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the specified object to another type.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture to represent the number.
	 * @param value The object to convert.
	 * @param destinationType The type to convert the object to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
}
