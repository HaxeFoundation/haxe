package cs.system.componentmodel;

/** Provides a type converter to convert Unicode character objects to and from various other representations. */
@:native("System.ComponentModel.CharConverter")
extern class CharConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a Unicode character object using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you want to convert from.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Converts the given object to a Unicode character object.
	 * @param context An  that provides a format context.
	 * @param culture The culture into which  will be converted.
	 * @param value The  to convert.
	 * @return An  that represents the converted .
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to a Unicode character object using the
	 * arguments.
	 * @param context An  that provides a format context.
	 * @param culture The culture into which  will be converted.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
}
