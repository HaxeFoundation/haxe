package cs.system.componentmodel;

/** Provides a type converter to convert  objects to and from various other representations. */
@:native("System.ComponentModel.DateTimeConverter")
extern class DateTimeConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a  using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you wish to convert from.
	 * @return if this object can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Gets a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type you wish to convert to.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given value object to a .
	 * @param context An  that provides a format context.
	 * @param culture An optional . If not supplied, the current culture is assumed.
	 * @param value The  to convert.
	 * @return An  that represents the converted .
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the given value object to a  using the arguments.
	 * @param context An  that provides a format context.
	 * @param culture An optional . If not supplied, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the value to.
	 * @return An  that represents the converted .
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
}
