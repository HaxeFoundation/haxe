package cs.system.componentmodel;

/** Provides a type converter to convert string objects to and from other representations. */
@:native("System.ComponentModel.StringConverter")
extern class StringConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a string using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you wish to convert from.
	 * @return if this converter can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Converts the specified value object to a  object.
	 * @param context An  that provides a format context.
	 * @param culture The  to use.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
}
