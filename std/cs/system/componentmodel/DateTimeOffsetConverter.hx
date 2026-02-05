package cs.system.componentmodel;

/** Provides a type converter to convert  structures to and from various other representations. */
@:native("System.ComponentModel.DateTimeOffsetConverter")
extern class DateTimeOffsetConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Returns a value that indicates whether an object of the specified source type
	 * can be converted to a .
	 * @param context The date format context.
	 * @param sourceType The source type to check.
	 * @return if the specified type can be converted to a ; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns a value that indicates whether a  can be converted to an object of the
	 * specified type.
	 * @param context The date format context.
	 * @param destinationType The destination type to check.
	 * @return if a  can be converted to the specified type; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the specified object to a .
	 * @param context The date format context.
	 * @param culture The date culture.
	 * @param value The object to be converted.
	 * @return A  that represents the specified object.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts a  to an object of the specified type.
	 * @param context The date format context.
	 * @param culture The date culture.
	 * @param value The  to be converted.
	 * @param destinationType The type to convert to.
	 * @return An object of the specified type that represents the .
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
}
