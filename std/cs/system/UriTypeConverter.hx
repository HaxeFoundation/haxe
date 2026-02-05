package cs.system;

/** Converts a  type to a  type, and vice versa. */
@:native("System.UriTypeConverter")
extern class UriTypeConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Returns whether this converter can convert an object of the given type to the
	 * type of this converter.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type that you want to convert from.
	 * @return if  is a  type or a  type can be assigned from ; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns whether this converter can convert the object to the specified type,
	 * using the specified context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type that you want to convert to.
	 * @return if  is of type , , or ; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given object to the type of this converter, using the specified
	 * context and culture information.
	 * @param context An  that provides a format context.
	 * @param culture The  to use as the current culture.
	 * @param value The  to convert.
	 * @return An  that represents the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts a given value object to the specified type, using the specified context
	 * and culture information.
	 * @param context An  that provides a format context.
	 * @param culture A . If  is passed, the current culture is assumed.
	 * @param value The  to convert.
	 * @param destinationType The  to convert the  parameter to.
	 * @return An  that represents the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Returns whether the given value object is a  or a  can be created from it.
	 * @param context An  that provides a format context.
	 * @param value The  to test for validity.
	 * @return if  is a  or a  from which a  can be created; otherwise, .
	 */
	function IsValid(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic):Bool;
}
