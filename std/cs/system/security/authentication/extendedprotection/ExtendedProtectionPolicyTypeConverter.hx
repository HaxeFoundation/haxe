package cs.system.security.authentication.extendedprotection;

/** The  class represents the type converter for extended protection policy used by the server to validate incoming client connections. */
@:native("System.Security.Authentication.ExtendedProtection.ExtendedProtectionPolicyTypeConverter")
extern class ExtendedProtectionPolicyTypeConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Returns whether this converter can convert the object to the specified type.
	 * @param context The object to convert.
	 * @param destinationType A  that represents the type you want to convert to.
	 * @return if this converter can perform the conversion; otherwise .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Convert the object to the specified type
	 * @param context An  that provides a format context.
	 * @param culture A  object. If  is passed, the current culture is assumed.
	 * @param value The  to convert. This should be a  object.
	 * @param destinationType The  to convert the value parameter to.
	 * @return An  that represents the converted  parameter.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
}
