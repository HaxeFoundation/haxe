package cs.system.drawing;

/** Converts colors from one data type to another. Access this class through the . */
@:native("System.Drawing.ColorConverter")
extern class ColorConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Determines if this converter can convert an object in the given source type to
	 * the native type of the converter.
	 * @param context An  that provides a format context. You can use this object to
	 * get additional information about the environment from which this converter is
	 * being invoked.
	 * @param sourceType The type from which you want to convert.
	 * @return if this object can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Returns a value indicating whether this converter can convert an object to the
	 * given destination type using the context.
	 * @param context An  that provides a format context.
	 * @param destinationType A  that represents the type to which you want to convert.
	 * @return if this converter can perform the operation; otherwise, .
	 */
	function CanConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, destinationType:cs.system.Type):Bool;
	/**
	 * Converts the given object to the converter's native type.
	 * @param context A  that provides a format context. You can use this object to get
	 * additional information about the environment from which this converter is being
	 * invoked.
	 * @param culture A  that specifies the culture to represent the color.
	 * @param value The object to convert.
	 * @return An  representing the converted value.
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Converts the specified object to another type.
	 * @param context A formatter context. Use this object to extract additional
	 * information about the environment from which this converter is being invoked.
	 * Always check whether this value is . Also, properties on the context object may
	 * return .
	 * @param culture A  that specifies the culture to represent the color.
	 * @param value The object to convert.
	 * @param destinationType The type to convert the object to.
	 * @return An  representing the converted value.
	 */
	function ConvertTo(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic, destinationType:cs.system.Type):Dynamic;
	/**
	 * Retrieves a collection containing a set of standard values for the data type for
	 * which this validator is designed. This will return  if the data type does not
	 * support a standard set of values.
	 * @param context A formatter context. Use this object to extract additional
	 * information about the environment from which this converter is being invoked.
	 * Always check whether this value is . Also, properties on the context object may
	 * return .
	 * @return A collection containing  or a standard set of valid values. The default
	 * implementation always returns .
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Determines if this object supports a standard set of values that can be chosen
	 * from a list.
	 * @param context A  through which additional context can be provided.
	 * @return if  must be called to find a common set of values the object supports;
	 * otherwise, .
	 */
	function GetStandardValuesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
