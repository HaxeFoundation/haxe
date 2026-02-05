package cs.system.componentmodel;

/** Provides a type converter to convert  objects to and from various other representations. */
@:native("System.ComponentModel.BooleanConverter")
extern class BooleanConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a value indicating whether this converter can convert an object in the
	 * given source type to a Boolean object using the specified context.
	 * @param context An  that provides a format context.
	 * @param sourceType A  that represents the type you wish to convert from.
	 * @return if this object can perform the conversion; otherwise, .
	 */
	function CanConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, sourceType:cs.system.Type):Bool;
	/**
	 * Converts the given value object to a Boolean object.
	 * @param context An  that provides a format context.
	 * @param culture A  that specifies the culture to which to convert.
	 * @param value The  to convert.
	 * @return An  that represents the converted .
	 */
	function ConvertFrom(context:cs.system.componentmodel.ITypeDescriptorContext, culture:cs.system.globalization.CultureInfo, value:Dynamic):Dynamic;
	/**
	 * Gets a collection of standard values for the Boolean data type.
	 * @param context An  that provides a format context.
	 * @return A  that holds a standard set of valid values.
	 */
	function GetStandardValues(context:cs.system.componentmodel.ITypeDescriptorContext):cs.system.componentmodel.TypeConverter_StandardValuesCollection;
	/**
	 * Gets a value indicating whether the list of standard values returned from the 
	 * method is an exclusive list.
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
