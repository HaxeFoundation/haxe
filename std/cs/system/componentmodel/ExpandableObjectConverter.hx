package cs.system.componentmodel;

/** Provides a type converter to convert expandable objects to and from various other representations. */
@:native("System.ComponentModel.ExpandableObjectConverter")
extern class ExpandableObjectConverter extends cs.system.componentmodel.TypeConverter {
	function new():Void;
	/**
	 * Gets a collection of properties for the type of object specified by the value
	 * parameter.
	 * @param context An  that provides a format context.
	 * @param value An  that specifies the type of object to get the properties for.
	 * @param attributes An array of type  that will be used as a filter.
	 * @return A  with the properties that are exposed for the component, or  if there
	 * are no properties.
	 */
	function GetProperties(context:cs.system.componentmodel.ITypeDescriptorContext, value:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Gets a value indicating whether this object supports properties using the
	 * specified context.
	 * @param context An  that provides a format context.
	 * @return because  should be called to find the properties of this object. This
	 * method never returns .
	 */
	function GetPropertiesSupported(context:cs.system.componentmodel.ITypeDescriptorContext):Bool;
}
