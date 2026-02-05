package cs.system.componentmodel;

/** Provides a simple default implementation of the  interface. */
@:native("System.ComponentModel.CustomTypeDescriptor")
extern class CustomTypeDescriptor {
	/**
	 * Returns a collection of custom attributes for the type represented by this type
	 * descriptor.
	 * @return An  containing the attributes for the type. The default is .
	 */
	function GetAttributes():cs.system.componentmodel.AttributeCollection;
	/**
	 * Returns the fully qualified name of the class represented by this type
	 * descriptor.
	 * @return A  containing the fully qualified class name of the type this type
	 * descriptor is describing. The default is .
	 */
	function GetClassName():String;
	/**
	 * Returns the name of the class represented by this type descriptor.
	 * @return A  containing the name of the component instance this type descriptor is
	 * describing. The default is .
	 */
	function GetComponentName():String;
	/**
	 * Returns a type converter for the type represented by this type descriptor.
	 * @return A  for the type represented by this type descriptor. The default is a
	 * newly created .
	 */
	function GetConverter():cs.system.componentmodel.TypeConverter;
	/**
	 * Returns the event descriptor for the default event of the object represented by
	 * this type descriptor.
	 * @return The  for the default event on the object represented by this type
	 * descriptor. The default is .
	 */
	function GetDefaultEvent():cs.system.componentmodel.EventDescriptor;
	/**
	 * Returns the property descriptor for the default property of the object
	 * represented by this type descriptor.
	 * @return A  for the default property on the object represented by this type
	 * descriptor. The default is .
	 */
	function GetDefaultProperty():cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Returns an editor of the specified type that is to be associated with the class
	 * represented by this type descriptor.
	 * @param editorBaseType The base type of the editor to retrieve.
	 * @return An editor of the given type that is to be associated with the class
	 * represented by this type descriptor. The default is .
	 */
	function GetEditor(editorBaseType:cs.system.Type):Dynamic;
	@:overload(function():cs.system.componentmodel.EventDescriptorCollection {})
	/**
	 * Returns a collection of event descriptors for the object represented by this
	 * type descriptor.
	 * @return An  containing the event descriptors for the object represented by this
	 * type descriptor. The default is .
	 */
	function GetEvents(attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptorCollection;
	@:overload(function():cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Returns a collection of property descriptors for the object represented by this
	 * type descriptor.
	 * @return A  containing the property descriptions for the object represented by
	 * this type descriptor. The default is .
	 */
	function GetProperties(attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns an object that contains the property described by the specified property
	 * descriptor.
	 * @param pd The property descriptor for which to retrieve the owning object.
	 * @return An  that owns the given property specified by the type descriptor. The
	 * default is .
	 */
	function GetPropertyOwner(pd:cs.system.componentmodel.PropertyDescriptor):Dynamic;
}
