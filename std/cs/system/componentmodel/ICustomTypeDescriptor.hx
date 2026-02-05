package cs.system.componentmodel;

/** Provides an interface that supplies dynamic custom type information for an object. */
@:native("System.ComponentModel.ICustomTypeDescriptor")
extern interface ICustomTypeDescriptor {
	/**
	 * Returns a collection of custom attributes for this instance of a component.
	 * @return An  containing the attributes for this object.
	 */
	function GetAttributes():cs.system.componentmodel.AttributeCollection;
	/**
	 * Returns the class name of this instance of a component.
	 * @return The class name of the object, or  if the class does not have a name.
	 */
	function GetClassName():String;
	/**
	 * Returns the name of this instance of a component.
	 * @return The name of the object, or  if the object does not have a name.
	 */
	function GetComponentName():String;
	/**
	 * Returns a type converter for this instance of a component.
	 * @return A  that is the converter for this object, or  if there is no  for this
	 * object.
	 */
	function GetConverter():cs.system.componentmodel.TypeConverter;
	/**
	 * Returns the default event for this instance of a component.
	 * @return An  that represents the default event for this object, or  if this
	 * object does not have events.
	 */
	function GetDefaultEvent():cs.system.componentmodel.EventDescriptor;
	/**
	 * Returns the default property for this instance of a component.
	 * @return A  that represents the default property for this object, or  if this
	 * object does not have properties.
	 */
	function GetDefaultProperty():cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Returns an editor of the specified type for this instance of a component.
	 * @param editorBaseType A  that represents the editor for this object.
	 * @return An  of the specified type that is the editor for this object, or  if the
	 * editor cannot be found.
	 */
	function GetEditor(editorBaseType:cs.system.Type):Dynamic;
	@:overload(function():cs.system.componentmodel.EventDescriptorCollection {})
	/**
	 * Returns the events for this instance of a component.
	 * @return An  that represents the events for this component instance.
	 */
	function GetEvents(attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptorCollection;
	@:overload(function():cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Returns the properties for this instance of a component.
	 * @return A  that represents the properties for this component instance.
	 */
	function GetProperties(attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Returns an object that contains the property described by the specified property
	 * descriptor.
	 * @param pd A  that represents the property whose owner is to be found.
	 * @return An  that represents the owner of the specified property.
	 */
	function GetPropertyOwner(pd:cs.system.componentmodel.PropertyDescriptor):Dynamic;
}
