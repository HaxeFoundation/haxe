package cs.system.componentmodel;

/** Provides a top-level mapping layer between a COM object and a . */
@:native("System.ComponentModel.IComNativeDescriptorHandler")
extern interface IComNativeDescriptorHandler {
	/**
	 * Gets the attributes for the specified component.
	 * @param component The component to get attributes for.
	 * @return A collection of attributes for .
	 */
	function GetAttributes(component:Dynamic):cs.system.componentmodel.AttributeCollection;
	/**
	 * Gets the class name for the specified component.
	 * @param component The component to get the class name for.
	 * @return The name of the class that corresponds with .
	 */
	function GetClassName(component:Dynamic):String;
	/**
	 * Gets the type converter for the specified component.
	 * @param component The component to get the  for.
	 * @return The  for .
	 */
	function GetConverter(component:Dynamic):cs.system.componentmodel.TypeConverter;
	/**
	 * Gets the default event for the specified component.
	 * @param component The component to get the default event for.
	 * @return An  that represents 's default event.
	 */
	function GetDefaultEvent(component:Dynamic):cs.system.componentmodel.EventDescriptor;
	/**
	 * Gets the default property for the specified component.
	 * @param component The component to get the default property for.
	 * @return A  that represents 's default property.
	 */
	function GetDefaultProperty(component:Dynamic):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Gets the editor for the specified component.
	 * @param component The component to get the editor for.
	 * @param baseEditorType The base type of the editor for .
	 * @return The editor for .
	 */
	function GetEditor(component:Dynamic, baseEditorType:cs.system.Type):Dynamic;
	@:overload(function(component:Dynamic):cs.system.componentmodel.EventDescriptorCollection {})
	/**
	 * Gets the events for the specified component.
	 * @param component The component to get events for.
	 * @return A collection of event descriptors for .
	 */
	function GetEvents(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptorCollection;
	/**
	 * Gets the name of the specified component.
	 * @param component The component to get the name of.
	 * @return The name of .
	 */
	function GetName(component:Dynamic):String;
	/**
	 * Gets the properties with the specified attributes for the specified component.
	 * @param component The component to get events for.
	 * @param attributes The attributes used to filter properties.
	 * @return A collection of property descriptors for .
	 */
	function GetProperties(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	@:overload(function(component:Dynamic, dispid:Int, success:cs.Ref<Bool>):Dynamic {})
	/**
	 * Gets the value of the property that has the specified dispatch identifier.
	 * @param component The object to which the property belongs.
	 * @param dispid The dispatch identifier.
	 * @param success A , passed by reference, that represents whether the property was
	 * retrieved.
	 * @return The value of the property that has the specified dispatch identifier.
	 */
	function GetPropertyValue(component:Dynamic, propertyName:String, success:cs.Ref<Bool>):Dynamic;
}
