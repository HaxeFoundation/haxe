package cs.system.componentmodel;

/** Provides information about the characteristics for a component, such as its attributes, properties, and events. This class cannot be inherited. */
@:native("System.ComponentModel.TypeDescriptor")
extern class TypeDescriptor {
	/**
	 * Gets or sets the provider for the Component Object Model (COM) type information
	 * for the target component.
	 * @return An  instance representing the COM type information provider.
	 */
	static var ComNativeDescriptorHandler(default, default):cs.system.componentmodel.IComNativeDescriptorHandler;
	/**
	 * Gets the type of the Component Object Model (COM) object represented by the
	 * target component.
	 * @return The  of the COM object represented by this component, or  for non-COM
	 * objects.
	 */
	static var ComObjectType(default, never):cs.system.Type;
	/**
	 * Gets a type that represents a type description provider for all interface types.
	 * @return A  that represents a custom type description provider for all interface
	 * types.
	 */
	static var InterfaceType(default, never):cs.system.Type;
	@:overload(function(instance:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.TypeDescriptionProvider {})
	/**
	 * Adds class-level attributes to the target component instance.
	 * @param instance An instance of the target component.
	 * @param attributes An array of  objects to add to the component's class.
	 * @return The newly created  that was used to add the specified attributes.
	 */
	static function AddAttributes(type:cs.system.Type, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.TypeDescriptionProvider;
	/**
	 * Adds an editor table for the given editor base type.
	 * @param editorBaseType The editor base type to add the editor table for. If a
	 * table already exists for this type, this method will do nothing.
	 * @param table The  to add.
	 */
	static function AddEditorTable(editorBaseType:cs.system.Type, table:cs.system.collections.Hashtable):Void;
	@:overload(function(provider:cs.system.componentmodel.TypeDescriptionProvider, instance:Dynamic):Void {})
	/**
	 * Adds a type description provider for a single instance of a component.
	 * @param provider The  to add.
	 * @param instance An instance of the target component.
	 */
	static function AddProvider(provider:cs.system.componentmodel.TypeDescriptionProvider, type:cs.system.Type):Void;
	@:overload(function(provider:cs.system.componentmodel.TypeDescriptionProvider, instance:Dynamic):Void {})
	/**
	 * Adds a type description provider for a single instance of a component.
	 * @param provider The  to add.
	 * @param instance An instance of the target component.
	 */
	static function AddProviderTransparent(provider:cs.system.componentmodel.TypeDescriptionProvider, type:cs.system.Type):Void;
	/**
	 * Creates a primary-secondary association between two objects.
	 * @param primary The primary .
	 * @param secondary The secondary .
	 */
	static function CreateAssociation(primary:Dynamic, secondary:Dynamic):Void;
	/**
	 * Creates an instance of the designer associated with the specified component and
	 * of the specified type of designer.
	 * @param component An  that specifies the component to associate with the
	 * designer.
	 * @param designerBaseType A  that represents the type of designer to create.
	 * @return An  that is an instance of the designer for the component, or  if no
	 * designer can be found.
	 */
	static function CreateDesigner(component:cs.system.componentmodel.IComponent, designerBaseType:cs.system.Type):cs.system.componentmodel.design.IDesigner;
	@:overload(function(componentType:cs.system.Type, oldEventDescriptor:cs.system.componentmodel.EventDescriptor, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptor {})
	/**
	 * Creates a new event descriptor that is identical to an existing event
	 * descriptor, when passed the existing .
	 * @param componentType The type of the component for which to create the new
	 * event.
	 * @param oldEventDescriptor The existing event information.
	 * @param attributes The new attributes.
	 * @return A new  that has merged the specified metadata attributes with the
	 * existing metadata attributes.
	 */
	static function CreateEvent(componentType:cs.system.Type, name:String, type:cs.system.Type, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptor;
	/**
	 * Creates an object that can substitute for another data type.
	 * @param provider The service provider that provides a  service. This parameter
	 * can be .
	 * @param objectType The  of object to create.
	 * @param argTypes An optional array of parameter types to be passed to the
	 * object's constructor. This parameter can be  or an array of zero length.
	 * @param args An optional array of parameter values to pass to the object's
	 * constructor. If not , the number of elements must be the same as .
	 * @return An instance of the substitute data type if an associated  is found;
	 * otherwise, .
	 */
	static function CreateInstance(provider:cs.system.IServiceProvider, objectType:cs.system.Type, argTypes:cs.NativeArray<cs.system.Type>, args:cs.NativeArray<Dynamic>):Dynamic;
	@:overload(function(componentType:cs.system.Type, oldPropertyDescriptor:cs.system.componentmodel.PropertyDescriptor, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptor {})
	/**
	 * Creates a new property descriptor from an existing property descriptor, using
	 * the specified existing  and attribute array.
	 * @param componentType The  of the component that the property is a member of.
	 * @param oldPropertyDescriptor The existing property descriptor.
	 * @param attributes The new attributes for this property.
	 * @return A new  that has the specified metadata attributes merged with the
	 * existing metadata attributes.
	 */
	static function CreateProperty(componentType:cs.system.Type, name:String, type:cs.system.Type, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptor;
	/**
	 * Returns an instance of the type associated with the specified primary object.
	 * @param type The  of the target component.
	 * @param primary The primary object of the association.
	 * @return An instance of the secondary type that has been associated with the
	 * primary object if an association exists; otherwise,  if no specified association
	 * exists.
	 */
	static function GetAssociation(type:cs.system.Type, primary:Dynamic):Dynamic;
	@:overload(function(component:Dynamic):cs.system.componentmodel.AttributeCollection {})
	@:overload(function(componentType:cs.system.Type):cs.system.componentmodel.AttributeCollection {})
	/**
	 * Returns the collection of attributes for the specified component.
	 * @param component The component for which you want to get attributes.
	 * @return An  containing the attributes for the component. If  is , this method
	 * returns an empty collection.
	 */
	static function GetAttributes(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.AttributeCollection;
	@:overload(function(component:Dynamic):String {})
	@:overload(function(componentType:cs.system.Type):String {})
	/**
	 * Returns the name of the class for the specified component using the default type
	 * descriptor.
	 * @param component The  for which you want the class name.
	 * @return A  containing the name of the class for the specified component.
	 */
	static function GetClassName(component:Dynamic, noCustomTypeDesc:Bool):String;
	@:overload(function(component:Dynamic):String {})
	/**
	 * Returns the name of the specified component using the default type descriptor.
	 * @param component The  for which you want the class name.
	 * @return A  containing the name of the specified component, or  if there is no
	 * component name.
	 */
	static function GetComponentName(component:Dynamic, noCustomTypeDesc:Bool):String;
	@:overload(function(component:Dynamic):cs.system.componentmodel.TypeConverter {})
	@:overload(function(type:cs.system.Type):cs.system.componentmodel.TypeConverter {})
	/**
	 * Returns a type converter for the type of the specified component.
	 * @param component A component to get the converter for.
	 * @return A  for the specified component.
	 */
	static function GetConverter(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.TypeConverter;
	@:overload(function(component:Dynamic):cs.system.componentmodel.EventDescriptor {})
	@:overload(function(componentType:cs.system.Type):cs.system.componentmodel.EventDescriptor {})
	/**
	 * Returns the default event for the specified component.
	 * @param component The component to get the event for.
	 * @return An  with the default event, or  if there are no events.
	 */
	static function GetDefaultEvent(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.EventDescriptor;
	@:overload(function(component:Dynamic):cs.system.componentmodel.PropertyDescriptor {})
	@:overload(function(componentType:cs.system.Type):cs.system.componentmodel.PropertyDescriptor {})
	/**
	 * Returns the default property for the specified component.
	 * @param component The component to get the default property for.
	 * @return A  with the default property, or  if there are no properties.
	 */
	static function GetDefaultProperty(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.PropertyDescriptor;
	@:overload(function(component:Dynamic, editorBaseType:cs.system.Type):Dynamic {})
	@:overload(function(type:cs.system.Type, editorBaseType:cs.system.Type):Dynamic {})
	/**
	 * Gets an editor with the specified base type for the specified component.
	 * @param component The component to get the editor for.
	 * @param editorBaseType A  that represents the base type of the editor you want to
	 * find.
	 * @return An instance of the editor that can be cast to the specified editor type,
	 * or  if no editor of the requested type can be found.
	 */
	static function GetEditor(component:Dynamic, editorBaseType:cs.system.Type, noCustomTypeDesc:Bool):Dynamic;
	@:overload(function(component:Dynamic):cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(componentType:cs.system.Type):cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.EventDescriptorCollection {})
	@:overload(function(componentType:cs.system.Type, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.EventDescriptorCollection {})
	/**
	 * Returns the collection of events for the specified component.
	 * @param component A component to get the events for.
	 * @return An  with the events for this component.
	 */
	static function GetEvents(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>, noCustomTypeDesc:Bool):cs.system.componentmodel.EventDescriptorCollection;
	/**
	 * Returns the fully qualified name of the component.
	 * @param component The  to find the name for.
	 * @return The fully qualified name of the specified component, or  if the
	 * component has no name.
	 */
	static function GetFullComponentName(component:Dynamic):String;
	@:overload(function(component:Dynamic):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(componentType:cs.system.Type):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(component:Dynamic, noCustomTypeDesc:Bool):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(componentType:cs.system.Type, attributes:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Returns the collection of properties for a specified component.
	 * @param component A component to get the properties for.
	 * @return A  with the properties for the specified component.
	 */
	static function GetProperties(component:Dynamic, attributes:cs.NativeArray<cs.system.Attribute>, noCustomTypeDesc:Bool):cs.system.componentmodel.PropertyDescriptorCollection;
	@:overload(function(instance:Dynamic):cs.system.componentmodel.TypeDescriptionProvider {})
	/**
	 * Returns the type description provider for the specified component.
	 * @param instance An instance of the target component.
	 * @return A  associated with the specified component.
	 */
	static function GetProvider(type:cs.system.Type):cs.system.componentmodel.TypeDescriptionProvider;
	@:overload(function(instance:Dynamic):cs.system.Type {})
	/**
	 * Returns a  that can be used to perform reflection, given an object.
	 * @param instance An instance of the target component.
	 * @return A  for the specified object.
	 */
	static function GetReflectionType(type:cs.system.Type):cs.system.Type;
	@:overload(function(component:Dynamic):Void {})
	@:overload(function(assembly:cs.system.reflection.Assembly):Void {})
	@:overload(function(module:cs.system.reflection.Module):Void {})
	/**
	 * Clears the properties and events for the specified component from the cache.
	 * @param component A component for which the properties or events have changed.
	 */
	static function Refresh(type:cs.system.Type):Void;
	/**
	 * Removes an association between two objects.
	 * @param primary The primary .
	 * @param secondary The secondary .
	 */
	static function RemoveAssociation(primary:Dynamic, secondary:Dynamic):Void;
	/**
	 * Removes all associations for a primary object.
	 * @param primary The primary  in an association.
	 */
	static function RemoveAssociations(primary:Dynamic):Void;
	@:overload(function(provider:cs.system.componentmodel.TypeDescriptionProvider, instance:Dynamic):Void {})
	/**
	 * Removes a previously added type description provider that is associated with the
	 * specified object.
	 * @param provider The  to remove.
	 * @param instance An instance of the target component.
	 */
	static function RemoveProvider(provider:cs.system.componentmodel.TypeDescriptionProvider, type:cs.system.Type):Void;
	@:overload(function(provider:cs.system.componentmodel.TypeDescriptionProvider, instance:Dynamic):Void {})
	/**
	 * Removes a previously added type description provider that is associated with the
	 * specified object.
	 * @param provider The  to remove.
	 * @param instance An instance of the target component.
	 */
	static function RemoveProviderTransparent(provider:cs.system.componentmodel.TypeDescriptionProvider, type:cs.system.Type):Void;
	/**
	 * Sorts descriptors using the name of the descriptor.
	 * @param infos An  that contains the descriptors to sort.
	 */
	static function SortDescriptorArray(infos:cs.system.collections.IList):Void;
}
