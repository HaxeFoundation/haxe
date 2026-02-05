package cs.system.componentmodel;

/** Provides an abstraction of a property on a class. */
@:native("System.ComponentModel.PropertyDescriptor")
extern class PropertyDescriptor extends cs.system.componentmodel.MemberDescriptor {
	/**
	 * When overridden in a derived class, gets the type of the component this property
	 * is bound to.
	 * @return A  that represents the type of component this property is bound to. When
	 * the  or  methods are invoked, the object specified might be an instance of this
	 * type.
	 */
	var ComponentType(default, never):cs.system.Type;
	/**
	 * Gets the type converter for this property.
	 * @return A  that is used to convert the  of this property.
	 */
	var Converter(default, never):cs.system.componentmodel.TypeConverter;
	/**
	 * Gets a value indicating whether this property should be localized, as specified
	 * in the .
	 * @return if the member is marked with the  set to ; otherwise, .
	 */
	var IsLocalizable(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether this
	 * property is read-only.
	 * @return if the property is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * When overridden in a derived class, gets the type of the property.
	 * @return A  that represents the type of the property.
	 */
	var PropertyType(default, never):cs.system.Type;
	/**
	 * Gets a value indicating whether this property should be serialized, as specified
	 * in the .
	 * @return One of the  enumeration values that specifies whether this property
	 * should be serialized.
	 */
	var SerializationVisibility(default, never):cs.system.componentmodel.DesignerSerializationVisibility;
	/**
	 * Gets a value indicating whether value change notifications for this property may
	 * originate from outside the property descriptor.
	 * @return if value change notifications may originate from outside the property
	 * descriptor; otherwise, .
	 */
	var SupportsChangeEvents(default, never):Bool;
	/**
	 * Enables other objects to be notified when this property changes.
	 * @param component The component to add the handler for.
	 * @param handler The delegate to add as a listener.
	 */
	function AddValueChanged(component:Dynamic, handler:cs.system.EventHandler):Void;
	/**
	 * When overridden in a derived class, returns whether resetting an object changes
	 * its value.
	 * @param component The component to test for reset capability.
	 * @return if resetting the component changes its value; otherwise, .
	 */
	function CanResetValue(component:Dynamic):Bool;
	/**
	 * Compares this to another object to see if they are equivalent.
	 * @param obj The object to compare to this .
	 * @return if the values are equivalent; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	@:overload(function():cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(filter:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection {})
	@:overload(function(instance:Dynamic):cs.system.componentmodel.PropertyDescriptorCollection {})
	/**
	 * Returns the default .
	 * @return A collection of property descriptor.
	 */
	function GetChildProperties(instance:Dynamic, filter:cs.NativeArray<cs.system.Attribute>):cs.system.componentmodel.PropertyDescriptorCollection;
	/**
	 * Gets an editor of the specified type.
	 * @param editorBaseType The base type of editor, which is used to differentiate
	 * between multiple editors that a property supports.
	 * @return An instance of the requested editor type, or  if an editor cannot be
	 * found.
	 */
	function GetEditor(editorBaseType:cs.system.Type):Dynamic;
	/**
	 * Returns the hash code for this object.
	 * @return The hash code for this object.
	 */
	function GetHashCode():Int;
	/**
	 * When overridden in a derived class, gets the current value of the property on a
	 * component.
	 * @param component The component with the property for which to retrieve the
	 * value.
	 * @return The value of a property for a given component.
	 */
	function GetValue(component:Dynamic):Dynamic;
	/**
	 * Enables other objects to be notified when this property changes.
	 * @param component The component to remove the handler for.
	 * @param handler The delegate to remove as a listener.
	 */
	function RemoveValueChanged(component:Dynamic, handler:cs.system.EventHandler):Void;
	/**
	 * When overridden in a derived class, resets the value for this property of the
	 * component to the default value.
	 * @param component The component with the property value that is to be reset to
	 * the default value.
	 */
	function ResetValue(component:Dynamic):Void;
	/**
	 * When overridden in a derived class, sets the value of the component to a
	 * different value.
	 * @param component The component with the property value that is to be set.
	 * @param value The new value.
	 */
	function SetValue(component:Dynamic, value:Dynamic):Void;
	/**
	 * When overridden in a derived class, determines a value indicating whether the
	 * value of this property needs to be persisted.
	 * @param component The component with the property to be examined for persistence.
	 * @return if the property should be persisted; otherwise, .
	 */
	function ShouldSerializeValue(component:Dynamic):Bool;
}
