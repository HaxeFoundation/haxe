package cs.system.componentmodel.design.serialization;

/** Provides the base class for serializing a set of components or serializable objects into a serialization store. */
@:native("System.ComponentModel.Design.Serialization.ComponentSerializationService")
extern class ComponentSerializationService {
	/**
	 * Creates a new .
	 * @return A new created serialization store.
	 */
	function CreateStore():cs.system.componentmodel.design.serialization.SerializationStore;
	@:overload(function(store:cs.system.componentmodel.design.serialization.SerializationStore):cs.system.collections.ICollection {})
	/**
	 * Deserializes the given store to produce a collection of objects.
	 * @param store The  to deserialize.
	 * @return A collection of objects created according to the stored state.
	 */
	function Deserialize(store:cs.system.componentmodel.design.serialization.SerializationStore, container:cs.system.componentmodel.IContainer):cs.system.collections.ICollection;
	@:overload(function(store:cs.system.componentmodel.design.serialization.SerializationStore, container:cs.system.componentmodel.IContainer):Void {})
	@:overload(function(store:cs.system.componentmodel.design.serialization.SerializationStore, container:cs.system.componentmodel.IContainer, validateRecycledTypes:Bool):Void {})
	/**
	 * Deserializes the given  to the given container.
	 * @param store The  to deserialize.
	 * @param container The container to which  objects will be added.
	 */
	function DeserializeTo(store:cs.system.componentmodel.design.serialization.SerializationStore, container:cs.system.componentmodel.IContainer, validateRecycledTypes:Bool, applyDefaults:Bool):Void;
	/**
	 * Loads a  from a stream.
	 * @param stream The  from which the store will be loaded.
	 * @return A new  instance.
	 */
	function LoadStore(stream:cs.system.io.Stream):cs.system.componentmodel.design.serialization.SerializationStore;
	/**
	 * Serializes the given object to the given .
	 * @param store The  to which the state of  will be written.
	 * @param value The object to serialize.
	 */
	function Serialize(store:cs.system.componentmodel.design.serialization.SerializationStore, value:Dynamic):Void;
	/**
	 * Serializes the given object, accounting for default property values.
	 * @param store The  to which the state of  will be serialized.
	 * @param value The object to serialize.
	 */
	function SerializeAbsolute(store:cs.system.componentmodel.design.serialization.SerializationStore, value:Dynamic):Void;
	/**
	 * Serializes the given member on the given object.
	 * @param store The  to which the state of  will be serialized.
	 * @param owningObject The object to which  is attached.
	 * @param member A  specifying the member to serialize.
	 */
	function SerializeMember(store:cs.system.componentmodel.design.serialization.SerializationStore, owningObject:Dynamic, member:cs.system.componentmodel.MemberDescriptor):Void;
	/**
	 * Serializes the given member on the given object, accounting for the default
	 * property value.
	 * @param store The  to which the state of  will be serialized.
	 * @param owningObject The object to which  is attached.
	 * @param member The member to serialize.
	 */
	function SerializeMemberAbsolute(store:cs.system.componentmodel.design.serialization.SerializationStore, owningObject:Dynamic, member:cs.system.componentmodel.MemberDescriptor):Void;
}
