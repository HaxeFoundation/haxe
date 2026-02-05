package cs.system.componentmodel.design.serialization;

/** Provides the base class for relating one member to another. */
@:native("System.ComponentModel.Design.Serialization.MemberRelationshipService")
extern class MemberRelationshipService {
	@:overload(function(index0:cs.system.componentmodel.design.serialization.MemberRelationship):cs.system.componentmodel.design.serialization.MemberRelationship {})
	@:native("get_Item")
	function get_Item(index0:Dynamic, index1:cs.system.componentmodel.MemberDescriptor):cs.system.componentmodel.design.serialization.MemberRelationship;
	@:overload(function(index0:cs.system.componentmodel.design.serialization.MemberRelationship, value:cs.system.componentmodel.design.serialization.MemberRelationship):Void {})
	@:native("set_Item")
	function set_Item(index0:Dynamic, index1:cs.system.componentmodel.MemberDescriptor, value:cs.system.componentmodel.design.serialization.MemberRelationship):Void;
	/**
	 * Gets a value indicating whether the given relationship is supported.
	 * @param source The source relationship.
	 * @param relationship The relationship to set into the source.
	 * @return if a relationship between the given two objects is supported; otherwise,
	 * .
	 */
	function SupportsRelationship(source:cs.system.componentmodel.design.serialization.MemberRelationship, relationship:cs.system.componentmodel.design.serialization.MemberRelationship):Bool;
}
