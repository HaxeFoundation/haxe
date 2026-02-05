package cs.system.componentmodel.design.serialization;

/** Represents a single relationship between an object and a member. */
@:native("System.ComponentModel.Design.Serialization.MemberRelationship")
extern class MemberRelationship extends cs.system.ValueType {
	/** Represents the empty member relationship. This field is read-only. */
	static var Empty(default, never):cs.system.componentmodel.design.serialization.MemberRelationship;
	/**
	 * Gets a value indicating whether this relationship is equal to the  relationship.
	 * @return if this relationship is equal to the  relationship; otherwise, .
	 */
	var IsEmpty(default, never):Bool;
	/**
	 * Gets the related member.
	 * @return The member that is passed in to the .
	 */
	var Member(default, never):cs.system.componentmodel.MemberDescriptor;
	/**
	 * Gets the owning object.
	 * @return The owning object that is passed in to the .
	 */
	var Owner(default, never):Dynamic;
	function new(owner:Dynamic, member:cs.system.componentmodel.MemberDescriptor):Void;
	/**
	 * Tests whether two specified  structures are equivalent.
	 * @param left The  structure that is to the left of the equality operator.
	 * @param right The  structure that is to the right of the equality operator.
	 * @return This operator returns  if the two  structures are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.componentmodel.design.serialization.MemberRelationship, right:cs.system.componentmodel.design.serialization.MemberRelationship):Bool;
	/**
	 * Tests whether two specified  structures are different.
	 * @param left The  structure that is to the left of the inequality operator.
	 * @param right The  structure that is to the right of the inequality operator.
	 * @return This operator returns  if the two  structures are different; otherwise,
	 * .
	 */
	static function op_Inequality(left:cs.system.componentmodel.design.serialization.MemberRelationship, right:cs.system.componentmodel.design.serialization.MemberRelationship):Bool;
	/**
	 * Determines whether two  instances are equal.
	 * @param obj The  to compare with the current .
	 * @return if the specified  is equal to the current ; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
