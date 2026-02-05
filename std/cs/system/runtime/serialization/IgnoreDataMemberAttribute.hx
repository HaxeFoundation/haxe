package cs.system.runtime.serialization;

/** When applied to the member of a type, specifies that the member is not part of a data contract and is not serialized. */
@:native("System.Runtime.Serialization.IgnoreDataMemberAttribute")
extern class IgnoreDataMemberAttribute extends cs.system.Attribute {
	function new():Void;
}
