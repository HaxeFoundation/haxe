package cs.system.collections.generic;

@:native("System.Collections.Generic.LinkedListNode")
extern class LinkedListNode<T> {
	var List(default, never):cs.system.collections.generic.LinkedList<T>;
	var Next(default, never):cs.system.collections.generic.LinkedListNode<T>;
	var Previous(default, never):cs.system.collections.generic.LinkedListNode<T>;
	var Value(default, default):T;
	function new(value:T):Void;
}
