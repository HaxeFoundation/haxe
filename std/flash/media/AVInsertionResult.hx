package flash.media;

extern class AVInsertionResult extends AVResult {
	var insertedBeforeReadHead(default,never) : Bool;
	var periodIndex(default,never) : Int;
	function new(result : Int, periodIndex : Int, insertedBeforeReadHead : Bool) : Void;
}
