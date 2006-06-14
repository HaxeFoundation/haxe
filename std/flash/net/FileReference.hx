package flash.net;

extern class FileReference {

	var creator : String;
	var creationDate : Date;
	var modificationDate : Date;
	var size : Float;
	var type : String;
	var name : String;

	function new() : Void;

	function browse( ?typeList : Array<Dynamic> ) : Bool;
	function upload( url : String ) : Bool;
	function download( url : String, ?defaultName : String ) : Bool;
	function cancel() : Void;

	function addListener( listener : Dynamic ) : Void;
	function removeListener( listener : Dynamic ) : Bool;

}