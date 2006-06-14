package flash.net;

extern class FileReferenceList {

	var fileList : Array<Dynamic>;

	function new() : Void;

	function browse( ?typeList : Array<Dynamic> ) : Bool;
	function addListener( listener : Dynamic ) : Void;
	function removeListener( listener : Dynamic ) : Bool;

}