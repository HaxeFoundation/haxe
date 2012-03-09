package flash.net;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class FileReferenceList {

	var fileList : Array<Dynamic>;

	function new() : Void;

	function browse( ?typeList : Array<Dynamic> ) : Bool;
	function addListener( listener : Dynamic ) : Void;
	function removeListener( listener : Dynamic ) : Bool;

}