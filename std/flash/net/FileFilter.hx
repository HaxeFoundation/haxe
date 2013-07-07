package flash.net;

@:final extern class FileFilter {
	var description : String;
	var extension : String;
	var macType : String;
	function new(description : String, extension : String, ?macType : String) : Void;
}
