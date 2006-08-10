package flash.net;

extern class FileFilter {
	function new(description : String, extension : String, ?macType : String) : Void;
	var description : String;
	var extension : String;
	var macType : String;
}
