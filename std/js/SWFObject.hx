package js;

@:initPackage
extern class SWFObject {

	function new( swf : String, id : String, width : Int, height : Int, ver : String, color : String /*...*/ ) : Void;
	function addParam( param : String, value : String ) : Void;
	function getSWFHTML() : String;
	function write( elementId : String ) : Bool;
	function addVariable( param : String, value : String ) : Void;
	function setAttribute( id : String, value : String ) : Void;

	private static function __init__() : Void untyped {
		#if !embedJs
		haxe.macro.Tools.includeFile("js/swfobject-1.5.js");
		#end
		js.SWFObject = deconcept.SWFObject;
	}

}