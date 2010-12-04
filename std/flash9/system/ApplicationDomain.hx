package flash.system;

@:final extern class ApplicationDomain {
	var parentDomain(default,null) : ApplicationDomain;
	function new(?parentDomain : ApplicationDomain) : Void;
	function getDefinition(name : String) : Dynamic;
	function hasDefinition(name : String) : Bool;
	static var currentDomain(default,null) : ApplicationDomain;
}
