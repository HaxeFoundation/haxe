package flash.system;

extern class ApplicationDomain {
	var parentDomain(default,null) : ApplicationDomain;
	function new(?parentDomain : ApplicationDomain) : Void;
	function getDefinition(name : String) : Dynamic;
	function hasDefinition(name : String) : Bool;
	static var currentDomain(default,null) : ApplicationDomain;
	#if flash10
	static var MIN_DOMAIN_MEMORY_LENGTH(default,null) : UInt;
	var domainMemory : flash.utils.ByteArray;
	#end
}
