package flash.system;

extern class ApplicationDomain {
	function new(?parentDomain : flash.system.ApplicationDomain) : Void;
	function getDefinition(name : String) : Dynamic;
	function hasDefinition(name : String) : Bool;
	var parentDomain(default,null) : flash.system.ApplicationDomain;
	static var currentDomain(default,null) : flash.system.ApplicationDomain;

	#if flash10
	static var MIN_DOMAIN_MEMORY_LENGTH(default,null) : UInt;
	var domainMemory : flash.utils.ByteArray;
	#end
}
