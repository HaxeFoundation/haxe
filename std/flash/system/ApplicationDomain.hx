package flash.system;

@:final extern class ApplicationDomain {
	@:require(flash10) var domainMemory : flash.utils.ByteArray;
	var parentDomain(default,null) : ApplicationDomain;
	function new(?parentDomain : ApplicationDomain) : Void;
	function getDefinition(name : String) : flash.utils.Object;
	@:require(flash11_3) function getQualifiedDefinitionNames() : flash.Vector<String>;
	function hasDefinition(name : String) : Bool;
	@:require(flash10) static var MIN_DOMAIN_MEMORY_LENGTH(default,null) : UInt;
	static var currentDomain(default,null) : ApplicationDomain;
}
