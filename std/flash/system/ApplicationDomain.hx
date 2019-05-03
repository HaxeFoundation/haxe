package flash.system;

extern final class ApplicationDomain {
	@:require(flash10) var domainMemory(get,set) : flash.utils.ByteArray;
	var parentDomain(get,never) : ApplicationDomain;
	function new(?parentDomain : ApplicationDomain) : Void;
	function getDefinition(name : String) : flash.utils.Object;
	@:require(flash11_3) function getQualifiedDefinitionNames() : flash.Vector<String>;
	private function get_domainMemory() : flash.utils.ByteArray;
	private function get_parentDomain() : ApplicationDomain;
	function hasDefinition(name : String) : Bool;
	private function set_domainMemory(value : flash.utils.ByteArray) : flash.utils.ByteArray;
	@:require(flash10) static var MIN_DOMAIN_MEMORY_LENGTH(get,never) : UInt;
	static var currentDomain(get,never) : ApplicationDomain;
	private static function get_MIN_DOMAIN_MEMORY_LENGTH() : UInt;
	private static function get_currentDomain() : ApplicationDomain;
}
