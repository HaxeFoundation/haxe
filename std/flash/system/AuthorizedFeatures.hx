package flash.system;

@:require(flash11_3) @:final extern class AuthorizedFeatures {
	function new() : Void;
	function createApplicationInstaller(strings : flash.xml.XML, icon : flash.utils.ByteArray) : ApplicationInstaller;
}
