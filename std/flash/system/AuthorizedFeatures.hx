package flash.system;

@:final @:require(flash11_3) extern class AuthorizedFeatures {
	function new() : Void;
	function createApplicationInstaller(strings : flash.xml.XML, icon : flash.utils.ByteArray) : ApplicationInstaller;
}
