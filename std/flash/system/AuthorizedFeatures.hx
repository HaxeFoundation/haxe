package flash.system;

@:final @:require(flash11_3) extern class AuthorizedFeatures {
	function new() : Void;
	function createApplicationInstaller(strings : flash.xml.XML, icon : flash.utils.ByteArray) : ApplicationInstaller;
	@:require(flash11_4) function enableDiskCache(stream : flash.net.URLStream) : Bool;
	@:require(flash11_7) function enableHLSPlayback(stream : flash.media.AVStream) : Bool;
	@:require(flash11_4) function isFeatureEnabled(feature : String, ?data : String) : Bool;
	@:require(flash11_4) function isNegativeToken() : Bool;
}
