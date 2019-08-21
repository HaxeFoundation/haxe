package flash.system;

@:require(flash11_3) extern final class AuthorizedFeatures {
	function new() : Void;
	function createApplicationInstaller(strings : flash.xml.XML, icon : flash.utils.ByteArray) : ApplicationInstaller;
	function enableAVLoader(loader : flash.display.AVLoader) : Bool;
	function enableAVURLLoader(loader : flash.media.AVURLLoader) : Bool;
	function enableAVURLStream(stream : flash.media.AVURLStream) : Bool;
	@:require(flash11_4) function enableDiskCache(stream : flash.net.URLStream) : Bool;
	@:require(flash11_7) function enableHLSPlayback(stream : flash.media.AVStream) : Bool;
	function enableMediaPlayerHLSPlayback(player : Dynamic) : Bool;
	@:ns("flash.system",internal) @:require(flash11_4) function isFeatureEnabled(feature : String, ?data : String) : Bool;
	@:ns("flash.system",internal) @:require(flash11_4) function isNegativeToken() : Bool;
}
