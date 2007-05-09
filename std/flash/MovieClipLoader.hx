package flash;

#if flash_strict
typedef MclListener = {

	function onLoadInit(target:MovieClip) : Void;
	function onLoadStart(target:MovieClip) : Void;
	function onLoadProgress(target:MovieClip, loaded : Int, total : Int) : Void;
	function onLoadComplete(target:MovieClip, httpStatus : Int) : Void;
	function onLoadError(target:MovieClip, error:String, httpStatus : Int) : Void;

}
#end

extern class MovieClipLoader
{
	// don't allow target : String
	// and target : Int (_levelX ?)

	function new() : Void;
	function getProgress(target:MovieClip): { bytesLoaded : Int, bytesTotal : Int };
	function loadClip(url:String, target:MovieClip):Bool;
	function unloadClip(target:MovieClip):Bool;

	function onLoadInit(target:MovieClip) : Void;
	function onLoadStart(target:MovieClip) : Void;
	function onLoadProgress(target:MovieClip, loaded : Int, total : Int) : Void;
	function onLoadComplete(target:MovieClip) : Void;
	function onLoadError(target:MovieClip, error:String) : Void;

#if flash_strict
	function addListener(listener:MclListener):Bool;
	function removeListener(listener:MclListener):Bool;
#else true
	function addListener(listener:Dynamic):Bool;
	function removeListener(listener:Dynamic):Bool;
#end

#if flash_v9
	var checkPolicyFile : Bool;
#end

	private static function __init__() : Void untyped {
		flash.MovieClipLoader = _global["MovieClipLoader"];
	}

}
