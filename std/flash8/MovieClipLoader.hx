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

	dynamic function onLoadInit(target:MovieClip) : Void;
	dynamic function onLoadStart(target:MovieClip) : Void;
	dynamic function onLoadProgress(target:MovieClip, loaded : Int, total : Int) : Void;
	dynamic function onLoadComplete(target:MovieClip) : Void;
	dynamic function onLoadError(target:MovieClip, error:String) : Void;

#if flash_strict
	function addListener(listener:MclListener):Bool;
	function removeListener(listener:MclListener):Bool;
#else
	function addListener(listener:Dynamic):Bool;
	function removeListener(listener:Dynamic):Bool;
#end

	/** in FP 9 **/
	var checkPolicyFile : Bool;

	private static function __init__() : Void untyped {
		flash.MovieClipLoader = _global["MovieClipLoader"];
	}

}
