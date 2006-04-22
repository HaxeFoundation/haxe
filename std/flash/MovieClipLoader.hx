package flash;

extern class MovieClipLoader
{
	function new() : Void;
	function addListener(listener:Dynamic):Bool;
	function getProgress(target:Dynamic):Dynamic;
	function loadClip(url:String, target:Dynamic):Bool;
	function removeListener(listener:Dynamic):Bool;
	function unloadClip(target:Dynamic):Bool;

	function onLoadInit(target:MovieClip) : Void;
	function onLoadStart(target:MovieClip) : Void;
	function onLoadProgress(target:MovieClip, loaded : Int, total : Int) : Void;
	function onLoadComplete(target:MovieClip) : Void;
	function onLoadError(target:MovieClip, error:String) : Void;
}
