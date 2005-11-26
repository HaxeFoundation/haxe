extern class MovieClipLoader
{
	function new() : Void;
	function addListener(listener:Dynamic):Bool;
	function getProgress(target:Dynamic):Dynamic;
	function loadClip(url:String, target:Dynamic):Bool;
	function removeListener(listener:Dynamic):Bool;
	function unloadClip(target:Dynamic):Bool;
}
