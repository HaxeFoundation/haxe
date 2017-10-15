package js;

import haxe.extern.Rest;

extern class Syntax {
	@:overload(function(cl:String, args:Rest<Dynamic>):Dynamic {})
	static function new_<T>(cl:Class<T>, args:Rest<Dynamic>):T;

	@:pure static function instanceof(v:Dynamic, cl:Class<Dynamic>):Bool;

	@:pure static function typeof(o:Dynamic):String;

	@:pure static function strictEq(a:Dynamic, b:Dynamic):Bool;
	@:pure static function strictNeq(a:Dynamic, b:Dynamic):Bool;

	static function delete(o:Dynamic, f:String):Void;
}
