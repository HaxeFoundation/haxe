package haxe;

import haxe.extern.Rest;
import haxe.Log;

extern class Trace {
	/**
		Print given arguments
	**/
	#if !no_traces
	static function trace(arg:Dynamic, rest:Rest<Dynamic>):Void;
	#else
	static inline function trace(arg:Dynamic, rest:Rest<Dynamic>):Void {}
	#end
}
