package java.util.regex;
import java.NativeArray;

extern class Pattern
{
	static function compile(regex:String, flags:Int):Pattern;
	
	function matcher(input:String):Matcher;
	function split(input:String):NativeArray<String>;
	
	static var CANON_EQ(default, null):Int;
	static var CASE_INSENSITIVE(default, null):Int;
	static var COMMENTS(default, null):Int;
	static var DOTALL(default, null):Int;
	static var MULTILINE(default, null):Int;
	static var UNICODE_CASE(default, null):Int;
	static var UNIX_LINES (default, null):Int;
}


extern interface MatchResult
{
	@:overload(function(group:Int):Int {})
	function end():Int;
	
	@:overload(function():String {})
	function group(group:Int):String;
	
	function groupCount():Int;
	
	@:overload(function(group:Int):Int {})
	function start():Int;
}

extern class Matcher implements MatchResult
{
	function reset(input:String):Matcher;
	
	@:overload(function(group:Int):Int {})
	function end():Int;
	
	@:overload(function():String {})
	function group(group:Int):String;
	
	function groupCount():Int;

	@:overload(function(group:Int):Int {})
	function start():Int;
	
	function find():Bool;
	
	function replaceAll(replacement:String):String;
	
	function pattern():Pattern;
}