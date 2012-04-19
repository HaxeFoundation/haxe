package java.util.regex;

/**
 * ...
 * @author waneck
 */

extern class Pattern
{
	static function compile(regex:String):Pattern;
	
	function matcher(input:String):Matcher;
}


extern interface MatchResult
{
	@:overload(function(group:Int):Int {})
	function end():Int;
	
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
	
	function group(group:Int):String;
	
	function groupCount():Int;

	@:overload(function(group:Int):Int {})
	function start():Int;
	
	function find():Bool;
	
	function replaceAll(replacement:String):String;
}