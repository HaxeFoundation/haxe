package cs.system.text.regularExpressions;
import cs.NativeArray;

@:native('System.Text.RegularExpressions.Regex') extern class Regex 
{
	function new(pattern:String, options:RegexOptions):Void;
	function Match(input:String):Match;
	function Split(input:String):NativeArray<String>;
	function Replace(input:String, replacement:String):String;
}

@:native("System.Text.RegularExpressions.RegexOptions") extern enum RegexOptions
{
	None;
	IgnoreCase;
	Multiline;
	ExplicitCapture;
	Compiled;
	Singleline;
	IgnorePatternWhitespace;
	RightToLeft;
	ECMAScript;
	CultureInvariant;
}

@:native("System.Text.RegularExpressions.Capture") extern class Capture
{
	var Index(default, null):Int;
	var Length(default, null):Int;
	var Value(default, null):String;
}

@:native("System.Text.RegularExpressions.Group") extern class Group extends Capture
{
	var Success(default, null):Bool;
}

@:native("System.Text.RegularExpressions.Match") extern class Match extends Group
{
	var Captures(default, null):CaptureCollection;
	var Groups(default, null):GroupCollection;
}

@:native("System.Text.RegularExpressions.CaptureCollection") extern class CaptureCollection implements ArrayAccess<Capture>
{
	var Count(default, null):Int;
}

@:native("System.Text.RegularExpressions.GroupCollection") extern class GroupCollection implements ArrayAccess<Group>
{
	var Count(default, null):Int;
}