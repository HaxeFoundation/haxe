package jvm.native.lang;
import jvm.StdTypes;

/**
 * ...
 * @author waneck
 */

@:final extern class Character 
{
	function new(value:Char16):Void;
	
	function charValue():Char16;
	
	static function toLowerCase(ch:Char16):Char16;
	static function toUpperCase(ch:Char16):Char16;
	static function toTitleCase(ch:Char16):Char16;
	static function toString(ch:Char16):String;
}