package jvm.native.lang;

/**
 * ...
 * @author waneck
 */

extern class Class<T>
{
	function isAssignableFrom(cls:Class<Dynamic>):Bool;
	function getName():String;
}