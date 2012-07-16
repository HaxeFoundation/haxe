package cs.system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.Version') extern class Version
{
	var Build(default, null):Int;
	var Major(default, null):Int;
	var Minor(default, null):Int;
	var Revision(default, null):Int;
}