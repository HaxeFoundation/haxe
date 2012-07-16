package cs.system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.OperatingSystem') extern class OperatingSystem
{
	var Platform(default, null):cs.system.PlatformID;
	var Version(default, null):cs.system.Version;
}