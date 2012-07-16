package cs.system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.PlatformID') extern enum PlatformID
{
	Win32S;
	Win32Windows;
	Win32NT;
	WinCE;
	Unix;
	Xbox;
	MacOSX;
}