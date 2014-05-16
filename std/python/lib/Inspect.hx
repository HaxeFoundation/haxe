
package python.lib;

import python.Syntax;

@:pythonImport("inspect")
extern class Inspect {

	static function getmembers (value:Dynamic, ?filter:Dynamic->Bool):Bool;
	static function ismethod (value:Dynamic):Bool;
	static function isclass (value:Dynamic):Bool;

	static function isfunction(value:Dynamic):Bool;


}