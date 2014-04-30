
package python.lib.io;

import python.lib.io.TextIOBase;
import python.Syntax;

@:import("io", "StringIO")
extern class StringIO extends TextIOBase
{
	public function new (?s:String):Void;
	public function getvalue():String;
}