
package python.lib.io;

import python.lib.io.TextIOBase;
import python.Syntax;

@:pythonImport("io", "StringIO")
extern class StringIO extends TextIOBase
{
	public function new (?s:String):Void;
	public function getvalue():String;
}