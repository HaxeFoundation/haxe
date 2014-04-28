
package python.lib.io;

import python.lib.io.TextIOBase;
import python.Syntax;

extern class StringIO extends TextIOBase
{
	public function new (?s:String):Void;
	public function getvalue():String;



	static function __init__ ():Void {
		Syntax.importFromAs("io", "StringIO", "python.lib.io.StringIO");
	}
}