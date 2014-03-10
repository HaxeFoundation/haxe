
package python.lib.io;

import python.lib.io.TextIOBase;
import python.Macros;

extern class StringIO extends TextIOBase 
{
	public function new (?s:String):Void;
	public function getvalue():String;



	static function __init__ ():Void {
		Macros.importFromAs("io", "StringIO", "python.lib.io.StringIO");
	}
}