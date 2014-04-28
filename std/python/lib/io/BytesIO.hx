
package python.lib.io;

extern class BytesIO extends python.lib.io.BufferedIOBase {

	public function new (base:python.lib.io.IOBase);

	static function __init__ ():Void
	{
		python.Syntax.importFromAs("io", "BytesIO", "python.lib.io.BytesIO");
	}

}