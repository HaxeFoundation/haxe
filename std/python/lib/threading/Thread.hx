
package python.lib.threading;

extern class Thread {

	static function __init__ ():Void
	{
		python.Syntax.importFromAs("threading","Thread", "python.lib.thrading.Thread");
	}

}