package java.vm;

@:native('haxe.java.vm.Gc') class Gc
{
	public static function run( major : Bool )
	{
		java.lang.System.gc();
	}

	public static function stats() : { heap : Int, free : Int }
	{
		var r = java.lang.Runtime.getRuntime();
		return { heap : r.totalMemory(), free : r.freeMemory() };
	}
}
