class Main
{
	public static function main()
	{
		var asm = cs.system.reflection.Assembly.LoadFile(sys.FileSystem.fullPath("bin/lib1/bin/lib1.dll"));
		var name = #if no_root "haxe.root.Lib1" #else "Lib1" #end;
		var tp:Dynamic = asm.GetType(name);
		var obj = tp.test();
		trace(obj);
		for (field in Reflect.fields(obj))
		{
			var val:Dynamic = Reflect.field(obj,field);
			if (val != true)
				throw 'Value $val for field $field';
		}
		var names = ["longInexistentName","otherName","yetAnotherName","fdskljdflskjf","xxy"];
		var n2 = Reflect.fields(obj);
		names.sort(Reflect.compare);
		n2.sort(Reflect.compare);
		if (names.toString() != n2.toString()) throw 'Mismatch: $names and $n2';
	}
}
