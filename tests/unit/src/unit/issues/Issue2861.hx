package unit.issues;

class Issue2861 extends Test
{
#if (flash || neko || cpp || java)
	public function test()
	{
		var b = haxe.io.Bytes.alloc(2048);
		for (i in 0...2048)
			b.set(i,Std.random(255));
		// compress
		var comp = haxe.zip.Compress.run(b,9);
		var unc = haxe.zip.Uncompress.run(comp);
		eq(b.compare(unc), 0);
		eq(b.length,unc.length);

		if (b.compare(unc) != 0) // debug
			trace(b.toHex());

		// now check if it's compatible with the pure haxe implementation
		var unc2 = haxe.zip.InflateImpl.run(new haxe.io.BytesInput(comp));
		eq(b.compare(unc2), 0);

		if (b.compare(unc2) != 0) // debug
			trace(b.toHex());
		eq(unc.compare(unc2), 0);
	}
#end
}
