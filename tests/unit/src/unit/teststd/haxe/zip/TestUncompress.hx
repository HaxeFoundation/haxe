package unit.teststd.haxe.zip;

class TestUncompress extends unit.Test {
	public function test() {
		// not supported in js/python yet
		#if (cpp || php || jvm || neko || flash)
		var d = [120, 218, 43, 73, 45, 46, 1, 0, 4, 93, 1, 193];
		var b = haxe.io.Bytes.alloc(d.length);
		for (i in 0...d.length) b.set(i, d[i]);
		var c = haxe.zip.Uncompress.run(b);

		eq(c.toString(), "test");

		var d = [120, 218, 3, 0, 0, 0, 0, 1];
		var b = haxe.io.Bytes.alloc(d.length);
		for (i in 0...d.length) b.set(i, d[i]);
		var c = haxe.zip.Uncompress.run(b);

		eq(c.length, 0);
		#end

		#if php
		var d = [120, 218, 43, 73, 45, 46, 1, 0, 4, 93, 1, 193];
		var b = haxe.io.Bytes.alloc(d.length);
		for (i in 0...d.length) b.set(i, d[i]);
		var c = haxe.io.Bytes.alloc(4);
		var d = new haxe.zip.Uncompress();
		var r = d.execute(b, 0, c, 0);

		t(r.done);
		eq(r.read, 12);
		eq(r.write, 4);
		eq(c.toString(), "test");
		#else
		eq(1, 1);
		#end

	}
}
