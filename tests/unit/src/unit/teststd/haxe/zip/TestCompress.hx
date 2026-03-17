package unit.teststd.haxe.zip;

class TestCompress extends unit.Test {
	public function test() {
		// not supported in js/python yet
		#if (cpp || php || jvm || neko || flash || hl)
		var b = haxe.io.Bytes.ofString("test");
		var c = haxe.zip.Compress.run(b, 9);

		eq(c.length, 12);
		eq(c.get(0), 120);
		eq(c.get(1), 218);
		eq(c.get(2), 43);
		eq(c.get(3), 73);
		eq(c.get(4), 45);
		eq(c.get(5), 46);
		eq(c.get(6), 1);
		eq(c.get(7), 0);
		eq(c.get(8), 4);
		eq(c.get(9), 93);
		eq(c.get(10), 1);
		eq(c.get(11), 193);

		var b = haxe.io.Bytes.alloc(0);
		var c = haxe.zip.Compress.run(b, 9);

		eq(c.length, 8);
		eq(c.get(0), 120);
		eq(c.get(1), 218);
		eq(c.get(2), 3);
		eq(c.get(3), 0);
		eq(c.get(4), 0);
		eq(c.get(5), 0);
		eq(c.get(6), 0);
		eq(c.get(7), 1);
		#end

		#if php
		var b = haxe.io.Bytes.alloc(0);
		var c = haxe.io.Bytes.alloc(8);
		var d = new haxe.zip.Compress(9);
		var r = d.execute(b, 0, c, 0);

		t(r.done);
		eq(r.read, 0);
		eq(r.write, 8);
		eq(c.length, 8);
		eq(c.get(0), 120);
		eq(c.get(1), 218);
		eq(c.get(2), 3);
		eq(c.get(3), 0);
		eq(c.get(4), 0);
		eq(c.get(5), 0);
		eq(c.get(6), 0);
		eq(c.get(7), 1);
		#else
		eq(1, 1);
		#end

	}
}
