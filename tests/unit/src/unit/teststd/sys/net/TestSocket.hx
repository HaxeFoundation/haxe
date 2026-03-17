package unit.teststd.sys.net;

class TestSocket extends unit.Test {
	public function test() {
		#if sys
		// socket api is not complete on hxnodejs

		// bind & listen
		var s = new sys.net.Socket();
		var host = new sys.net.Host("127.0.0.1");
		s.bind(host, 0);
		s.listen(1);
		var port = s.host().port;
		t(port > 0);

		// connect
		var c = new sys.net.Socket();
		c.connect(host, port);
		t(c.input != null);
		t(c.output != null);

		#if !jvm
		// select when accept() would succeed
		var select = sys.net.Socket.select([s], [s], [s], 0.01);
		eq(select.read.length, 1);
		eq(select.write.length, 0);
		eq(select.others.length, 0);

		// multiple selects without reading
		var select = sys.net.Socket.select([s], [s], [s], 0.01);
		eq(select.read.length, 1);
		eq(select.write.length, 0);
		eq(select.others.length, 0);

		// accept
		var w = s.accept();
		t(w != null);
		t(w.input != null);
		t(w.output != null);
		w.setFastSend(true);
		s.setBlocking(false);

		// select after accept
		var select = sys.net.Socket.select([s], [s], [s], 0.01);
		eq(select.read.length, 0);
		eq(select.write.length, 0);
		eq(select.others.length, 0);

		// write
		w.output.writeByte(97);
		w.output.writeByte(98);
		w.output.writeByte(99);
		w.close();

		// read
		c.waitForRead();
		var select = sys.net.Socket.select([c], [c], [c]);
		eq(select.read.length, 1);
		eq(select.write.length, 1);
		eq(select.others.length, 0);
		eq(c.read(), "abc");
		#end

		c.close();
		s.close();

		#else
		eq(1, 1);
		#end

	}
}
