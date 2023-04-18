#if sys

// bind & listen
var s = new sys.net.Socket();
var host = new sys.net.Host("127.0.0.1");
s.bind(host, 0);
s.listen(1);
var port = s.host().port;
port > 0;

// connect
var c = new sys.net.Socket();
c.connect(host, port);
c.input != null;
c.output != null;

#if !java
// select when accept() would succeed
var select = sys.net.Socket.select([s], [s], [s], 0.01);
select.read.length == 1;
select.write.length == 0;
select.others.length == 0;

// multiple selects without reading
var select = sys.net.Socket.select([s], [s], [s], 0.01);
select.read.length == 1;
select.write.length == 0;
select.others.length == 0;

// accept
var w = s.accept();
w != null;
w.input != null;
w.output != null;
w.setFastSend(true);
s.setBlocking(false);

// select after accept
var select = sys.net.Socket.select([s], [s], [s], 0.01);
select.read.length == 0;
select.write.length == 0;
select.others.length == 0;

// write
w.output.writeByte(97);
w.output.writeByte(98);
w.output.writeByte(99);
w.close();

// read
c.waitForRead();
var select = sys.net.Socket.select([c], [c], [c]);
select.read.length == 1;
select.write.length == 1;
select.others.length == 0;
c.read() == "abc";
#end

c.close();
s.close();

#else
1 == 1;
#end