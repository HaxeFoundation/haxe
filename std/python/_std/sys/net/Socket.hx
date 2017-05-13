/*
* Copyright (C)2005-2017 Haxe Foundation
*
* Permission is hereby granted, free of charge, to any person obtaining a
* copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation
* the rights to use, copy, modify, merge, publish, distribute, sublicense,
* and/or sell copies of the Software, and to permit persons to whom the
* Software is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
* DEALINGS IN THE SOFTWARE.
*/
package sys.net;

import haxe.io.Error;
import haxe.io.Bytes;
import haxe.io.BytesData;
import python.Exceptions;
import python.Tuple;
import python.lib.net.Socket in PSocket;
import python.lib.net.Socket.SocketModule in PSocketModule;
import python.lib.net.Address in PAddress;
import python.lib.Select;

private class SocketInput extends haxe.io.Input {

    var __s : PSocket;

    public function new(s) {
        __s = s;
    }

    public override function readByte() : Int {
        var r:BytesData;
        try {
            r = __s.recv(1,0);
        } catch( e : BlockingIOError ) {
                throw Blocked;
        }
        if( r.length == 0 )
            throw new haxe.io.Eof();
        return python.Syntax.pythonCode("r[0]");
    }

    public override function readBytes( buf : haxe.io.Bytes, pos : Int, len : Int ) : Int {
        var r;
        var data = buf.getData();
        try {
            r = __s.recv(len,0);
            for (i in pos...(pos+r.length)){
                data.set(i,r[i-pos]);
            }
        } catch( e : BlockingIOError ) {
                throw Blocked;
        }
        if( r.length == 0 )
            throw new haxe.io.Eof();
        return r.length;
    }

    public override function close() {
        super.close();
        if( __s != null ) __s.close();
    }

}

private class SocketOutput extends haxe.io.Output {

    var __s : PSocket;

    public function new(s) {
        __s = s;
    }

    public override function writeByte( c : Int ) {
        try {
            __s.send(python.Syntax.pythonCode('bytes([c])'),0);
        } catch( e : BlockingIOError ) {
                throw Blocked;
        }
    }

    public override function writeBytes( buf : haxe.io.Bytes, pos : Int, len : Int) : Int {
        try {
            var data    = buf.getData();
            var payload = python.Syntax.pythonCode("data[{0}:{0}+{1}]", pos, len);
            var r = __s.send(payload,0);
            return r;
        } catch( e : BlockingIOError ) {
            throw Blocked;
        }
    }

    public override function close() {
        super.close();
        if( __s != null ) __s.close();
    }
}

/**
    A TCP socket class : allow you to both connect to a given server and exchange messages or start your own server and wait for connections.
**/
@:coreApi class Socket {


    var __s:PSocket;
    /**
        The stream on which you can read available data. By default the stream is blocking until the requested data is available,
        use `setBlocking(false)` or `setTimeout` to prevent infinite waiting.
    **/
    public var input(default,null) : haxe.io.Input;

    /**
        The stream on which you can send data. Please note that in case the output buffer you will block while writing the data, use [setBlocking(false)] or [setTimeout] to prevent that.
    **/
    public var output(default,null) : haxe.io.Output;

    /**
        A custom value that can be associated with the socket. Can be used to retrieve your custom infos after a `select`.
    ***/
    public var custom : Dynamic;

    /**
        Creates a new unconnected socket.
    **/
    public function new() : Void {
    }

    function __init() : Void  {
        __s = new PSocket();
        input = new SocketInput(__s);
        output = new SocketOutput(__s);
    }

    /**
        Closes the socket : make sure to properly close all your sockets or you will crash when you run out of file descriptors.
    **/
    public function close() : Void {
        __s.close();
    }

    /**
        Read the whole data available on the socket.
    **/
    public function read() : String {
        return input.readAll().toString();
    }

    /**
        Write the whole data to the socket output.
    **/
    public function write( content : String ) : Void {
        output.writeString(content);
    }

    /**
        Connect to the given server host/port. Throw an exception in case we couldn't successfully connect.
    **/
    public function connect( host : Host, port : Int ) : Void {
        __init();
        var host_str = host.toString();
        __s.connect(python.Syntax.pythonCode("(host_str,port)"));
    }

    /**
        Allow the socket to listen for incoming questions. The parameter tells how many pending connections we can have until they get refused. Use `accept()` to accept incoming connections.
    **/
    public function listen( connections : Int ) : Void {
        __s.listen(connections);
    }

    /**
        Shutdown the socket, either for reading or writing.
    **/
    public function shutdown( read : Bool, write : Bool ) : Void
        __s.shutdown( (read && write) ? PSocketModule.SHUT_RDWR : read ?  PSocketModule.SHUT_RD : PSocketModule.SHUT_WR  );

    /**
        Bind the socket to the given host/port so it can afterwards listen for connections there.
    **/
    public function bind( host : Host, port : Int ) : Void {
        __init();
        var host_str = host.toString();
        __s.bind(python.Syntax.pythonCode("(host_str,port)"));
    }

    /**
        Accept a new connected client. This will return a connected socket on which you can read/write some data.
    **/
    public function accept() : Socket {
        var tp2:Tuple2<PSocket,PAddress> = __s.accept();
        var s = new Socket();
        s.__s = tp2._1;
        s.input = new SocketInput(s.__s);
        s.output = new SocketOutput(s.__s);
        return s;
    }

    /**
        Return the information about the other side of a connected socket.
    **/
    public function peer() : { host : Host, port : Int } {
        var pn = __s.getpeername();
        return { host:new Host( pn._1 ), port:pn._2}
    }

    /**
        Return the information about our side of a connected socket.
    **/
    public function host() : { host : Host, port : Int } {
        var pn = __s.getsockname();
        return { host:new Host( pn._1 ), port:pn._2};
    }

    /**
        Gives a timeout after which blocking socket operations (such as reading and writing) will abort and throw an exception.
    **/
    public function setTimeout( timeout : Float ) : Void {
        __s.settimeout(timeout);
    }

    /**
        Block until some data is available for read on the socket.
    **/
    public function waitForRead() : Void {

    }

    /**
        Change the blocking mode of the socket. A blocking socket is the default behavior. A non-blocking socket will abort blocking operations immediately by throwing a haxe.io.Error.Blocking value.
    **/
    public function setBlocking( b : Bool ) : Void {
        __s.setblocking(b);
    }

    /**
        Allows the socket to immediately send the data when written to its output : this will cause less ping but might increase the number of packets / data size, especially when doing a lot of small writes.
    **/
    public function setFastSend( b : Bool ) : Void {}

    @:keep function fileno():Int return __s.fileno();

    /**
        Wait until one of the sockets groups is ready for the given operation :
        - `read` contains sockets on which we want to wait for available data to be read,
        - `write` contains sockets on which we want to wait until we are allowed to write some data to their output buffers,
        - `others` contains sockets on which we want to wait for exceptional conditions.
        - `select` will block until one of the condition is met, in which case it will return the sockets for which the condition was true.
        In case a `timeout` (in seconds) is specified, select might wait at worse until the timeout expires.
    **/
    public static function select(read : Array<Socket>, write : Array<Socket>, others : Array<Socket>, ?timeout : Float) : { read: Array<Socket>,write: Array<Socket>,others: Array<Socket> } {
        var t3 = Select.select(read,write,others,timeout);
        return {read:t3._1,write:t3._2,others:t3._3};
    }

}
