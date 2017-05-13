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
package js;

/**
	By compiling the `haxe.remoting.SocketWrapper` into a SWF, you can create 
	and use XMLSockets directly from Javascript.
**/
class XMLSocket {

	var cnx : haxe.remoting.ExternalConnection;

	public function new( flashObject : String ) {
		var ctx = new haxe.remoting.Context();
		var cnx = haxe.remoting.ExternalConnection.flashConnect("SocketWrapper",flashObject,ctx);
		var sockId = cnx.api.create.call([flashObject]);
		cnx.close();
		ctx.addObject("api",this,false);
		this.cnx = haxe.remoting.ExternalConnection.flashConnect(sockId,flashObject,ctx);
	}

	public function connect( host : String, port : Int ) {
		cnx.sock.connect.call([host,port]);
	}

	public function send( data : String ) {
		cnx.sock.send.call([data]);
	}

	public function close() {
		cnx.sock.close.call([]);
		cnx.api.destroy.call([]);
		cnx.close();
	}

	public dynamic function onData( data : String ) {
	}

	public dynamic function onClose() {
	}

	public dynamic function onConnect( b : Bool ) {
	}

}
