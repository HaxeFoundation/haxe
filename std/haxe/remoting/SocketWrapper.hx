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
package haxe.remoting;
import haxe.remoting.SocketProtocol.Socket;

/**
	@see `js.XMLSocket`
**/
class SocketWrapper {

	static var ID = 0;

	static function create( prefix : String ) : String {
		var id = prefix + "WrappedSocket"+(ID++);
		var s = new Socket();
		var ctx = new Context();
		var cnx = haxe.remoting.ExternalConnection.jsConnect(id,ctx);
		ctx.addObject("sock",s);
		var o = {};
		Reflect.setField(o,"close",cnx.close);
		ctx.addObject("api",o);
		#if flash
		var connected = false;
		s.addEventListener(flash.events.Event.CONNECT,function(_) {
			connected = true;
			cnx.api.onConnect.call([true]);
		});
		s.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR,function(_) {
			if( connected )
				cnx.api.onClose.call([]);
			else
				cnx.api.onConnect.call([false]);
		});
		s.addEventListener(flash.events.Event.CLOSE,function(_) {
			cnx.api.onClose.call([]);
		});
		s.addEventListener(flash.events.DataEvent.DATA,function(e:flash.events.DataEvent) {
			cnx.api.onData.call([e.data]);
		});
		#end
		return id;
	}

	static function init() {
		if( !flash.external.ExternalInterface.available ) return null;
		var ctx = new Context();
		var o = {};
		Reflect.setField(o,"create",create);
		ctx.addObject("api",o);
		haxe.remoting.ExternalConnection.jsConnect("SocketWrapper", ctx);
		return null;
	}

	static var _ = init();

}
