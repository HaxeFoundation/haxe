package jsonrpc.node;

import js.node.Buffer;
import js.node.stream.Writable.IWritable;
import jsonrpc.Types;

class MessageWriter {
	static inline var CONTENT_LENGTH = "Content-Length: ";
	static inline var CRLF = '\r\n';

	var writable:IWritable;
	var encoding:String;

	public function new(writable:IWritable, encoding = "utf8") {
		this.writable = writable;
		this.encoding = encoding;
	}

	public function write(msg:Message, _):Void {
		var json = haxe.Json.stringify(msg);
		var contentLength = Buffer.byteLength(json, encoding);
		writable.write(CONTENT_LENGTH, "ascii");
		writable.write("" + contentLength, "ascii");
		writable.write(CRLF);
		writable.write(CRLF);
		writable.write(json, encoding);
	}
}
