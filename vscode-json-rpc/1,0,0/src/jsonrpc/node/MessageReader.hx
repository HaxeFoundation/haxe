package jsonrpc.node;

import haxe.extern.EitherType;
import js.node.Buffer;
import js.node.stream.Readable;
import jsonrpc.Types;

class MessageReader {
	var readable:IReadable;
	var callback:Message->Void;
	var buffer:MessageBuffer;
	var nextMessageLength:Int;

	public function new(readable:IReadable, encoding = "utf-8") {
		this.readable = readable;
		buffer = new MessageBuffer(encoding);
	}

	public function listen(cb:Message->Void):Void {
		nextMessageLength = -1;
		callback = cb;
		readable.on(ReadableEvent.Data, onData);
	}

	function onData(data:EitherType<Buffer, String>):Void {
		buffer.append(data);
		while (true) {
			if (nextMessageLength == -1) {
				var headers = buffer.tryReadHeaders();
				if (headers == null)
					return;
				var contentLength = headers['Content-Length'];
				if (contentLength == null)
					throw 'Header must provide a Content-Length property.';

				var length = Std.parseInt(contentLength);
				if (length == null)
					throw 'Content-Length value must be a number.';
				nextMessageLength = length;
			}
			var msg = buffer.tryReadContent(nextMessageLength);
			if (msg == null)
				return;
			nextMessageLength = -1;
			var json = haxe.Json.parse(msg);
			callback(json);
		}
	}
}

private class MessageBuffer {
	static inline var DEFAULT_SIZE = 8192;
	static var CR = Buffer.from("\r", "ascii")[0];
	static var LF = Buffer.from("\n", "ascii")[0];
	static inline var CRLF = "\r\n";

	var encoding:String;
	var index:Int;
	var buffer:Buffer;

	public function new(encoding = "utf-8") {
		this.encoding = encoding;
		index = 0;
		buffer = Buffer.alloc(DEFAULT_SIZE);
	}

	public function append(chunk:EitherType<Buffer, String>):Void {
		var toAppend;
		if ((chunk is String)) {
			var str = (chunk : String);
			toAppend = Buffer.alloc(str.length);
			toAppend.write(str, 0, str.length, encoding);
		} else {
			toAppend = chunk;
		}
		if (buffer.length - index >= toAppend.length) {
			toAppend.copy(buffer, index, 0, toAppend.length);
		} else {
			var newSize = (Math.ceil((index + toAppend.length) / DEFAULT_SIZE) + 1) * DEFAULT_SIZE;
			if (index == 0) {
				buffer = Buffer.alloc(newSize);
				toAppend.copy(buffer, 0, 0, toAppend.length);
			} else {
				buffer = Buffer.concat([buffer.slice(0, index), toAppend], newSize);
			}
		}
		index += toAppend.length;
	}

	public function tryReadHeaders():Map<String, String> {
		var current = 0;
		while (current + 3 < index
			&& (buffer[current] != CR || buffer[current + 1] != LF || buffer[current + 2] != CR || buffer[current + 3] != LF))
			current++;
		// No header / body separator found (e.g CRLFCRLF)
		if (current + 3 >= index)
			return null;
		var result = new Map();
		var headers = buffer.toString('ascii', 0, current).split(CRLF);
		for (header in headers) {
			var index = header.indexOf(':');
			if (index == -1)
				throw "Message header must separate key and value using :";
			var key = header.substr(0, index);
			var value = StringTools.trim(header.substr(index + 1));
			result[key] = value;
		}

		var nextStart = current + 4;
		buffer = buffer.slice(nextStart);
		index = index - nextStart;
		return result;
	}

	public function tryReadContent(length:Int):String {
		if (index < length)
			return null;
		var result = buffer.toString(encoding, 0, length);
		var nextStart = length;
		buffer.copy(buffer, 0, nextStart);
		index -= nextStart;
		return result;
	}
}
