/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package haxe.display;

using StringTools;

abstract FsPath(String) {
	static final upperCaseDriveRe = ~/^(\/)?([A-Z]:)/;

	public inline function new(path:String) {
		this = path;
	}

	/** ported from VSCode sources **/
	public function toUri():DocumentUri {
		var path = this;
		path = path.replace("\\", "/");
		if (path.fastCodeAt(0) != "/".code)
			path = "/" + path;

		var parts = ["file://"];

		if (upperCaseDriveRe.match(path))
			path = upperCaseDriveRe.matched(1) + upperCaseDriveRe.matched(2).toLowerCase() + upperCaseDriveRe.matchedRight();

		var lastIdx = 0;
		while (true) {
			var idx = path.indexOf("/", lastIdx);
			if (idx == -1) {
				parts.push(UrlEncoder.urlEncode2(path.substring(lastIdx)));
				break;
			}
			parts.push(UrlEncoder.urlEncode2(path.substring(lastIdx, idx)));
			parts.push("/");
			lastIdx = idx + 1;
		}
		return new DocumentUri(parts.join(""));
	}

	public inline function toString():String {
		return this;
	}
}

private class UrlEncoder {
	public static function urlEncode2(s:String):String {
		return ~/[!'()*]/g.map(s.urlEncode(), function(re) {
			return "%" + re.matched(0).fastCodeAt(0).hex();
		});
	}
}
