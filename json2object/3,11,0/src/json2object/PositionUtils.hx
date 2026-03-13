/*
Copyright (c) 2016-2019 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package json2object;

/**
 * Transform a hxjsonast.Position into a Position, which provides information on lines.
 */
class PositionUtils {
	/** Store line information (number/start/end). */
	var linesInfo = new Array<Position.Line>();

	public function new(content:String) {
		var s = 0; // Line start char
		var e = 0; // Line end char

		var i = 0;
		var lineCount = 0;
		while (i < content.length) {
			switch (content.charAt(i)) {
				case "\r":
					e = i;
					if (content.charAt(i+1) == "\n") {
						e++;
					}
					linesInfo.push({ number: lineCount, start: s, end: e });
					lineCount++;
					i = e+1;
					s = i;
				case "\n":
					e = i;
					linesInfo.push({ number: lineCount, start: s, end: e });
					lineCount++;
					i++;
					s = i;
				default:
					i++;
			}
		}
		linesInfo.push({number: lineCount, start: s, end: i });
	}

	/**
	 * Convert a hxjsonast.Position into a Position who contains information on lines.
	 */
	public function convertPosition(position:hxjsonast.Position):Position {
		var file = position.file;
		var min = position.min;
		var max = position.max;

		var pos = {file: file, min: min+1, max: max+1, lines:[]};
		var lastLine = linesInfo.length - 1;

		var bounds = {min:0, max:lastLine};
		if (min > linesInfo[0].end) {
			while (bounds.max > bounds.min) {
				var i = Std.int((bounds.min + bounds.max) / 2);
				var line = linesInfo[i];
				if (line.start == min) {
					bounds.min = i;
					bounds.max = i;
				}
				if (line.end < min) {
					bounds.min = i+1;
				}
				if (line.start > min || (line.end >= min && line.start < min)) {
					bounds.max = i;
				}
			}

		}

		// Usually first line/char are refered as 1 instead of 0
		for (i in bounds.min...linesInfo.length) {
			var line = linesInfo[i];
			if (line.start <= min && line.end >= max) {
				pos.lines.push({ number: line.number +1, start: min-line.start +1, end: max-line.start +1 });
				break;
			}
			if (line.start <= min && min <= line.end) {
				pos.lines.push({ number: line.number +1, start: min-line.start +1, end: line.end +1 });
			}
			if (line.start <= max && max <= line.end) {
				pos.lines.push({ number: line.number +1, start: line.start +1, end: max-line.start +1 });
			}
			if (line.start >= max || line.end >= max) {
				break;
			}
		}

		return pos;
	}

	public inline function revert(position:Position) : hxjsonast.Position {
		return {file:position.file, min:position.min - 1, max:position.max - 1};
	}
}
