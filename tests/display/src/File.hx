import haxe.display.Position;

class File {
	public var content(default, null):String;
	public var path(default, null):String;

	var lines:Array<Int>;

	public function new(path:String, content:String) {
		this.path = path;
		this.content = content;
		initLines();
	}

	function initLines() {
		lines = [];
		// составляем массив позиций начала строк
		var s = 0, p = 0;
		while (p < content.length) {
			inline function nextChar()
				return StringTools.fastCodeAt(content, p++);
			inline function line() {
				lines.push(s);
				s = p;
			};
			switch (nextChar()) {
				case "\n".code:
					line();
				case "\r".code:
					p++;
					line();
			}
		}
	}

	function findPosition(pos:Int):Position {
		function loop(min, max) {
			var mid = (min + max) >> 1;
			var start = lines[mid];
			return if (mid == min)
				{line: mid, character: pos - start + 1};
			else if (start > pos)
				loop(min, mid);
			else
				loop(mid, max);
		}
		return loop(0, lines.length);
	}

	public function findRange(min:Int, max:Int):Range {
		return {
			start: findPosition(min),
			end: findPosition(max)
		}
	}

	public function formatRange(min:Int, max:Int):String {
		var range = findRange(min, max);
		var start = range.start;
		var end = range.end;
		var pos = if (start.line == end.line) {
			if (start.character == end.character)
				'character ${start.character}';
			else
				'characters ${start.character}-${end.character}';
		} else {
			'lines ${start.line + 1}-${end.line + 1}';
		}
		return '$path:${start.line + 1}: $pos';
	}

	public static inline function read(path:String) {
		return new File(path, sys.io.File.getContent(path));
	}
}
