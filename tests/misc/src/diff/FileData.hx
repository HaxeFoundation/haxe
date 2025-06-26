package diff;

import haxe.io.Bytes;
import haxe.ds.Vector;

@:structInit
private class Line {
	public final pos:Int;
	public final length:Int;
}

class FileData {
	public final data:Bytes;
	public final label:String;
	public final mtime:Date;
	public var prefixEnd:Int;
	public var prefixNewlines:Int;
	public var suffixBegin:Int;
	public var equivs:Vector<Int>;
	public final equivCount:Array<Null<Int>>;
	public final lines:Array<Line>;

	public var changed:IndexVector;

	public function new(data:Bytes, label:String, mtime:Date) {
		this.label = label;
		this.mtime = mtime;
		this.data = data;
		prefixEnd = 0;
		suffixBegin = data.length;
		equivCount = [];
		lines = [];
	}

	#if sys
	static public function loadFile(path:String) {
		final stat = sys.FileSystem.stat(path);
		return new FileData(sys.io.File.getBytes(path), path, stat.mtime);
	}
	#end

	public function addLine(pos:Int, length:Int) {
		lines.push({pos: pos, length: length});
	}

	public function markChange(line:Int) {
		changed[line] = 1;
	}

	public function finishLineProcessing(equivs:Array<Int>) {
		this.equivs = Vector.fromArrayCopy(equivs);
	}

	public function increaseEquivCount(index:Int) {
		if (equivCount[index] == null) {
			equivCount[index] = 1;
		} else {
			equivCount[index]++;
		}
	}

	public function dump() {
		var buf = new StringBuf();
		inline function add(s:String) {
			buf.add(s);
		}
		add('  prefixEnd: ${prefixEnd}\n');
		add('suffixBegin: ${suffixBegin}\n');
		add('     equivs: ${equivs}\n');
		add(' equivCount: ${equivCount}\n');
		return buf.toString();
	}
}
