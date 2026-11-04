package diff;

import haxe.io.Bytes;
import haxe.ds.IntMap;
import haxe.ds.Vector;

using diff.Io.BytesTools;

private typedef Hash = Int;

class BytesTools {
	static public function compareSub(bytes1:Bytes, pos1:Int, bytes2:Bytes, pos2:Int, length:Int) {
		for (i in 0...length) {
			if (bytes1.get(pos1 + i) != bytes2.get(pos2 + i)) {
				return false;
			}
		}
		return true;
	}
}

@:structInit
private class Equivclass {
	public final hash:Hash;
	public final bytes:Bytes; // This is the complete bytes, not the sub-range
	public final pos:Int;
	public final length:Int;
}

private class IoContext {
	final equivLut:IntMap<Array<Int>>;
	final equivs:Array<Equivclass>;

	public function new() {
		equivLut = new IntMap();
		equivs = [];
	}

	public function addEquiv(equiv:Equivclass) {
		return equivs.push(equiv) - 1;
	}

	public function getEquiv(i:Int) {
		return equivs[i];
	}

	public function lookup(hash:Hash) {
		var a = equivLut.get(hash);
		if (a == null) {
			a = [];
			equivLut.set(hash, a);
		}
		return a;
	}
}

class Io {
	static inline function ROL(v, n)
		return ((v) << (n) | (v) >> (32 - (n)));

	static inline function HASH(h, c)
		return ((c) + ROL(h, 7));

	static public function findIdenticalEnds(ctx:Context) {
		var w1 = 0;
		final file1 = ctx.file1;
		final file2 = ctx.file2;
		final len1 = file1.data.length;
		final len2 = file2.data.length;
		final min = len1 < len2 ? len1 : len2;
		var newlineCount = 0;
		final ringMod = ctx.context + 1;
		final newlineRing = new Vector(ringMod);
		var c;
		while (w1 < min && (c = file1.data.get(w1)) == file2.data.get(w1)) {
			if (c == '\n'.code) {
				newlineRing[newlineCount++ % ringMod] = w1;
			}
			++w1;
		}
		if (newlineCount < ringMod) {
			file1.prefixEnd = 0;
			file2.prefixEnd = 0;
			newlineCount = 0;
		} else {
			file1.prefixEnd = newlineRing[newlineCount % ringMod] + 1;
			file2.prefixEnd = file1.prefixEnd;
			newlineCount -= ringMod - 1;
		}
		file1.prefixNewlines = file2.prefixNewlines = newlineCount;

		w1 = len1 - 1;
		var w2 = len2 - 1;
		newlineCount = 0;
		while (w1 > file1.prefixEnd && w2 > file2.prefixEnd && (c = file1.data.get(w1)) == file2.data.get(w2)) {
			if (c == '\n'.code) {
				newlineRing[newlineCount++ % ringMod] = w1;
			}
			w1--;
			w2--;
		}
		if (newlineCount < ringMod) {
			file1.suffixBegin = len1 - 1;
			file2.suffixBegin = len2 - 1;
		} else {
			file1.suffixBegin = newlineRing[newlineCount % ringMod] + 1;
			file2.suffixBegin = file1.suffixBegin + (len2 - len1);
		}
	}

	static public function findAndHashEachLine(ctx:IoContext, file:FileData) {
		var p = file.prefixEnd;
		var c = 0;
		final cureqs = [];
		while (p < file.suffixBegin) {
			final ip = p;
			var h = 0;
			// TODO: This has a big switch in io.c that depends on the whitespace policy
			while ((c = file.data.get(p++)) != '\n'.code) {
				h = HASH(h, c);
				if (p == file.data.length) {
					p++;
					break;
				}
			}
			final length = p - ip - 1;
			final a = ctx.lookup(h);
			var equivIndex = -1;
			for (i in a) {
				final equiv = ctx.getEquiv(i);
				if (equiv.hash == h && equiv.length == length && file.data.compareSub(ip, equiv.bytes, equiv.pos, length)) {
					equivIndex = i;
					break;
				}
			}
			if (equivIndex < 0) {
				final equiv:Equivclass = {
					hash: h,
					bytes: file.data,
					pos: ip,
					length: length
				}
				equivIndex = ctx.addEquiv(equiv);
				a.push(equivIndex);
			}
			cureqs.push(equivIndex);
			file.increaseEquivCount(equivIndex);
			file.addLine(ip, length);
		}
		file.finishLineProcessing(cureqs);
	}

	static public function readFiles(ctx:Context) {
		findIdenticalEnds(ctx);
		var ioctx = new IoContext();
		findAndHashEachLine(ioctx, ctx.file1);
		findAndHashEachLine(ioctx, ctx.file2);
	}
}
