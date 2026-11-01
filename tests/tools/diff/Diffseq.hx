package diff;

import haxe.ds.Vector;

@:structInit
private class Parition {
	public var xmid:Int;
	public var ymid:Int;

	function toString() {
		return '[Partition xmid: $xmid, ymid: $ymid]';
	}
}

final OFFSET_MAX = 1 << 30;

@:structInit
class DiffseqContext {
	public final xvec:Vector<Int>;
	public final yvec:Vector<Int>;
	public final fdiag:IndexVector;
	public final bdiag:IndexVector;
	public final NOTE_DELETE:(Int) -> Void;
	public final NOTE_INSERT:(Int) -> Void;
}

class Diffseq {
	static public function diag(xoff:Int, xlim:Int, yoff:Int, ylim:Int, ctxt:DiffseqContext):Parition {
		var fd = ctxt.fdiag;
		var bd = ctxt.bdiag;
		var xv = ctxt.xvec;
		var yv = ctxt.yvec;
		inline function XREF_YREF_EQUAL(x, y) {
			return xv[x] == yv[y];
		}
		final dmin = xoff - ylim;
		final dmax = xlim - yoff;
		final fmid = xoff - yoff;
		final bmid = xlim - ylim;
		var fmin = fmid;
		var fmax = fmid;
		var bmin = bmid;
		var bmax = bmid;
		var odd = ((fmid - bmid) & 1) != 0;
		fd[fmid] = xoff;
		bd[bmid] = xlim;
		while (true) {
			var d;
			if (fmin > dmin) {
				fd[--fmin - 1] = -1;
			} else {
				++fmin;
			}
			if (fmax < dmax) {
				fd[++fmax + 1] = -1;
			} else {
				--fmax;
			}
			d = fmax;
			while (d >= fmin) {
				var tlo = fd[d - 1];
				var thi = fd[d + 1];
				var x0 = tlo < thi ? thi : tlo + 1;
				var x = x0;
				var y = x0 - d;
				while (x < xlim && y < ylim && XREF_YREF_EQUAL(x, y)) {
					x++;
					y++;
				}
				fd[d] = x;
				if (odd && bmin <= d && d <= bmax && bd[d] <= x) {
					return {
						xmid: x,
						ymid: y
					}
				}
				d -= 2;
			}

			if (bmin > dmin) {
				bd[--bmin - 1] = OFFSET_MAX;
			} else {
				++bmin;
			}
			if (bmax < dmax) {
				bd[++bmax + 1] = OFFSET_MAX;
			} else {
				--bmax;
			}
			d = bmax;
			while (d >= bmin) {
				var tlo = bd[d - 1];
				var thi = bd[d + 1];
				var x0 = tlo < thi ? tlo : thi - 1;
				var x = x0;
				var y = x0 - d;
				while (xoff < x && yoff < y && XREF_YREF_EQUAL(x - 1, y - 1)) {
					x--;
					y--;
				}
				bd[d] = x;
				if (!odd && fmin <= d && d <= fmax && x <= fd[d]) {
					return {
						xmid: x,
						ymid: y
					}
				}
				d -= 2;
			}
		}
	}

	static public function compareseq(xoff:Int, xlim:Int, yoff:Int, ylim:Int, ctxt:DiffseqContext) {
		final xv = ctxt.xvec;
		final yv = ctxt.yvec;
		inline function XREF_YREF_EQUAL(x, y) {
			return xv[x] == yv[y];
		}
		while (true) {
			while (xoff < xlim && yoff < ylim && XREF_YREF_EQUAL(xoff, yoff)) {
				xoff++;
				yoff++;
			}

			while (xoff < xlim && yoff < ylim && XREF_YREF_EQUAL(xlim - 1, ylim - 1)) {
				xlim--;
				ylim--;
			}

			if (xoff == xlim) {
				while (yoff < ylim) {
					ctxt.NOTE_INSERT(yoff);
					yoff++;
				}
				break;
			}
			if (yoff == ylim) {
				while (xoff < xlim) {
					ctxt.NOTE_DELETE(xoff);
					xoff++;
				}
				break;
			}

			var part = diag(xoff, xlim, yoff, ylim, ctxt);

			var xoff1, xlim1, yoff1, ylim1, xoff2, xlim2, yoff2, ylim2;
			if ((xlim + ylim) - (part.xmid + part.ymid) < (part.xmid + part.ymid) - (xoff + yoff)) {
				xoff1 = part.xmid;
				xlim1 = xlim;
				yoff1 = part.ymid;
				ylim1 = ylim;
				xoff2 = xoff;
				xlim2 = part.xmid;
				yoff2 = yoff;
				ylim2 = part.ymid;
			} else {
				xoff1 = xoff;
				xlim1 = part.xmid;
				yoff1 = yoff;
				ylim1 = part.ymid;
				xoff2 = part.xmid;
				xlim2 = xlim;
				yoff2 = part.ymid;
				ylim2 = ylim;
			}
			compareseq(xoff1, xlim1, yoff1, ylim1, ctxt);

			xoff = xoff2;
			xlim = xlim2;
			yoff = yoff2;
			ylim = ylim2;
		}
		return false;
	}
}
