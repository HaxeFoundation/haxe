package diff;

import haxe.ds.Vector;
import diff.Diffseq;

class Analyze {
	static function discardConfusingLines(file1:FileData, file2:FileData) {}

	static function analyzeHunk(ctx:Context, script:Array<Change>) {
		var showFrom = 0;
		var showTo = 0;

		final first0 = script[0].line0;
		final first1 = script[0].line1;

		var l0 = 0;
		var l1 = 0;
		for (next in script) {
			l0 = next.line0 + next.deleted - 1;
			l1 = next.line1 + next.inserted - 1;
			showFrom += next.deleted;
			showTo += next.inserted;
		}

		return {
			first0: first0,
			first1: first1,
			last0: l0,
			last1: l1,
			showFrom: showFrom > 0,
			showTo: showTo > 0
		}
	}

	static function buildScript(ctx:Context) {
		var currentChange = null;
		var firstChange = null;
		function addChange(line0:Int, line1:Int, deleted:Int, inserted:Int) {
			var change:Change = {
				line0: line0,
				line1: line1,
				deleted: deleted,
				inserted: inserted
			};
			if (currentChange == null) {
				currentChange = change;
				firstChange = change;
			} else {
				currentChange.next = change;
				currentChange = change;
			}
		};
		final changed0 = ctx.file1.changed;
		final changed1 = ctx.file2.changed;
		final len0 = ctx.file1.lines.length;
		final len1 = ctx.file2.lines.length;
		var i0 = 0;
		var i1 = 0;
		while (i0 < len0 || i1 < len1) {
			if (changed0[i0] | changed1[i1] != 0) {
				final line0 = i0;
				final line1 = i1;
				while (changed0[i0] != 0) {
					++i0;
				}
				while (changed1[i1] != 0) {
					++i1;
				}
				addChange(line0, line1, i0 - line0, i1 - line1);
			}
			i0++;
			i1++;
		}
		return firstChange;
	}

	static function printNormalHunk(ctx:Context, script:Array<Change>) {
		// final changes = analyzeHunk(ctx, script);
		var buf = new StringBuf();
		for (change in script) {
			if (change.deleted > 0) {
				for (i in change.line0...change.line0 + change.deleted) {
					final line = ctx.file1.lines[i];
					buf.add("< ");
					buf.add(ctx.file1.data.sub(line.pos, line.length));
					buf.addChar("\n".code);
				}
			}
			buf.add("---\n");
			if (change.inserted > 0) {
				for (i in change.line1...change.line1 + change.inserted) {
					final line = ctx.file2.lines[i];
					buf.add("> ");
					buf.add(ctx.file2.data.sub(line.pos, line.length).toString());
					buf.addChar("\n".code);
				}
			}
		}
		return buf.toString();
	}

	static public function diff2Files(ctx:Context) {
		Io.readFiles(ctx);
		final flagSpace = new Vector(ctx.file1.lines.length + ctx.file2.lines.length + 4);
		for (i in 0...flagSpace.length) {
			flagSpace[i] = 0;
		}
		ctx.file1.changed = new IndexVector(flagSpace, 1);
		ctx.file2.changed = new IndexVector(flagSpace, ctx.file1.lines.length + 3);
		// TODO: discardConfusingLines
		final diags = ctx.file1.equivs.length + ctx.file2.equivs.length + 3;
		var fdiag = new IndexVector(new Vector(diags * 2), 0);
		var bdiag = fdiag + diags;
		fdiag += ctx.file2.equivs.length + 1;
		bdiag += ctx.file2.equivs.length + 1;
		var dCtx:DiffseqContext = {
			xvec: ctx.file1.equivs,
			yvec: ctx.file2.equivs,
			fdiag: fdiag,
			bdiag: bdiag,
			NOTE_INSERT: (d) -> {
				ctx.file2.markChange(d);
			},
			NOTE_DELETE: (d) -> {
				ctx.file1.markChange(d);
			}
		}
		Diffseq.compareseq(0, ctx.file1.equivs.length, 0, ctx.file2.equivs.length, dCtx);
		// TODO: shiftBoundaries
		return buildScript(ctx);
	}
}
