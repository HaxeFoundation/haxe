package diff;

class Printer {
	static function printContextLabel(mark:String, file:FileData) {
		var buf = new StringBuf();
		buf.add(mark);
		buf.add(" ");
		buf.add(file.label);
		buf.add("\t");
		buf.add(file.mtime.toString());
		buf.add("\n");
		return buf.toString();
	}

	static function printContextHeader(ctx:Context) {
		var buf = new StringBuf();
		buf.add(printContextLabel("---", ctx.file1));
		buf.add(printContextLabel("+++", ctx.file2));
		return buf.toString();
	}

	static function translateLineNumber(file:FileData, a:Int) {
		return file.prefixNewlines + a + 1;
	}

	static function printUnidiffNumberRange(file:FileData, a:Int, b:Int) {
		var transA = translateLineNumber(file, a - 1) + 1;
		var transB = translateLineNumber(file, b + 1) - 1;

		if (transB == transA) {
			return '$transB,0';
		} else if (transB - transA == 1) {
			return '1';
		} else {
			return '$transA,${transB - transA}';
		}
	}

	static function printUnidiffHunk(ctx:Context, change:Change) {
		var buf = new StringBuf();
		var from0 = change.line0 - ctx.context;
		if (from0 < 0) {
			from0 = 0;
		}
		var from1 = change.line1 - ctx.context;
		if (from1 < 0) {
			from1 = 0;
		}
		final last = {
			var next = change;
			while (next.next != null) {
				next = next.next;
			}
			next;
		}
		var to0 = last.line0 + last.deleted + ctx.context;
		if (to0 >= ctx.file1.lines.length) {
			to0 = ctx.file1.lines.length;
		}
		var to1 = last.line1 + last.inserted + ctx.context;
		if (to1 >= ctx.file2.lines.length) {
			to1 = ctx.file2.lines.length;
		}
		buf.add("@@ -");
		buf.add(printUnidiffNumberRange(ctx.file1, from0, to0));
		buf.add(" +");
		buf.add(printUnidiffNumberRange(ctx.file2, from1, to1));
		buf.add(" @@\n");

		function printLine(file:FileData, prefix:String, i:Int) {
			final line = file.lines[i];
			buf.add(prefix);
			buf.add(file.data.sub(line.pos, line.length).toString());
			buf.addChar("\n".code);
		}
		var i = from0;
		var j = from1;
		while (i < to0 || j < to1) {
			if (change == null || i < change.line0) {
				printLine(ctx.file1, " ", i);
				++i;
				++j;
			} else {
				for (_ in 0...change.deleted) {
					printLine(ctx.file1, "-", i++);
				}
				for (_ in 0...change.inserted) {
					printLine(ctx.file2, "+", j++);
				}
				change = change.next;
			}
		}
		return buf.toString();
	}

	static public function findHunk(ctx:Context, change:Change) {
		final threshold = ctx.context * 2 + 1;
		var previousChange = change;
		while ((change = change.next) != null) {
			if (change.line0 - (previousChange.line0 + previousChange.deleted) < threshold) {
				previousChange = change;
				continue;
			}
			break;
		}
		return previousChange;
	}

	static public function printUnidiff(ctx:Context, change:Change) {
		var buf = new StringBuf();
		buf.add(printContextHeader(ctx));
		var next = change;
		while (next != null) {
			var end = findHunk(ctx, next);
			final cur = next;
			next = end.next;
			end.next = null;
			buf.add(printUnidiffHunk(ctx, cur));
		}
		return buf.toString();
	}
}
