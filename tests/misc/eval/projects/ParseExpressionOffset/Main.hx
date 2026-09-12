import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.ExprTools;

/** Checks expression ranges independently of the parser's offset mechanism. */
class Main {
	public static macro function check():Expr {
		for (offset in [0, 1, 17, 1000000]) {
			final position = Context.makePosition({file: "offset-input.hx", min: offset, max: offset + 91});
			final expression = Context.parseInlineString("1 + 2", position);
			function positionRange(value:Position, start:Int, end:Int):Void {
				final actual = Context.getPosInfos(value);
				if (actual.file != "offset-input.hx" || actual.min != offset + start || actual.max != offset + end)
					throw 'unexpected source range at offset $offset: ${actual.min}-${actual.max}';
			}
			function range(value:Expr, start:Int, end:Int):Void {
				positionRange(value.pos, start, end);
			}
			range(expression, 0, 5);
			switch expression.expr {
				case EBinop(OpAdd, left, right):
					range(left, 0, 1);
					range(right, 4, 5);
				case _: throw "addition expression changed";
			}
			final ordinary = Context.parse("1 + 2", position);
			function checkOrdinary(value:Expr):Void {
				range(value, 0, 91);
				ExprTools.iter(value, checkOrdinary);
			}
			checkOrdinary(ordinary);
			// Unicode positions count decoded characters, not UTF-8 bytes.
			final unicode = Context.parseInlineString('"é" + "🦊"', position);
			range(unicode, 0, 9);
			switch unicode.expr {
				case EBinop(OpAdd, left, right):
					range(left, 0, 3);
					range(right, 6, 9);
				case _: throw "Unicode addition expression changed";
			}
			final multiline = Context.parseInlineString("1 +\n2", position);
			range(multiline, 0, 5);
			switch multiline.expr {
				case EBinop(OpAdd, left, right):
					range(left, 0, 1);
					range(right, 4, 5);
				case _: throw "multiline addition expression changed";
			}
			var rejected = false;
			try {
				Context.parseInlineString("1 + )", position);
			} catch (error:haxe.macro.Expr.Error) {
				rejected = true;
				positionRange(error.pos, 4, 5);
				if (error.message != "Expected expression")
					throw 'parse diagnostic changed: ${error.message}';
			}
			if (!rejected)
				throw "invalid expression was accepted";
		}
		for (header in ["#!ignored\n", "\uFEFF"]) {
			Context.parseInlineString(header + "1", Context.makePosition({file: "header.hx", min: 0, max: 0}));
			var rejected = false;
			try {
				Context.parseInlineString(header + "1", Context.makePosition({file: "header.hx", min: 17, max: 17}));
			} catch (_:haxe.macro.Expr.Error) {
				rejected = true;
			}
			if (!rejected)
				throw "nonzero source offset admitted a file header";
		}
		return macro null;
	}

	static function main():Void {
		final value = 3;
		if ('outer ${'inner ${value + 1}'}' != "outer inner 4")
			throw "nested interpolation changed";
	}
}
