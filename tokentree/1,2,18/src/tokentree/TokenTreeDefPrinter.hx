package tokentree;

class TokenTreeDefPrinter {
	public static function toString(def:TokenTreeDef):String {
		return switch (def) {
			case Root: "<root>";
			case Kwd(k): k.getName().substr(3).toLowerCase();
			#if (haxe >= version("4.3.0-rc.1"))
			case Const(CInt(v, null) | CFloat(v, null)): v;
			case Const(CInt(v, s) | CFloat(v, s)): '$v$s';
			#else
			case Const(CInt(s) | CFloat(s)): s;
			#end
			case Const(CIdent(s)): s;
			case Const(CString(s)): '"$s"';
			case Const(CRegexp(r, opt)): '~/$r/$opt';
			case Const(CMarkup(s)): '$s';
			case Sharp(s): '#$s';
			case Dollar(s): '$$$s';
			case Unop(op): new haxe.macro.Printer("").printUnop(op);
			case Binop(op): new haxe.macro.Printer("").printBinop(op);
			case Comment(s): '/*$s*/';
			case CommentLine(s): '//$s';
			case IntInterval(s): '$s...';
			case Semicolon: ";";
			case Dot: ".";
			case DblDot: ":";
			case QuestionDot: "?.";
			case Arrow: "->";
			case Comma: ",";
			case BkOpen: "[";
			case BkClose: "]";
			case BrOpen: "{";
			case BrClose: "}";
			case POpen: "(";
			case PClose: ")";
			case Question: "?";
			case At: "@";
			case Eof: "<eof>";
			case Spread: "...";
		}
	}
}