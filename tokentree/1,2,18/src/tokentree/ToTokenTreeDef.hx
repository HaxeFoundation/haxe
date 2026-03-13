package tokentree;

import haxeparser.Data;

abstract ToTokenTreeDef(TokenTreeDef) {
	function new(tok:TokenTreeDef) {
		this = tok;
	}

	@:to
	public function toTokenTreeDef():TokenTreeDef {
		return this;
	}

	@:from
	public static function fromTokenDef(tok:TokenDef):ToTokenTreeDef {
		return new ToTokenTreeDef(switch (tok) {
			case Kwd(k): Kwd(k);
			case Const(c): switch (c) {
					#if (haxe >= version("4.3.0-rc.1"))
					case CInt(v, s): Const(CInt(v, s));
					case CFloat(f, s): Const(CFloat(f, s));
					#else
					case CInt(v): Const(CInt(v));
					case CFloat(f): Const(CFloat(f));
					#end
					case CString(s, kind): Const(CString(s, kind));
					case CIdent(s): Const(CIdent(s));
					case CRegexp(r, opt): Const(CRegexp(r, opt));
					case CMarkup(s): Const(CMarkup(s));
				}
			case Sharp(s): Sharp(s);
			case Dollar(s): Dollar(s);
			case Unop(op): Unop(op);
			case Binop(op): Binop(op);
			case Comment(s): Comment(s);
			case CommentLine(s): CommentLine(s);
			case IntInterval(s): IntInterval(s);
			case Semicolon: Semicolon;
			case Dot: Dot;
			case DblDot: DblDot;
			case QuestionDot: QuestionDot;
			case Arrow: Arrow;
			case Comma: Comma;
			case BkOpen: BkOpen;
			case BkClose: BkClose;
			case BrOpen: BrOpen;
			case BrClose: BrClose;
			case POpen: POpen;
			case PClose: PClose;
			case Question: Question;
			case At: At;
			case Eof: Eof;
			case Spread: Spread;
		});
	}
}