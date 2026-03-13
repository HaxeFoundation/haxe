package tokentree;

import haxe.macro.Expr;
import haxeparser.Data;

enum TokenTreeDef {
	Root;
	Kwd(k:Keyword);
	Const(c:haxeparser.Data.Constant);
	Sharp(s:String);
	Dollar(s:String);
	Unop(op:haxe.macro.Expr.Unop);
	Binop(op:haxe.macro.Expr.Binop);
	Comment(s:String);
	CommentLine(s:String);
	IntInterval(s:String);
	Semicolon;
	Dot;
	DblDot;
	QuestionDot;
	Arrow;
	Comma;
	BkOpen;
	BkClose;
	BrOpen;
	BrClose;
	POpen;
	PClose;
	Question;
	At;
	Eof;
	Spread;
}