package refactor.refactor.extractmethod;

typedef ExtractMethodData = {
	var content:String;
	var root:TokenTree;
	var startToken:TokenTree;
	var endToken:TokenTree;
	var newMethodName:String;
	var newMethodOffset:Int;
	var functionToken:TokenTree;
	var isStatic:Bool;
	var isSingleExpr:Bool;
	var functionType:LocalFunctionType;
	var functionIndent:Int;
	var snippetIndent:Int;
}

enum LocalFunctionType {
	NoFunction;
	Named(name:String);
	Unnamed;
}
