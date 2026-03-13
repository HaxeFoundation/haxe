package refactor.discover;

enum FileContentType {
	Text(text:String);
	Token(root:TokenTree, text:String);
}
