package refactor.edits;

enum FormatType {
	NoFormat;
	Format(indentOffset:Int, trimRight:Bool);
}
