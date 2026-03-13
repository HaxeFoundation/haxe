package refactor.edits;

typedef EditContext = {
	var forRealExecute:Bool;
	var docFactory:(fileName:String) -> IEditableDocument;
}
