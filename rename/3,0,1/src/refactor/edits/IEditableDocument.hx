package refactor.edits;

interface IEditableDocument {
	function addChange(edit:FileEdit):Void;

	function endEdits():Void;
}
