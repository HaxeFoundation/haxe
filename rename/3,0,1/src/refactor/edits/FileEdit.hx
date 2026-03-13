package refactor.edits;

import refactor.discover.IdentifierPos;

enum FileEdit {
	CreateFile(newFileName:String);
	DeleteFile(fileName:String);
	Move(newFileName:String);
	ReplaceText(text:String, pos:IdentifierPos, format:FormatType);
	InsertText(text:String, pos:IdentifierPos, format:FormatType);
	RemoveText(pos:IdentifierPos);
}
