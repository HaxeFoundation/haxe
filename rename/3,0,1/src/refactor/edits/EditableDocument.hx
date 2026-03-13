package refactor.edits;

import haxe.io.Bytes;
import haxe.io.Path;
import sys.FileSystem;
import sys.io.File;

class EditableDocument implements IEditableDocument {
	var originalContent:Bytes;
	var originalFileName:String;
	var fileName:String;
	var refactoredContent:StringBuf;
	var lastPos:Int;

	public function new(fileName:String) {
		this.fileName = fileName;
		this.originalFileName = fileName;
		originalContent = File.getBytes(fileName);
		refactoredContent = new StringBuf();
		lastPos = 0;
	}

	public function addChange(edit:FileEdit) {
		switch (edit) {
			case CreateFile(newFileName):
				fileName = newFileName;
			case DeleteFile(oldFileName):
				fileName = oldFileName;
			case Move(newFileName):
				fileName = newFileName;
			case ReplaceText(text, pos, _):
				refactoredContent.add(originalContent.getString(lastPos, pos.start - lastPos));
				refactoredContent.add(text);
				lastPos = pos.end;
			case InsertText(text, pos, _):
				refactoredContent.add(originalContent.getString(lastPos, pos.start - lastPos));
				refactoredContent.add(text);
				lastPos = pos.start;
			case RemoveText(pos):
				refactoredContent.add(originalContent.getString(lastPos, pos.start - lastPos));
				lastPos = pos.end;
		}
	}

	public function endEdits() {
		if (lastPos < originalContent.length) {
			refactoredContent.add(originalContent.getString(lastPos, originalContent.length - lastPos));
		}
		var folder:String = Path.directory(fileName);
		if (!FileSystem.isDirectory(folder)) {
			FileSystem.createDirectory(folder);
		}

		File.saveContent(fileName, refactoredContent.toString());
		if (fileName != originalFileName) {
			FileSystem.deleteFile(originalFileName);
		}
	}
}
