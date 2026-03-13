package refactor.rename;

import refactor.discover.IdentifierPos;

typedef CanRenameResult = {
	var name:String;
	var pos:IdentifierPos;
}
