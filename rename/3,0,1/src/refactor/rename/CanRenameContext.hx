package refactor.rename;

import refactor.CacheAndTyperContext;
typedef CanRenameContext = CacheAndTyperContext & {
	var what:RenameWhat;
}
