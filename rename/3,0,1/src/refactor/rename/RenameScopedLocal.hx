package refactor.rename;

import refactor.RefactorResult;
import refactor.discover.File;
import refactor.discover.Identifier;
import refactor.discover.IdentifierPos;
import refactor.edits.Changelist;
import refactor.rename.RenameContext;

class RenameScopedLocal {
	public static function refactorScopedLocal(context:RenameContext, file:File, identifier:Identifier, scopeStart:Int, scopeEnd:Int):Promise<RefactorResult> {
		var changelist:Changelist = new Changelist(context);
		var identifierDot:String = identifier.name + ".";
		var toNameDot:String = context.what.toName + ".";
		changelist.addChange(identifier.pos.fileName, ReplaceText(context.what.toName, identifier.pos, NoFormat), identifier);

		var allUses:Array<Identifier> = identifier.defineType.findAllIdentifiers(function(ident:Identifier) {
			if (ident.pos.start < scopeStart) {
				return false;
			}
			if (ident.pos.start > scopeEnd) {
				return false;
			}
			if (ident.name == identifier.name) {
				return true;
			}
			if (ident.name.startsWith(identifierDot)) {
				return true;
			}
			return false;
		});
		var allShadows:Array<Identifier> = identifier.defineType.findAllIdentifiers(function(ident:Identifier) {
			if (ident.pos.start < scopeStart) {
				return false;
			}
			if (ident.pos.start > scopeEnd) {
				return false;
			}
			if (ident.name == context.what.toName) {
				return true;
			}
			if (ident.name.startsWith(toNameDot)) {
				return true;
			}
			return false;
		});

		for (use in allShadows) {
			switch (use.type) {
				case Access | Call(_):
					var pos:IdentifierPos = {
						fileName: use.pos.fileName,
						start: use.pos.start,
						end: use.pos.start
					};
					changelist.addChange(use.pos.fileName, InsertText("this.", pos, NoFormat), use);
				case ScopedLocal(_, _):
					return Promise.reject('local var "${context.what.toName}" exists');
				default:
			}
		}

		var skipForIterator:Bool = false;
		var innerScopeStart:Int = 0;
		var innerScopeEnd:Int = -1;
		for (use in allUses) {
			if ((innerScopeStart < use.pos.start) && (use.pos.start < innerScopeEnd)) {
				continue;
			}
			switch (use.type) {
				case ScopedLocal(start, scopeEnd, _):
					if (use.pos.start == identifier.pos.start) {
						scopeStart = start;
						skipForIterator = true;
					} else {
						innerScopeStart = start;
						innerScopeEnd = scopeEnd;
						continue;
					}
				case StructureField(_):
					continue;
				case ForIterator:
					if (skipForIterator) {
						skipForIterator = false;
						continue;
					}
				default:
					if (use.pos.start < scopeStart) {
						continue;
					}
			}
			if (use.name == identifier.name) {
				// exact match
				changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, use.pos, NoFormat), use);
			} else {
				// starts with identifier + "." -> replace only identifier part
				var pos:IdentifierPos = {
					fileName: use.pos.fileName,
					start: use.pos.start,
					end: use.pos.start + identifier.pos.end - identifier.pos.start
				};
				changelist.addChange(use.pos.fileName, ReplaceText(context.what.toName, pos, NoFormat), use);
			}
		}
		return Promise.resolve(changelist.execute());
	}
}
