package unit.issues;

private enum EListingField<T:Obj> {
	ELCounter;
	ELID;
}

private typedef TListingConf<T:Obj> = {
	fields:Array<EListingField<T>>
}

private class Obj {
	public var id : Int;
}

class Issue7672 extends unit.Test {
	function test() {
		doesnt({ fields: [ELID] }, [null]);
	}

	function doesnt<T:Obj>( conf : TListingConf<T>, iterable : Iterable<T> )
		for (o in iterable)
			for (field in conf.fields) {
				switch field {
					case ELCounter:
						'<td>&nbsp;</td>';
					case ELID:
						HelperMacros.typedAs((null : T), o);
						'<td>${o.id}</td>';
				}
			}
}