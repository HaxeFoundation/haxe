package cs;
import cs.internal.Exceptions;
import cs.internal.FieldLookup;
import cs.internal.Function;
import cs.internal.HxObject;
import cs.internal.Runtime;
import cs.internal.Iterator;
import cs.internal.Null;
import cs.internal.StringExt;
import cs.StdTypes;
import Hash;
import Reflect;

class Boot 
{

	@:keep public static function init():Void
	{
		cs.Lib.applyCultureChanges();
	}
	
}