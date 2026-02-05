package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface. */
@:native("System.Runtime.InteropServices.ComTypes.ITypeComp")
extern interface ITypeComp {
	/**
	 * Maps a name to a member of a type, or binds global variables and functions
	 * contained in a type library.
	 * @param szName The name to bind.
	 * @param lHashVal A hash value for  computed by .
	 * @param wFlags A flags word containing one or more of the invoke flags defined in
	 * the  enumeration.
	 * @param ppTInfo When this method returns, contains a reference to the type
	 * description that contains the item to which it is bound, if a  or  was returned.
	 * This parameter is passed uninitialized.
	 * @param pDescKind When this method returns, contains a reference to a  enumerator
	 * that indicates whether the name bound-to is a , , or . This parameter is passed
	 * uninitialized.
	 * @param pBindPtr When this method returns, contains a reference to the bound-to ,
	 * , or  interface. This parameter is passed uninitialized.
	 */
	function Bind(szName:String, lHashVal:Int, wFlags:cs.Int16, ppTInfo:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>, pDescKind:cs.Ref<cs.system.runtime.interopservices.comtypes.DESCKIND>, pBindPtr:cs.Ref<cs.system.runtime.interopservices.comtypes.BINDPTR>):Void;
	/**
	 * Binds to the type descriptions contained within a type library.
	 * @param szName The name to bind.
	 * @param lHashVal A hash value for  determined by .
	 * @param ppTInfo When this method returns, contains a reference to an  of the type
	 * to which  was bound. This parameter is passed uninitialized.
	 * @param ppTComp When this method returns, contains a reference to an  variable.
	 * This parameter is passed uninitialized.
	 */
	function BindType(szName:String, lHashVal:Int, ppTInfo:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeInfo>, ppTComp:cs.Ref<cs.system.runtime.interopservices.comtypes.ITypeComp>):Void;
}
