package cs.system.runtime.interopservices.comtypes;

/** Provides the managed definition of the  interface, with COM functionality from  and . */
@:native("System.Runtime.InteropServices.ComTypes.IMoniker")
extern interface IMoniker {
	/**
	 * Uses the moniker to bind to the object that it identifies.
	 * @param pbc A reference to the  interface on the bind context object used in this
	 * binding operation.
	 * @param pmkToLeft A reference to the moniker to the left of the current moniker,
	 * if the moniker is part of a composite moniker.
	 * @param riidResult The interface identifier (IID) of the interface that the
	 * client intends to use to communicate with the object that the moniker
	 * identifies.
	 * @param ppvResult When this method returns, contains a reference to the interface
	 * requested by . This parameter is passed uninitialized.
	 */
	function BindToObject(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, riidResult:cs.Ref<cs.system.Guid>, ppvResult:cs.Ref<Dynamic>):Void;
	/**
	 * Retrieves an interface pointer to the storage that contains the object
	 * identified by the moniker.
	 * @param pbc A reference to the  interface on the bind context object used during
	 * this binding operation.
	 * @param pmkToLeft A reference to the moniker to the left of the current moniker,
	 * if the moniker is part of a composite moniker.
	 * @param riid The interface identifier (IID) of the storage interface requested.
	 * @param ppvObj When this method returns, contains a reference to the interface
	 * requested by . This parameter is passed uninitialized.
	 */
	function BindToStorage(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, riid:cs.Ref<cs.system.Guid>, ppvObj:cs.Ref<Dynamic>):Void;
	/**
	 * Creates a new moniker based on the common prefix that this moniker shares with
	 * another moniker.
	 * @param pmkOther A reference to the  interface on another moniker to compare with
	 * the current moniker for a common prefix.
	 * @param ppmkPrefix When this method returns, contains the moniker that is the
	 * common prefix of the current moniker and . This parameter is passed
	 * uninitialized.
	 */
	function CommonPrefixWith(pmkOther:cs.system.runtime.interopservices.comtypes.IMoniker, ppmkPrefix:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Combines the current moniker with another moniker, creating a new composite
	 * moniker.
	 * @param pmkRight A reference to the  interface on a moniker to append to the end
	 * of the current moniker.
	 * @param fOnlyIfNotGeneric to indicate that the caller requires a nongeneric
	 * composition. The operation proceeds only if  is a moniker class that the current
	 * moniker can combine with in some way other than forming a generic composite.  to
	 * indicate that the method can create a generic composite if necessary.
	 * @param ppmkComposite When this method returns, contains a reference to the
	 * resulting composite moniker. This parameter is passed uninitialized.
	 */
	function ComposeWith(pmkRight:cs.system.runtime.interopservices.comtypes.IMoniker, fOnlyIfNotGeneric:Bool, ppmkComposite:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Supplies a pointer to an enumerator that can enumerate the components of a
	 * composite moniker.
	 * @param fForward to enumerate the monikers from left to right.  to enumerate from
	 * right to left.
	 * @param ppenumMoniker When this method returns, contains a reference to the
	 * enumerator object for the moniker. This parameter is passed uninitialized.
	 */
	function Enum(fForward:Bool, ppenumMoniker:cs.Ref<cs.system.runtime.interopservices.comtypes.IEnumMoniker>):Void;
	/**
	 * Retrieves the class identifier (CLSID) of an object.
	 * @param pClassID When this method returns, contains the CLSID. This parameter is
	 * passed uninitialized.
	 */
	function GetClassID(pClassID:cs.Ref<cs.system.Guid>):Void;
	/**
	 * Gets the display name, which is a user-readable representation of the current
	 * moniker.
	 * @param pbc A reference to the bind context to use in this operation.
	 * @param pmkToLeft A reference to the moniker to the left of the current moniker,
	 * if the moniker is part of a composite moniker.
	 * @param ppszDisplayName When this method returns, contains the display name
	 * string. This parameter is passed uninitialized.
	 */
	function GetDisplayName(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, ppszDisplayName:cs.Ref<String>):Void;
	/**
	 * Returns the size, in bytes, of the stream needed to save the object.
	 * @param pcbSize When this method returns, contains a  value indicating the size,
	 * in bytes, of the stream needed to save this object. This parameter is passed
	 * uninitialized.
	 */
	function GetSizeMax(pcbSize:cs.Ref<haxe.Int64>):Void;
	/**
	 * Provides a number representing the time that the object identified by the
	 * current moniker was last changed.
	 * @param pbc A reference to the bind context to use in this binding operation.
	 * @param pmkToLeft A reference to the moniker to the left of the current moniker,
	 * if the moniker is part of a composite moniker.
	 * @param pFileTime When this method returns, contains the time of the last change.
	 * This parameter is passed uninitialized.
	 */
	function GetTimeOfLastChange(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, pFileTime:cs.Ref<cs.system.runtime.interopservices.comtypes.FILETIME>):Void;
	/**
	 * Calculates a 32-bit integer using the internal state of the moniker.
	 * @param pdwHash When this method returns, contains the hash value for this
	 * moniker. This parameter is passed uninitialized.
	 */
	function Hash(pdwHash:cs.Ref<Int>):Void;
	/**
	 * Provides a moniker that, when composed to the right of the current moniker or
	 * one of similar structure, composes to nothing.
	 * @param ppmk When this method returns, contains a moniker that is the inverse of
	 * the current moniker. This parameter is passed uninitialized.
	 */
	function Inverse(ppmk:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Checks the object for changes since it was last saved.
	 * @return An  value if the object has changed; otherwise, an  value.
	 */
	function IsDirty():Int;
	/**
	 * Compares the current moniker with a specified moniker and indicates whether they
	 * are identical.
	 * @param pmkOtherMoniker A reference to the moniker to use for comparison.
	 * @return An  value if the monikers are identical; otherwise, an  value.
	 */
	function IsEqual(pmkOtherMoniker:cs.system.runtime.interopservices.comtypes.IMoniker):Int;
	/**
	 * Determines whether the object that is identified by the current moniker is
	 * currently loaded and running.
	 * @param pbc A reference to the bind context to use in this binding operation.
	 * @param pmkToLeft A reference to the moniker to the left of the current moniker
	 * if the current moniker is part of a composite.
	 * @param pmkNewlyRunning A reference to the moniker most recently added to the
	 * Running Object Table (ROT).
	 * @return An  value if the moniker is running; an  value if the moniker is not
	 * running; or an  value.
	 */
	function IsRunning(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, pmkNewlyRunning:cs.system.runtime.interopservices.comtypes.IMoniker):Int;
	/**
	 * Indicates whether this moniker is of one of the system-supplied moniker classes.
	 * @param pdwMksys When this method returns, contains a pointer to an integer that
	 * is one of the values from the  enumeration, and refers to one of the COM moniker
	 * classes. This parameter is passed uninitialized.
	 * @return An  value if the moniker is a system moniker; otherwise, an  value.
	 */
	function IsSystemMoniker(pdwMksys:cs.Ref<Int>):Int;
	/**
	 * Initializes an object from the stream where it was previously saved.
	 * @param pStm The stream that the object is loaded from.
	 */
	function Load(pStm:cs.system.runtime.interopservices.comtypes.IStream):Void;
	/**
	 * Reads as many characters of the specified display name as the  understands and
	 * builds a moniker corresponding to the portion read.
	 * @param pbc A reference to the bind context to use in this binding operation.
	 * @param pmkToLeft A reference to the moniker that has been built from the display
	 * name up to this point.
	 * @param pszDisplayName A reference to the string containing the remaining display
	 * name to parse.
	 * @param pchEaten When this method returns, contains the number of characters that
	 * were consumed in parsing . This parameter is passed uninitialized.
	 * @param ppmkOut When this method returns, contains a reference to the moniker
	 * that was built from . This parameter is passed uninitialized.
	 */
	function ParseDisplayName(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, pmkToLeft:cs.system.runtime.interopservices.comtypes.IMoniker, pszDisplayName:String, pchEaten:cs.Ref<Int>, ppmkOut:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Returns a reduced moniker, which is another moniker that refers to the same
	 * object as the current moniker but can be bound with equal or greater efficiency.
	 * @param pbc A reference to the  interface on the bind context to use in this
	 * binding operation.
	 * @param dwReduceHowFar A value that specifies how far the current moniker should
	 * be reduced.
	 * @param ppmkToLeft A reference to the moniker to the left of the current moniker.
	 * @param ppmkReduced When this method returns, contains a reference to the reduced
	 * form of the current moniker, which can be  if an error occurs or if the current
	 * moniker is reduced to nothing. This parameter is passed uninitialized.
	 */
	function Reduce(pbc:cs.system.runtime.interopservices.comtypes.IBindCtx, dwReduceHowFar:Int, ppmkToLeft:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>, ppmkReduced:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Supplies a moniker that, when appended to the current moniker (or one with a
	 * similar structure), yields the specified moniker.
	 * @param pmkOther A reference to the moniker to which a relative path should be
	 * taken.
	 * @param ppmkRelPath When this method returns, contains a reference to the
	 * relative moniker. This parameter is passed uninitialized.
	 */
	function RelativePathTo(pmkOther:cs.system.runtime.interopservices.comtypes.IMoniker, ppmkRelPath:cs.Ref<cs.system.runtime.interopservices.comtypes.IMoniker>):Void;
	/**
	 * Saves an object to the specified stream.
	 * @param pStm The stream to which the object is saved.
	 * @param fClearDirty to clear the modified flag after the save is complete;
	 * otherwise
	 */
	function Save(pStm:cs.system.runtime.interopservices.comtypes.IStream, fClearDirty:Bool):Void;
}
