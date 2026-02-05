package cs.system.io.isolatedstorage;

/** Represents the abstract base class from which all isolated storage implementations must derive. */
@:native("System.IO.IsolatedStorage.IsolatedStorage")
extern class IsolatedStorage extends cs.system.MarshalByRefObject {
	/**
	 * Gets an application identity that scopes isolated storage.
	 * @return An  that represents the  identity.
	 */
	var ApplicationIdentity(default, never):Dynamic;
	/**
	 * Gets an assembly identity used to scope isolated storage.
	 * @return An  that represents the  identity.
	 */
	var AssemblyIdentity(default, never):Dynamic;
	/**
	 * When overridden in a derived class, gets the available free space for isolated
	 * storage, in bytes.
	 * @return The available free space for isolated storage, in bytes.
	 */
	var AvailableFreeSpace(default, never):haxe.Int64;
	/**
	 * Gets a value representing the current size of isolated storage.
	 * @return The number of storage units currently used within the isolated storage
	 * scope.
	 */
	var CurrentSize(default, never):cs.UInt64;
	/**
	 * Gets a domain identity that scopes isolated storage.
	 * @return An  that represents the  identity.
	 */
	var DomainIdentity(default, never):Dynamic;
	/**
	 * Gets a value representing the maximum amount of space available for isolated
	 * storage. When overridden in a derived class, this value can take different units
	 * of measure.
	 * @return The maximum amount of isolated storage space in bytes. Derived classes
	 * can return different units of value.
	 */
	var MaximumSize(default, never):cs.UInt64;
	/**
	 * When overridden in a derived class, gets a value that represents the maximum
	 * amount of space available for isolated storage.
	 * @return The limit of isolated storage space, in bytes.
	 */
	var Quota(default, never):haxe.Int64;
	/**
	 * Gets an  enumeration value specifying the scope used to isolate the store.
	 * @return A bitwise combination of  values specifying the scope used to isolate
	 * the store.
	 */
	var Scope(default, never):cs.system.io.isolatedstorage.IsolatedStorageScope;
	/**
	 * Gets a backslash character that can be used in a directory string. When
	 * overridden in a derived class, another character might be returned.
	 * @return The default implementation returns the '\' (backslash) character.
	 */
	var SeparatorExternal(default, never):cs.Char16;
	/**
	 * Gets a period character that can be used in a directory string. When overridden
	 * in a derived class, another character might be returned.
	 * @return The default implementation returns the '.' (period) character.
	 */
	var SeparatorInternal(default, never):cs.Char16;
	/**
	 * When overridden in a derived class, gets a value that represents the amount of
	 * the space used for isolated storage.
	 * @return The used amount of isolated storage space, in bytes.
	 */
	var UsedSize(default, never):haxe.Int64;
	/**
	 * When overridden in a derived class, prompts a user to approve a larger quota
	 * size, in bytes, for isolated storage.
	 * @param newQuotaSize The requested new quota size, in bytes, for the user to
	 * approve.
	 * @return in all cases.
	 */
	function IncreaseQuotaTo(newQuotaSize:haxe.Int64):Bool;
	/** When overridden in a derived class, removes the individual isolated store and all contained data. */
	function Remove():Void;
}
