package cs.system.data.common;

/** Identifies which provider-specific property in the strongly typed parameter classes is to be used when setting a provider-specific type. */
@:native("System.Data.Common.DbProviderSpecificTypePropertyAttribute")
extern class DbProviderSpecificTypePropertyAttribute extends cs.system.Attribute {
	/**
	 * Indicates whether the attributed property is a provider-specific type.
	 * @return if the property that this attribute is applied to is a provider-specific
	 * type property; otherwise .
	 */
	var IsProviderSpecificTypeProperty(default, never):Bool;
	function new(isProviderSpecificTypeProperty:Bool):Void;
}
