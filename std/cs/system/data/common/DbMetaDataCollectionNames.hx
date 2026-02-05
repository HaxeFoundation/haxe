package cs.system.data.common;

/** Provides a list of constants for the well-known MetaDataCollections: DataSourceInformation, DataTypes, MetaDataCollections, ReservedWords, and Restrictions. */
@:native("System.Data.Common.DbMetaDataCollectionNames")
extern class DbMetaDataCollectionNames {
	/** A constant for use with the  method that represents the DataSourceInformation collection. */
	static var DataSourceInformation(default, never):String;
	/** A constant for use with the  method that represents the DataTypes collection. */
	static var DataTypes(default, never):String;
	/** A constant for use with the  method that represents the MetaDataCollections collection. */
	static var MetaDataCollections(default, never):String;
	/** A constant for use with the  method that represents the ReservedWords collection. */
	static var ReservedWords(default, never):String;
	/** A constant for use with the  method that represents the Restrictions collection. */
	static var Restrictions(default, never):String;
}
