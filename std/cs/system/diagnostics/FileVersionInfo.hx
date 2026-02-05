package cs.system.diagnostics;

/** Provides version information for a physical file on disk. */
@:native("System.Diagnostics.FileVersionInfo")
extern class FileVersionInfo {
	/**
	 * Gets the comments associated with the file.
	 * @return The comments associated with the file or  if the file did not contain
	 * version information.
	 */
	var Comments(default, never):String;
	/**
	 * Gets the name of the company that produced the file.
	 * @return The name of the company that produced the file or  if the file did not
	 * contain version information.
	 */
	var CompanyName(default, never):String;
	/**
	 * Gets the build number of the file.
	 * @return A value representing the build number of the file or  if the file did
	 * not contain version information.
	 */
	var FileBuildPart(default, never):Int;
	/**
	 * Gets the description of the file.
	 * @return The description of the file or  if the file did not contain version
	 * information.
	 */
	var FileDescription(default, never):String;
	/**
	 * Gets the major part of the version number.
	 * @return A value representing the major part of the version number or 0 (zero) if
	 * the file did not contain version information.
	 */
	var FileMajorPart(default, never):Int;
	/**
	 * Gets the minor part of the version number of the file.
	 * @return A value representing the minor part of the version number of the file or
	 * 0 (zero) if the file did not contain version information.
	 */
	var FileMinorPart(default, never):Int;
	/**
	 * Gets the name of the file that this instance of  describes.
	 * @return The name of the file described by this instance of .
	 */
	var FileName(default, never):String;
	/**
	 * Gets the file private part number.
	 * @return A value representing the file private part number or  if the file did
	 * not contain version information.
	 */
	var FilePrivatePart(default, never):Int;
	/**
	 * Gets the file version number.
	 * @return The version number of the file or  if the file did not contain version
	 * information.
	 */
	var FileVersion(default, never):String;
	/**
	 * Gets the internal name of the file, if one exists.
	 * @return The internal name of the file. If none exists, this property will
	 * contain the original name of the file without the extension.
	 */
	var InternalName(default, never):String;
	/**
	 * Gets a value that specifies whether the file contains debugging information or
	 * is compiled with debugging features enabled.
	 * @return if the file contains debugging information or is compiled with debugging
	 * features enabled; otherwise, .
	 */
	var IsDebug(default, never):Bool;
	/**
	 * Gets a value that specifies whether the file has been modified and is not
	 * identical to the original shipping file of the same version number.
	 * @return if the file is patched; otherwise, .
	 */
	var IsPatched(default, never):Bool;
	/**
	 * Gets a value that specifies whether the file is a development version, rather
	 * than a commercially released product.
	 * @return if the file is prerelease; otherwise, .
	 */
	var IsPreRelease(default, never):Bool;
	/**
	 * Gets a value that specifies whether the file was built using standard release
	 * procedures.
	 * @return if the file is a private build;  if the file was built using standard
	 * release procedures or if the file did not contain version information.
	 */
	var IsPrivateBuild(default, never):Bool;
	/**
	 * Gets a value that specifies whether the file is a special build.
	 * @return if the file is a special build; otherwise, .
	 */
	var IsSpecialBuild(default, never):Bool;
	/**
	 * Gets the default language string for the version info block.
	 * @return The description string for the Microsoft Language Identifier in the
	 * version resource or  if the file did not contain version information.
	 */
	var Language(default, never):String;
	/**
	 * Gets all copyright notices that apply to the specified file.
	 * @return The copyright notices that apply to the specified file.
	 */
	var LegalCopyright(default, never):String;
	/**
	 * Gets the trademarks and registered trademarks that apply to the file.
	 * @return The trademarks and registered trademarks that apply to the file or  if
	 * the file did not contain version information.
	 */
	var LegalTrademarks(default, never):String;
	/**
	 * Gets the name the file was created with.
	 * @return The name the file was created with or  if the file did not contain
	 * version information.
	 */
	var OriginalFilename(default, never):String;
	/**
	 * Gets information about a private version of the file.
	 * @return Information about a private version of the file or  if the file did not
	 * contain version information.
	 */
	var PrivateBuild(default, never):String;
	/**
	 * Gets the build number of the product this file is associated with.
	 * @return A value representing the build number of the product this file is
	 * associated with or  if the file did not contain version information.
	 */
	var ProductBuildPart(default, never):Int;
	/**
	 * Gets the major part of the version number for the product this file is
	 * associated with.
	 * @return A value representing the major part of the product version number or  if
	 * the file did not contain version information.
	 */
	var ProductMajorPart(default, never):Int;
	/**
	 * Gets the minor part of the version number for the product the file is associated
	 * with.
	 * @return A value representing the minor part of the product version number or  if
	 * the file did not contain version information.
	 */
	var ProductMinorPart(default, never):Int;
	/**
	 * Gets the name of the product this file is distributed with.
	 * @return The name of the product this file is distributed with or  if the file
	 * did not contain version information.
	 */
	var ProductName(default, never):String;
	/**
	 * Gets the private part number of the product this file is associated with.
	 * @return A value representing the private part number of the product this file is
	 * associated with or  if the file did not contain version information.
	 */
	var ProductPrivatePart(default, never):Int;
	/**
	 * Gets the version of the product this file is distributed with.
	 * @return The version of the product this file is distributed with or  if the file
	 * did not contain version information.
	 */
	var ProductVersion(default, never):String;
	/**
	 * Gets the special build information for the file.
	 * @return The special build information for the file or  if the file did not
	 * contain version information.
	 */
	var SpecialBuild(default, never):String;
	/**
	 * Returns a  representing the version information associated with the specified
	 * file.
	 * @param fileName The fully qualified path and name of the file to retrieve the
	 * version information for.
	 * @return A  containing information about the file. If the file did not contain
	 * version information, the  contains only the name of the file requested.
	 */
	static function GetVersionInfo(fileName:String):cs.system.diagnostics.FileVersionInfo;
	/**
	 * Returns a partial list of properties in the  and their values.
	 * @return A list of the following properties in this class and their values: , , ,
	 * , , , , , , , , , . If the file did not contain version information, this list
	 * will contain only the name of the requested file. Boolean values will be , and
	 * all other entries will be .
	 */
	function ToString():String;
}
