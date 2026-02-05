package cs.system.io;

/** Provides access to information on a drive. */
@:native("System.IO.DriveInfo")
extern class DriveInfo {
	/**
	 * Indicates the amount of available free space on a drive, in bytes.
	 * @return The amount of free space available on the drive, in bytes.
	 */
	var AvailableFreeSpace(default, never):haxe.Int64;
	/**
	 * Gets the name of the file system, such as NTFS or FAT32.
	 * @return The name of the file system on the specified drive.
	 */
	var DriveFormat(default, never):String;
	/**
	 * Gets the drive type, such as CD-ROM, removable, network, or fixed.
	 * @return One of the enumeration values that specifies a drive type.
	 */
	var DriveType(default, never):cs.system.io.DriveType;
	/**
	 * Gets a value that indicates whether a drive is ready.
	 * @return if the drive is ready;  if the drive is not ready.
	 */
	var IsReady(default, never):Bool;
	/**
	 * Gets the name of a drive, such as C:\.
	 * @return The name of the drive.
	 */
	var Name(default, never):String;
	/**
	 * Gets the root directory of a drive.
	 * @return An object that contains the root directory of the drive.
	 */
	var RootDirectory(default, never):cs.system.io.DirectoryInfo;
	/**
	 * Gets the total amount of free space available on a drive, in bytes.
	 * @return The total free space available on a drive, in bytes.
	 */
	var TotalFreeSpace(default, never):haxe.Int64;
	/**
	 * Gets the total size of storage space on a drive, in bytes.
	 * @return The total size of the drive, in bytes.
	 */
	var TotalSize(default, never):haxe.Int64;
	/**
	 * Gets or sets the volume label of a drive.
	 * @return The volume label.
	 */
	var VolumeLabel(default, default):String;
	function new(driveName:String):Void;
	/**
	 * Retrieves the drive names of all logical drives on a computer.
	 * @return An array of type  that represents the logical drives on a computer.
	 */
	static function GetDrives():cs.NativeArray<cs.system.io.DriveInfo>;
	/**
	 * Returns a drive name as a string.
	 * @return The name of the drive.
	 */
	function ToString():String;
}
