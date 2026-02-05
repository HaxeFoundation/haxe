package cs.system.security.claims;

/** Defines constants for the well-known claim types that can be assigned to a subject. This class cannot be inherited. */
@:native("System.Security.Claims.ClaimTypes")
extern class ClaimTypes {
	/** The URI for a claim that specifies the actor, . */
	static var Actor(default, never):String;
	/** The URI for a claim that specifies the anonymous user; . */
	static var Anonymous(default, never):String;
	/** The URI for a claim that specifies details about whether an identity is authenticated, . */
	static var Authentication(default, never):String;
	/** The URI for a claim that specifies the instant at which an entity was authenticated; . */
	static var AuthenticationInstant(default, never):String;
	/** The URI for a claim that specifies the method with which an entity was authenticated; . */
	static var AuthenticationMethod(default, never):String;
	/** The URI for a claim that specifies an authorization decision on an entity; . */
	static var AuthorizationDecision(default, never):String;
	/** The URI for a claim that specifies the cookie path; . */
	static var CookiePath(default, never):String;
	/** The URI for a claim that specifies the country/region in which an entity resides, . */
	static var Country(default, never):String;
	/** The URI for a claim that specifies the date of birth of an entity, . */
	static var DateOfBirth(default, never):String;
	/** The URI for a claim that specifies the deny-only primary group SID on an entity; . A deny-only SID denies the specified entity to a securable object. */
	static var DenyOnlyPrimaryGroupSid(default, never):String;
	/** The URI for a claim that specifies the deny-only primary SID on an entity; . A deny-only SID denies the specified entity to a securable object. */
	static var DenyOnlyPrimarySid(default, never):String;
	/** The URI for a claim that specifies a deny-only security identifier (SID) for an entity, . A deny-only SID denies the specified entity to a securable object. */
	static var DenyOnlySid(default, never):String;
	/** The URI for a claim that specifies the Windows deny-only group SID of the device, . */
	static var DenyOnlyWindowsDeviceGroup(default, never):String;
	/** The URI for a claim that specifies the DNS name associated with the computer name or with the alternative name of either the subject or issuer of an X.509 certificate, . */
	static var Dns(default, never):String;
	/** . */
	static var Dsa(default, never):String;
	/** The URI for a claim that specifies the email address of an entity, . */
	static var Email(default, never):String;
	/** . */
	static var Expiration(default, never):String;
	/** . */
	static var Expired(default, never):String;
	/** The URI for a claim that specifies the gender of an entity, . */
	static var Gender(default, never):String;
	/** The URI for a claim that specifies the given name of an entity, . */
	static var GivenName(default, never):String;
	/** The URI for a claim that specifies the SID for the group of an entity, . */
	static var GroupSid(default, never):String;
	/** The URI for a claim that specifies a hash value, . */
	static var Hash(default, never):String;
	/** The URI for a claim that specifies the home phone number of an entity, . */
	static var HomePhone(default, never):String;
	/** . */
	static var IsPersistent(default, never):String;
	/** The URI for a claim that specifies the locale in which an entity resides, . */
	static var Locality(default, never):String;
	/** The URI for a claim that specifies the mobile phone number of an entity, . */
	static var MobilePhone(default, never):String;
	/** The URI for a claim that specifies the name of an entity, . */
	static var Name(default, never):String;
	/** The URI for a claim that specifies the name of an entity, . */
	static var NameIdentifier(default, never):String;
	/** The URI for a claim that specifies the alternative phone number of an entity, . */
	static var OtherPhone(default, never):String;
	/** The URI for a claim that specifies the postal code of an entity, . */
	static var PostalCode(default, never):String;
	/** The URI for a claim that specifies the primary group SID of an entity, . */
	static var PrimaryGroupSid(default, never):String;
	/** The URI for a claim that specifies the primary SID of an entity, . */
	static var PrimarySid(default, never):String;
	/** The URI for a claim that specifies the role of an entity, . */
	static var Role(default, never):String;
	/** The URI for a claim that specifies an RSA key, . */
	static var Rsa(default, never):String;
	/** The URI for a claim that specifies a serial number, . */
	static var SerialNumber(default, never):String;
	/** The URI for a claim that specifies a security identifier (SID), . */
	static var Sid(default, never):String;
	/** The URI for a claim that specifies a service principal name (SPN) claim, . */
	static var Spn(default, never):String;
	/** The URI for a claim that specifies the state or province in which an entity resides, . */
	static var StateOrProvince(default, never):String;
	/** The URI for a claim that specifies the street address of an entity, . */
	static var StreetAddress(default, never):String;
	/** The URI for a claim that specifies the surname of an entity, . */
	static var Surname(default, never):String;
	/** The URI for a claim that identifies the system entity, . */
	static var System(default, never):String;
	/** The URI for a claim that specifies a thumbprint, . A thumbprint is a globally unique SHA-1 hash of an X.509 certificate. */
	static var Thumbprint(default, never):String;
	/** The URI for a claim that specifies a user principal name (UPN), . */
	static var Upn(default, never):String;
	/** The URI for a claim that specifies a URI, . */
	static var Uri(default, never):String;
	/** The URI for a claim that specifies the user data, . */
	static var UserData(default, never):String;
	/** The URI for a claim that specifies the version, . */
	static var Version(default, never):String;
	/** The URI for a claim that specifies the webpage of an entity, . */
	static var Webpage(default, never):String;
	/** The URI for a claim that specifies the Windows domain account name of an entity, . */
	static var WindowsAccountName(default, never):String;
	/** . */
	static var WindowsDeviceClaim(default, never):String;
	/** The URI for a claim that specifies the Windows group SID of the device, . */
	static var WindowsDeviceGroup(default, never):String;
	/** . */
	static var WindowsFqbnVersion(default, never):String;
	/** . */
	static var WindowsSubAuthority(default, never):String;
	/** . */
	static var WindowsUserClaim(default, never):String;
	/** The URI for an X.500 distinguished name claim, such as the subject of an X.509 Public Key Certificate or an entry identifier in a directory services Directory Information Tree; . */
	static var X500DistinguishedName(default, never):String;
}
