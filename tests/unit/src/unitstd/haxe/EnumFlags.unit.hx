// new + has
var flags = new haxe.EnumFlags();
flags.has(EA) == false;
flags = new haxe.EnumFlags(1);
flags.has(EA) == true;

// set
flags.set(EB);
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;

// unset
flags.unset(EC);
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;
flags.unset(EA);
flags.has(EA) == false;
flags.has(EB) == true;
flags.has(EC) == false;

// new + has
var flags = new haxe.EnumFlags();
flags.has(EA) == false;
flags = new haxe.EnumFlags(1);
flags.has(EA) == true;

// set
flags.set(EB);
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;

// unset
flags.unset(EC);
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;
flags.unset(EA);
flags.has(EA) == false;
flags.has(EB) == true;
flags.has(EC) == false;


// Big Enum (32)
var bigFlags = new haxe.EnumFlags(1<<31);
bigFlags.has( EF_31 ) == true;
bigFlags.unset( EF_31 );
bigFlags.has( EF_31 ) == false;
bigFlags.set( EF_31 );
bigFlags.has( EF_31 ) == true;

// setTo
var flags = new haxe.EnumFlags();
flags.setTo(EB, true);
flags.has(EB) == true;
flags.setTo(EB, false);
flags.has(EB) == false;
flags.setTo(EB, true);
flags.has(EB) == true;