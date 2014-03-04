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