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

// ofInt
flags = 3;
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;

// toInt
flags.unset(EA);
var i:Int = flags;
i == 2;
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

// ofInt
flags = 3;
flags.has(EA) == true;
flags.has(EB) == true;
flags.has(EC) == false;

// toInt
flags.unset(EA);
var i:Int = flags;
i == 2;