import sys.io.Process;

using StringTools;

enum abstract Arch(String) {
	final Arm64;
	final Arm;
	final X86;
	final X86_64;

	public function getNdllSuffix():String {
		return switch abstract {
			case Arm64: "Arm64";
			case Arm: "Arm";
			case X86_64: "64";
			case X86: "";
		};
	}
}

function getArchWindows() {
	return switch Sys.getEnv("PROCESSOR_ARCHITECTURE") {
		case "x86": X86;
		case "AMD64": X86_64;
		case "ARM64": Arm64;
		case other: throw 'Unknown CPU architecture: $other';
	};
}

function getArchUnix() {
	final uname = new Process("uname", ["-m"]);

	final arch = try {
		uname.stdout.readLine();
	} catch (e:haxe.io.Eof) {
		"";
	};

	uname.kill();
	uname.close();

	return switch arch {
		case "x86_64" | "amd64": X86_64;
		case "i386" | "x86": X86;
		case "arm64" | "aarch64": Arm64;
		case "arm": Arm;
		case other: throw 'Unknown CPU architecture: "$other"';
	};
}

function getArch() {
	return switch Sys.systemName() {
		case "Windows":	getArchWindows();
		default: getArchUnix();
	};
}

function main() {
	final arch = getArch();

	final expectedNdllSubDir = Sys.systemName() + arch.getNdllSuffix() + "/";

	final ndllPath = neko.vm.Loader.local().getPath()[0];

	if (ndllPath.endsWith(expectedNdllSubDir)) {
		Sys.println("Success");
	} else {
		Sys.println('Failure: Expected $ndllPath to end with $expectedNdllSubDir');
	}
}
