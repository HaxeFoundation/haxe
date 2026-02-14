package runci.targets;

import haxe.io.Path;
import runci.Config.*;
import runci.System.*;

class Cs {
	static final miscCsDir = getMiscSubDir('cs');

	// Get the runtime identifier for dotnet publish
	static function getRuntimeId():String {
		return switch (Sys.systemName()) {
			case "Windows":
				// Windows CI runners are x64, check PROCESSOR_ARCHITECTURE for local dev
				var arch = Sys.getEnv("PROCESSOR_ARCHITECTURE");
				if (arch == "ARM64") "win-arm64" else "win-x64";
			case "Mac":
				// Check architecture using uname
				var arch = getUnixArch();
				if (arch == "arm64") "osx-arm64" else "osx-x64";
			case "Linux":
				var arch = getUnixArch();
				if (arch == "aarch64" || arch == "arm64") "linux-arm64" else "linux-x64";
			default: "linux-x64";
		};
	}

	// Get CPU architecture using uname -m (Unix/Linux/Mac only)
	static function getUnixArch():String {
		var process = new sys.io.Process("uname", ["-m"]);
		var arch = StringTools.trim(process.stdout.readAll().toString());
		process.close();
		return arch;
	}

	// Get AOT binary path
	static function getAotBinaryPath():String {
		var ext = if (Sys.systemName() == "Windows") ".exe" else "";
		return 'bin/aot/Project$ext';
	}

	// Create Project.aot.csproj file for AOT compilation
	static function createAotProjectFile():Void {
		sys.io.File.saveContent("Project.aot.csproj", '<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <AssemblyName>Project</AssemblyName>
    <ImplicitUsings>disable</ImplicitUsings>
    <Nullable>disable</Nullable>
    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
    <PublishAot>true</PublishAot>
    <TrimmerSingleWarn>false</TrimmerSingleWarn>
  </PropertyGroup>
</Project>');
	}

	// Run AOT test for a compiled C# project in the current directory
	static function runAotTest():Void {
		createAotProjectFile();

		var rid = getRuntimeId();
		infoMsg('Publishing AOT binary for $rid...');
		runCommand("dotnet", ["publish", "Project.aot.csproj", "-c", "Release", "-o", "bin/aot"]);

		var aotBinary = getAotBinaryPath();
		if (!sys.FileSystem.exists(aotBinary)) {
			throw 'AOT binary not found at $aotBinary';
		}

		infoMsg('Running AOT binary...');
		// Use full path for cross-platform compatibility
		var fullPath = sys.FileSystem.fullPath(aotBinary);
		runCommand(fullPath, []);
	}

	// Build utility binary to AOT (for sys tests)
	static function buildUtilityAot(utilDir:String):Void {
		if (!sys.FileSystem.exists('$utilDir/Project.csproj'))
			return;

		infoMsg('Building $utilDir to AOT...');
		changeDirectory(utilDir);
		createAotProjectFile();
		runCommand("dotnet", ["publish", "Project.aot.csproj", "-c", "Release", "-o", "bin/aot"]);
		changeDirectory("../..");
	}

	// Run AOT sys test (with EXISTS=1 environment variable)
	static function runAotSysTest():Void {
		createAotProjectFile();

		var rid = getRuntimeId();
		infoMsg('Publishing AOT binary for $rid...');
		runCommand("dotnet", ["publish", "Project.aot.csproj", "-c", "Release", "-o", "bin/aot"]);

		var aotBinary = getAotBinaryPath();
		if (!sys.FileSystem.exists(aotBinary)) {
			throw 'AOT binary not found at $aotBinary';
		}

		infoMsg('Running AOT binary (sys test)...');
		// Use full path for cross-platform compatibility
		var fullPath = sys.FileSystem.fullPath(aotBinary);
		runSysTest(fullPath, []);
	}

	static public function run(args:Array<String>) {
		// === Setup hxcs library ===
		if (!sys.FileSystem.exists(partyDir))
			sys.FileSystem.createDirectory(partyDir);
		changeDirectory(partyDir);
		if (!sys.FileSystem.exists("hxcs")) {
			runCommand("git", ["clone", "-b", Config.hxcsVersion, "https://github.com/jeremyfa/hxcs.git", "hxcs"]);
		}
		haxelibDev("hxcs", Path.join([partyDir, "hxcs"]));
		changeDirectory(unitDir);

		deleteDirectoryRecursively("bin/cs");

		runCommand("dotnet", ["--version"]);

		// === Unit Tests ===
		// JIT (without cs.aot or cs.closure-cache — different generated C# than AOT)
		runCommand("haxe", ["compile-cs.hxml"].concat(args));
		changeDirectory("bin/cs");
		infoMsg("=== Running Unit Tests (JIT) ===");
		runCommand("dotnet", ["run"]);
		changeDirectory(unitDir);

		// AOT (with cs.aot and cs.closure-cache — separate compilation, different generated C#)
		deleteDirectoryRecursively("bin/cs");
		runCommand("haxe", ["compile-cs.hxml", "-D", "cs.aot", "-D", "cs.closure-cache"].concat(args));
		changeDirectory("bin/cs");
		infoMsg("=== Running Unit Tests (AOT) ===");
		runAotTest();

		changeDirectory(unitDir);

		Display.maybeRunDisplayTests(Cs);

		// === Misc Tests (Bootstrap) ===
		// JIT
		changeDirectory(miscCsDir);
		deleteDirectoryRecursively("projects/Bootstrap/bin");
		changeDirectory("projects/Bootstrap");
		runCommand("haxe", ["compile.hxml", "-lib", "hxcs"].concat(args));
		changeDirectory("bin");
		infoMsg("=== Running Bootstrap Tests (JIT) ===");
		runCommand("dotnet", ["run"]);

		// AOT (separate compilation)
		changeDirectory(miscCsDir);
		deleteDirectoryRecursively("projects/Bootstrap/bin");
		changeDirectory("projects/Bootstrap");
		runCommand("haxe", ["compile.hxml", "-lib", "hxcs", "-D", "cs.aot", "-D", "cs.closure-cache"].concat(args));
		changeDirectory("bin");
		infoMsg("=== Running Bootstrap Tests (AOT) ===");
		runAotTest();

		// === Sys Tests ===
		// JIT
		changeDirectory(sysDir);
		runCommand("haxe", ["compile-cs.hxml"].concat(args));
		buildUtilityAot("bin/cs-args");
		buildUtilityAot("bin/cs-exit");
		buildUtilityAot("bin/cs-utility");
		infoMsg("=== Running Sys Tests (JIT) ===");
		runSysTest("dotnet", ["run", "--project", "bin/cs/Project.csproj"]);

		// AOT (separate compilation)
		changeDirectory(sysDir);
		runCommand("haxe", ["compile-cs.hxml", "-D", "cs.aot", "-D", "cs.closure-cache"].concat(args));
		buildUtilityAot("bin/cs-args");
		buildUtilityAot("bin/cs-exit");
		buildUtilityAot("bin/cs-utility");
		infoMsg("=== Running Sys Tests (AOT) ===");
		changeDirectory("bin/cs");
		createAotProjectFile();
		infoMsg('Publishing AOT binary...');
		runCommand("dotnet", ["publish", "Project.aot.csproj", "-c", "Release", "-o", "bin/aot"]);
		var aotBinary = getAotBinaryPath();
		if (!sys.FileSystem.exists(aotBinary)) {
			throw 'AOT binary not found at $aotBinary';
		}
		changeDirectory(sysDir);
		var fullPath = sys.FileSystem.fullPath('bin/cs/$aotBinary');
		infoMsg('Running AOT binary (sys test)...');
		runSysTest(fullPath, []);

		// === Thread Tests ===
		// JIT
		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-cs", "export/cs", "-lib", "hxcs"].concat(args));
		changeDirectory("export/cs");
		infoMsg("=== Running Thread Tests (JIT) ===");
		runCommand("dotnet", ["run"]);

		// AOT (separate compilation)
		changeDirectory(threadsDir);
		deleteDirectoryRecursively("export/cs");
		runCommand("haxe", ["build.hxml", "-cs", "export/cs", "-lib", "hxcs", "-D", "cs.aot", "-D", "cs.closure-cache"].concat(args));
		changeDirectory("export/cs");
		infoMsg("=== Running Thread Tests (AOT) ===");
		runAotTest();

		// === Coroutine Tests (hxcoro) ===
		if (!sys.FileSystem.exists(partyDir))
			sys.FileSystem.createDirectory(partyDir);
		changeDirectory(partyDir);
		if (!sys.FileSystem.exists("hxcoro")) {
			runCommand("git", ["clone", "-b", Config.hxcoroVersion, "https://github.com/HaxeFoundation/hxcoro", "hxcoro"]);
		}
		changeDirectory("hxcoro");
		// Patch Setup.hx to add cs target support (until hxcoro upstream adds it)
		var setupPath = "src/hxcoro/run/Setup.hx";
		var content = sys.io.File.getContent(setupPath);
		content = StringTools.replace(content, "#elseif (jvm || cpp || hl)", "#elseif (jvm || cpp || hl || cs)");
		sys.io.File.saveContent(setupPath, content);
		runCommand("haxelib", ["newrepo"]);
		runCommand("haxelib", ["git", "utest", "https://github.com/haxe-utest/utest.git"]);
		runCommand("haxelib", ["dev", "hxcoro", "."]);
		runCommand("haxelib", ["dev", "hxcs", Path.join([partyDir, "hxcs"])]);
		// JIT
		runCommand("haxe", ["--cwd", "tests", "build-base.hxml", "--cs", "bin/cs", "-lib", "hxcs"]);
		changeDirectory("tests/bin/cs");
		infoMsg("=== Running Coroutine Tests (JIT) ===");
		runCommand("dotnet", ["run"]);

		// AOT (separate compilation)
		changeDirectory(partyDir);
		changeDirectory("hxcoro");
		deleteDirectoryRecursively("tests/bin/cs");
		runCommand("haxe", ["--cwd", "tests", "build-base.hxml", "--cs", "bin/cs", "-lib", "hxcs", "-D", "cs.aot", "-D", "cs.closure-cache"]);
		changeDirectory("tests/bin/cs");
		infoMsg("=== Running Coroutine Tests (AOT) ===");
		runAotTest();
	}
}
