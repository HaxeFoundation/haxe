package runci.targets;

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
		return 'bin/aot/Project.aot$ext';
	}

	// Create Project.aot.csproj file for AOT compilation
	static function createAotProjectFile():Void {
		sys.io.File.saveContent("Project.aot.csproj", '<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
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
		deleteDirectoryRecursively("bin/cs");

		runCommand("dotnet", ["--version"]);

		// === Unit Tests ===
		runCommand("haxe", ["compile-cs.hxml"].concat(args));
		changeDirectory("bin/cs");

		// JIT test
		infoMsg("=== Running Unit Tests (JIT) ===");
		runCommand("dotnet", ["run"]);

		// AOT test
		infoMsg("=== Running Unit Tests (AOT) ===");
		runAotTest();

		changeDirectory(unitDir);

		Display.maybeRunDisplayTests(Cs);

		// === Misc Tests (Bootstrap) ===
		changeDirectory(miscCsDir);
		runCommand("haxe", ["run.hxml"]);

		// AOT for bootstrap - need to find the output directory
		infoMsg("=== Running Bootstrap Tests (AOT) ===");
		changeDirectory("projects/Bootstrap/bin");
		runAotTest();

		// === Sys Tests ===
		changeDirectory(sysDir);
		runCommand("haxe", ["compile-cs.hxml"].concat(args));

		// JIT
		infoMsg("=== Running Sys Tests (JIT) ===");
		runSysTest("dotnet", ["run", "--project", "bin/cs/Project.csproj"]);

		// AOT
		infoMsg("=== Running Sys Tests (AOT) ===");
		changeDirectory("bin/cs");
		runAotSysTest();

		// === Thread Tests ===
		changeDirectory(threadsDir);
		runCommand("haxe", ["build.hxml", "-cs", "export/cs"].concat(args));
		changeDirectory("export/cs");

		// JIT
		infoMsg("=== Running Thread Tests (JIT) ===");
		runCommand("dotnet", ["run"]);

		// AOT
		infoMsg("=== Running Thread Tests (AOT) ===");
		runAotTest();
	}
}
