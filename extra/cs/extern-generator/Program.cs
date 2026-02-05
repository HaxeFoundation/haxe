using System.CommandLine;
using ExternGenerator.Commands;

var rootCommand = new RootCommand("Haxe Extern Generator - Generate Haxe externs from .NET assemblies");

// Generate command
var generateCommand = new Command("generate", "Generate Haxe externs from a .NET assembly");

var dllOption = new Option<FileInfo>(
    name: "--dll",
    description: "Path to the .NET assembly (.dll)")
{ IsRequired = true };

var xmlOption = new Option<FileInfo?>(
    name: "--xml",
    description: "Path to the XML documentation file");

var outputOption = new Option<DirectoryInfo>(
    name: "--output",
    description: "Output directory for generated .hx files")
{ IsRequired = true };

var namespaceOption = new Option<string[]>(
    name: "--namespace",
    description: "Filter to specific namespace(s)")
{ AllowMultipleArgumentsPerToken = true };

var packageOption = new Option<string?>(
    name: "--package",
    description: "Haxe package prefix (default: derives from namespace)");

var configOption = new Option<FileInfo?>(
    name: "--config",
    description: "JSON configuration file");

var recursiveOption = new Option<bool>(
    name: "--recursive",
    description: "Include nested namespaces",
    getDefaultValue: () => true);

var overwriteOption = new Option<bool>(
    name: "--overwrite",
    description: "Overwrite existing files");

generateCommand.AddOption(dllOption);
generateCommand.AddOption(xmlOption);
generateCommand.AddOption(outputOption);
generateCommand.AddOption(namespaceOption);
generateCommand.AddOption(packageOption);
generateCommand.AddOption(configOption);
generateCommand.AddOption(recursiveOption);
generateCommand.AddOption(overwriteOption);

generateCommand.SetHandler(async (dll, xml, output, namespaces, package, config, recursive, overwrite) =>
{
    var cmd = new GenerateCommand();
    await cmd.ExecuteAsync(dll, xml, output, namespaces, package, config, recursive, overwrite);
}, dllOption, xmlOption, outputOption, namespaceOption, packageOption, configOption, recursiveOption, overwriteOption);

rootCommand.AddCommand(generateCommand);

// Download command
var downloadCommand = new Command("download", "Download .NET reference assemblies from NuGet");

var packageNameOption = new Option<string>(
    name: "--package",
    description: "NuGet package name (e.g., NETStandard.Library.Ref)")
{ IsRequired = true };

var versionOption = new Option<string>(
    name: "--version",
    description: "Package version (e.g., 2.1.0)")
{ IsRequired = true };

var downloadOutputOption = new Option<DirectoryInfo>(
    name: "--output",
    description: "Download location")
{ IsRequired = true };

downloadCommand.AddOption(packageNameOption);
downloadCommand.AddOption(versionOption);
downloadCommand.AddOption(downloadOutputOption);

downloadCommand.SetHandler(async (packageName, version, output) =>
{
    var cmd = new DownloadCommand();
    await cmd.ExecuteAsync(packageName, version, output);
}, packageNameOption, versionOption, downloadOutputOption);

rootCommand.AddCommand(downloadCommand);

return await rootCommand.InvokeAsync(args);
