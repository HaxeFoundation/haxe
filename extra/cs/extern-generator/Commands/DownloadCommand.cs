using NuGet.Common;
using NuGet.Configuration;
using NuGet.Packaging;
using NuGet.Protocol;
using NuGet.Protocol.Core.Types;
using NuGet.Versioning;

namespace ExternGenerator.Commands;

public class DownloadCommand
{
    public async Task ExecuteAsync(string packageName, string version, DirectoryInfo output)
    {
        Console.WriteLine($"Downloading {packageName} version {version}...");

        var logger = NullLogger.Instance;
        var cache = new SourceCacheContext();
        var repository = Repository.Factory.GetCoreV3("https://api.nuget.org/v3/index.json");
        var resource = await repository.GetResourceAsync<FindPackageByIdResource>();

        var packageVersion = new NuGetVersion(version);

        // Create output directory
        output.Create();
        var nupkgPath = Path.Combine(output.FullName, $"{packageName}.{version}.nupkg");

        // Download the package
        using (var packageStream = new FileStream(nupkgPath, FileMode.Create, FileAccess.Write))
        {
            var success = await resource.CopyNupkgToStreamAsync(
                packageName,
                packageVersion,
                packageStream,
                cache,
                logger,
                CancellationToken.None);

            if (!success)
            {
                Console.Error.WriteLine($"Failed to download package {packageName} version {version}");
                return;
            }
        }

        Console.WriteLine($"Downloaded to: {nupkgPath}");

        // Extract the package
        var extractDir = Path.Combine(output.FullName, $"{packageName}.{version}");
        if (Directory.Exists(extractDir))
        {
            Directory.Delete(extractDir, true);
        }

        using (var packageReader = new PackageArchiveReader(nupkgPath))
        {
            var files = await packageReader.GetFilesAsync(CancellationToken.None);

            foreach (var file in files)
            {
                var targetPath = Path.Combine(extractDir, file);
                var targetDir = Path.GetDirectoryName(targetPath);

                if (targetDir != null && !Directory.Exists(targetDir))
                {
                    Directory.CreateDirectory(targetDir);
                }

                using var entryStream = await packageReader.GetStreamAsync(file, CancellationToken.None);
                using var targetStream = File.Create(targetPath);
                await entryStream.CopyToAsync(targetStream);
            }
        }

        Console.WriteLine($"Extracted to: {extractDir}");

        // Find and report reference assemblies
        var refDir = Path.Combine(extractDir, "ref");
        if (Directory.Exists(refDir))
        {
            Console.WriteLine("\nReference assemblies found:");
            foreach (var framework in Directory.GetDirectories(refDir))
            {
                var frameworkName = Path.GetFileName(framework);
                var dlls = Directory.GetFiles(framework, "*.dll");
                var xmls = Directory.GetFiles(framework, "*.xml");
                Console.WriteLine($"  {frameworkName}: {dlls.Length} DLLs, {xmls.Length} XML docs");

                if (dlls.Length > 0)
                {
                    Console.WriteLine($"\n  Use with generate command:");
                    Console.WriteLine($"    --dll \"{dlls[0]}\"");
                    if (xmls.Length > 0)
                    {
                        Console.WriteLine($"    --xml \"{xmls[0]}\"");
                    }
                }
            }
        }

        // Clean up nupkg file
        File.Delete(nupkgPath);
    }
}
