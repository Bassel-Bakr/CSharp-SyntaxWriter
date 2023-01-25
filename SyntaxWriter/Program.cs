using System;
using System.IO;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.CSharp;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.Build.Locator;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Configuration;
using Microsoft.CodeAnalysis.FindSymbols;
using SyntaxWriter.Writers;

namespace SyntaxWriter
{
    public class Program
    {
        private static string SOLUTION_PATH = ConfigurationManager.AppSettings["Solution.Path"];

        public static void Main(string[] args)
        {
            if (!MSBuildLocator.IsRegistered)
            {
                var instances = MSBuildLocator.QueryVisualStudioInstances().ToArray();
                MSBuildLocator.RegisterInstance(instances.OrderByDescending(x => x.Version).First());
            }

            var workspace = MSBuildWorkspace.Create();
            workspace.SkipUnrecognizedProjects = true;
            workspace.WorkspaceFailed += (sender, diagnosticArgs) =>
            {
                if (diagnosticArgs.Diagnostic.Kind == WorkspaceDiagnosticKind.Failure)
                {
                    Console.Error.WriteLine(diagnosticArgs.Diagnostic.Message);
                }
            };

            ParseSolution(workspace);

            Console.WriteLine("Done");

            Console.ReadKey();
            Console.ReadKey();
        }

        private static void ParseSolution(MSBuildWorkspace workspace)
        {
            var solution = workspace.OpenSolutionAsync(SOLUTION_PATH,
                new ProgressBarProjectLoadStatus()).Result;

            var compilations = solution.Projects
                .Select(project => project.GetCompilationAsync().Result);

            compilations.ToList().ForEach(compilation =>
            {
                ParseProjectCompilation(solution, compilation);
            });
        }

        private static void ParseProjectCompilation(Solution solution, Compilation compilation)
        {
            var syntaxTrees = compilation.SyntaxTrees
                .ToList();


            var updatedSyntaxTrees = syntaxTrees.Select(syntaxTree =>
            {
                Console.WriteLine($"{syntaxTrees.IndexOf(syntaxTree)}/{syntaxTrees.Count}: {syntaxTree.FilePath}");
                Console.Out.FlushAsync();
                var syntaxWriter = new UnusedMethodRemover(compilation, solution, syntaxTree);
                var newRoot = syntaxWriter.Visit(syntaxTree.GetRoot());
                var newSyntaxTree = syntaxTree.WithRootAndOptions(newRoot, syntaxTree.Options);
                return new { Old = syntaxTree, New = newSyntaxTree };
            });

            //return; // Don't overwrite

            // Skip unmodified trees
            updatedSyntaxTrees
                .Where(args => !args.Old.IsEquivalentTo(args.New))
                .Select(args => args.New)
                .ToList()
                .ForEach(syntaxTree =>
                {
                    using (var fileWriter = new StreamWriter(syntaxTree.FilePath))
                    {
                        syntaxTree.GetRoot().WriteTo(fileWriter);
                    }
                });
        }
    }

    public class ProgressBarProjectLoadStatus : IProgress<ProjectLoadProgress>
    {
        public void Report(ProjectLoadProgress progress)
        {
            Console.WriteLine($"{progress.Operation} {progress.FilePath} {progress.ElapsedTime} {progress.TargetFramework}");
        }
    }
}
