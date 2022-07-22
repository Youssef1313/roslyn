// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Microsoft.CodeAnalysis.Simplification;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CodeStyle
{
    internal abstract partial class AbstractBuiltInCodeStyleDiagnosticAnalyzer : DiagnosticAnalyzer, IBuiltInAnalyzer
    {
        protected readonly DiagnosticDescriptor Descriptor;

        /// <summary>
        /// Constructor for a code style analyzer with a single diagnostic descriptor.
        /// </summary>
        /// <param name="diagnosticId">Diagnostic ID reported by this analyzer</param>
        /// <param name="enforceOnBuild">Build enforcement recommendation for this analyzer</param>
        /// <param name="title">Title for the diagnostic descriptor</param>
        /// <param name="messageFormat">
        /// Message for the diagnostic descriptor.
        /// <see langword="null"/> if the message is identical to the title.
        /// </param>
        /// <param name="isUnnecessary"><see langword="true"/> if the diagnostic is reported on unnecessary code; otherwise, <see langword="false"/>.</param>
        /// <param name="configurable">Flag indicating if the reported diagnostics are configurable by the end users</param>
        protected AbstractBuiltInCodeStyleDiagnosticAnalyzer(
            string diagnosticId,
            EnforceOnBuild enforceOnBuild,
            LocalizableString title,
            LocalizableString? messageFormat = null,
            bool isUnnecessary = false,
            bool configurable = true)
        {
            Descriptor = CreateDescriptorWithId(diagnosticId, enforceOnBuild, title, messageFormat ?? title, isUnnecessary: isUnnecessary, isConfigurable: configurable);
            SupportedDiagnostics = ImmutableArray.Create(Descriptor);
            if (isUnnecessary)
            {
                AddDiagnosticIdToFadingOptionMapping(diagnosticId, FadingOption);
            }
        }

        /// <summary>
        /// This should only be called if the subtype has a descriptor with unnecessary tag.
        /// </summary>
        protected virtual PerLanguageOption2<bool>? FadingOption
        {
            get
            {
                Debug.Fail("Analyzers with unnecessary tag must explicitly override FadingOption and return an option or null.");
                return null;
            }
        }

        private static void AddDiagnosticIdToFadingOptionMapping(string diagnosticId, PerLanguageOption2<bool>? fadingOption)
        {
            if (fadingOption != null)
            {
                IDEDiagnosticIdToOptionMappingHelper.AddFadingOptionMapping(diagnosticId, fadingOption);
            }
        }

        /// <summary>
        /// Constructor for a code style analyzer with a multiple diagnostic descriptors.
        /// </summary>
        protected AbstractBuiltInCodeStyleDiagnosticAnalyzer(ImmutableArray<DiagnosticDescriptor> supportedDiagnostics)
        {
            SupportedDiagnostics = supportedDiagnostics;

            Descriptor = SupportedDiagnostics[0];

            foreach (var descriptor in supportedDiagnostics)
            {
                if (descriptor.ImmutableCustomTags().Contains(WellKnownDiagnosticTags.Unnecessary))
                {
                    AddDiagnosticIdToFadingOptionMapping(descriptor.Id, FadingOption);
                }
            }
        }

        public abstract DiagnosticAnalyzerCategory GetAnalyzerCategory();

        public virtual bool OpenFileOnly(SimplifierOptions? options)
            => false;

        public CodeActionRequestPriority RequestPriority => CodeActionRequestPriority.Normal;

        public sealed override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; }

        protected static DiagnosticDescriptor CreateDescriptorWithId(
            string id,
            EnforceOnBuild enforceOnBuild,
            LocalizableString title,
            LocalizableString? messageFormat = null,
            bool isUnnecessary = false,
            bool isConfigurable = true,
            LocalizableString? description = null)
#pragma warning disable RS0030 // Do not used banned APIs
            => new(
                    id, title, messageFormat ?? title,
                    DiagnosticCategory.Style,
                    DiagnosticSeverity.Hidden,
                    isEnabledByDefault: true,
                    description: description,
                    helpLinkUri: DiagnosticHelper.GetHelpLinkForDiagnosticId(id),
                    customTags: DiagnosticCustomTags.Create(isUnnecessary, isConfigurable, enforceOnBuild));
#pragma warning restore RS0030 // Do not used banned APIs

        /// <summary>
        /// Flag indicating whether or not analyzer should receive analysis callbacks for generated code.
        /// By default, code style analyzers should not run on generated code, so the value is false.
        /// </summary>
        protected virtual bool ReceiveAnalysisCallbacksForGeneratedCode => false;

        public sealed override void Initialize(AnalysisContext context)
        {
            var flags = ReceiveAnalysisCallbacksForGeneratedCode ? GeneratedCodeAnalysisFlags.Analyze : GeneratedCodeAnalysisFlags.None;
            context.ConfigureGeneratedCodeAnalysis(flags);
            context.EnableConcurrentExecution();

            InitializeWorker(new(context));
        }

        protected abstract void InitializeWorker(IDEAnalysisContext context);
    }

}
