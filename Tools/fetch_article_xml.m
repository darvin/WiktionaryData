#!/usr/local/bin/WolframScript -script
ClearAll[WiktionaryData]
<< WiktionaryData`

pageTitle = $ScriptCommandLine[[2]];
outputFile = $ScriptCommandLine[[3]];

Print["Reading ", pageTitle, " and writing to ", outputFile];
rawXML = WiktionaryData[pageTitle, "ArticleRawXML"];
WriteString[outputFile, rawXML];