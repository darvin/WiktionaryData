(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: WiktionaryData *)
(* :Context: WiktionaryData` *)
(* :Author: darvin *)
(* :Date: 2016-02-24 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 darvin *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["WiktionaryData`", {"WiktionaryData`WiktionaryParse`", "MongoDBLink`"}]
ClearAll[WiktionaryData];
WiktionaryData::usage = "Fetches Wiktionary data";



Begin["`Private`"]



connection = OpenConnection[];

wikiDumpFetchPage[request_] :=
    Module[{page, collection, db},
      db = GetDatabase[connection, "wikiDump"];
    collection = GetCollection[db, "wikiDump"];
    pages = FindDocuments[collection, request];
    First[pages]
    ];

wiktionary[property_,arg_]:= Module[{request, result},
  request = Which[
    MemberQ[arg, "PageID"->_], {"_id" -> {"$eq" -> "PageID"/.arg}},
    MemberQ[arg, "Title"->_], {"pageTitle" -> {"$eq" -> "Title"/.arg}}
  ];
  result = wikiDumpFetchPage[request];
  parsedRaw[r_] := Lookup[r, "parsedWikiText", r];
  parsedXML[r_] := XML`Parser`XMLGetString[parsedRaw[r], "IncludeNamespaces" -> "Unparsed"];
  Switch[property,
    "Title", Lookup[result, "pageTitle", result],
    "PageID", Lookup[result, "_id", result],
    "ArticleWikicode", Lookup[result, "wikiText", result],
    "ArticleRawXML", parsedRaw[result],
    "ArticleXML", parsedXML[result],
    "Article", WiktionaryParse[parsedXML[result]]
  ]
]


WiktionaryData[arg_, property_] :=
    Module[{result, title, pageid}, title = arg;
    pageid =
        If[MatchQ[arg, "PageID" -> _], "PageID" /. arg,
          Missing["NotAvailable"]];
    Which[MatchQ[pageid, _Missing],
      result =
          WiktionaryData`Private`wiktionary[
            property, {"Title" -> title}], !
        MatchQ[pageid, _Missing],
      result =
          WiktionaryData`Private`wiktionary[
            property, {"PageID" -> pageid}], True,
      result = Missing["BadInput"]];
    result];

WiktionaryData[xml_?(MatchQ[#,XMLObject["Document"][___]] &), "Article"] := WiktionaryParse[xml];

End[] (* `Private` *)

EndPackage[]