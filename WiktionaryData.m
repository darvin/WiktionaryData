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

BeginPackage["WiktionaryData`"]
(* Exported symbols added here with SymbolName::usage *)
WiktionaryData::usage = "Fetches Wiktionary data"

Begin["`Private`"]


<< MongoDBLink`
wikiDumpGetCollection[] := Module[{conn, db},
  conn = OpenConnection[];
  db = GetDatabase[conn, "wikiDump"];
  {conn, GetCollection[db, "wikiDump"]}
];

wikiDumpFetchPage[request_] :=
    Module[{page, collection, connection}, {connection, collection} =
        wikiDumpGetCollection[];
    pages =
        FindDocuments[collection, request];
    CloseConnection[connection];
    First[pages]
    ];

wiktionary[property_,arg_]:= Module[{request, result},
  request = Which[
    MemberQ[arg, "PageID"->_], {"_id" -> {"$eq" -> "PageID"/.arg}},
    MemberQ[arg, "Title"->_], {"pageTitle" -> {"$eq" -> "Title"/.arg}}
  ];
  result = wikiDumpFetchPage[request];
  Switch[property,
    "Title", Lookup[result, "pageTitle", result],
    "PageID", Lookup[result, "_id", result],
    "ArticleWikicode", Lookup[result, "wikiText", result],
    "ArticleRawXML", Lookup[result, "parsedWikiText", result],
    "Article", XML`Parser`XMLGetString[Lookup[result, "parsedWikiText", result], "IncludeNamespaces" -> "Unparsed"]
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

End[] (* `Private` *)

EndPackage[]