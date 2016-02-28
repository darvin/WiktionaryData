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
WiktionaryData::usage = "Fetches Wiktionary data";


WkArticle::usage = "Node that represents article";
WkFrmt::usage = "Node that represents format";
WkTag::usage = "Node that represents tag";
WkTemplate::usage = "Node that represents template";
WkText::usage = "Node that represents text";
WkSection::usage = "Node that represents section";
WkIntlink::usage = "Node that represents internal link";
WkExtlink::usage = "Node that represents external link";

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
  parsedRaw[r_] := Lookup[r, "parsedWikiText", r];
  parsedXML[r_] := XML`Parser`XMLGetString[parsedRaw[r], "IncludeNamespaces" -> "Unparsed"];
  Switch[property,
    "Title", Lookup[result, "pageTitle", result],
    "PageID", Lookup[result, "_id", result],
    "ArticleWikicode", Lookup[result, "wikiText", result],
    "ArticleRawXML", parsedRaw[result],
    "ArticleXML", parsedXML[result],
    "Article", wiktionaryParseXMLArticle[parsedXML[result]]
  ]
]

wiktionaryParseXMLArticle[xml_]:=Module[{},
  Block[{XMLObject, XMLElement}, XMLObject["Document"] = #2 &;

  XMLElement["org.sweble.wikitext.engine.nodes.EngPage", attr_,
    data_] := WkArticle[data];
  XMLElement["section", __, {___,
    XMLElement["level", __, {level_}],
    ___,
    XMLElement["heading", __, {
      ___,
      WkText[heading_],
      ___}],
    ___,
    XMLElement["body", __, data_],
    ___}] :=
      WkSection[{"Level" -> level,
        "Title" -> heading}, {Sequence @@ data}];


  XMLElement["arg", {}, {
    XMLElement["name", {}, {name_}],
    XMLElement["value", {}, value_]
  }] := name -> value;
  XMLElement["arg", {}, {
    XMLElement["value", {}, value_]
  }] := value;


  XMLElement[
    "template", {}, {XMLElement["name", {}, {name_: Null}],
    XMLElement["args", {},
      args__
    ]}] := WkTemplate[name, args];

  XMLElement[
    "intlink", __, {___, XMLElement["target", __, {data_}], ___}] :=
      WkIntlink[data];

  XMLElement[
    "extlink", __, {XMLElement[
    "target", __, {XMLElement["protocol", {}, {protocol_}],
      XMLElement["path", {}, {path_}]}],
    XMLElement["title", {}, {title_}] | ___}] :=
      WkExtlink[protocol <> ":" <> path, title];



  XMLElement["text", attr_, {data_}] := WkText[data];
  XMLElement[
    elem_?(MatchQ[#,
      "p" | "ul" | "li" | "ol" | "dd" | "dl" | "i" | "b"] &), attr_,
    data_] := WkFrmt[elem, data];

  XMLElement[
    "elem", {}, {XMLElement["name", {}, {elemName_}],
    XMLElement["xmlAttributes", {}, attr_],
    XMLElement["body", {}, {body_}]}] := WkTag[elemName, body];
  XMLElement[
    "tagext", __, {XMLElement["name", __, {elemName_}],
    XMLElement["xmlAttributes", __, __]}] := ## &[];
  XMLElement[
    "tagext", __, {XMLElement["name", {}, {elemName_}],
    XMLElement["xmlAttributes", __, __],
    XMLElement["body", __, {XMLElement["content", __, data_]}]}] :=
      WkTag[elemName, body];

  XMLElement["rtd", attr_, data_] := ## &[];
  XMLElement["eref", attr_, data_] := ## &[]; (*fixme*)

  XMLElement["hr", attr_, data_] := ## &[];
  XMLElement["precededByNewline", attr_, data_] := ## &[];
  XMLElement[elem_, {"ptk:location" -> __}, data_] :=
      XMLElement[elem, {}, data];
  XMLElement["text", attr_, {}] := ## &[];
  res = xml;
  ];
  res
];

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

WiktionaryData[xml_?(MatchQ[#,XMLObject["Document"][___]] &), "Article"] := wiktionaryParseXMLArticle[xml];

End[] (* `Private` *)

EndPackage[]