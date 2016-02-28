(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: WiktionaryParse *)
(* :Context: WiktionaryParse` *)
(* :Author: darvin *)
(* :Date: 2016-02-28 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 darvin *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["WiktionaryData`WiktionaryParse`"]
(* Exported symbols added here with SymbolName::usage *)
ClearAll[WiktionaryParse, WkArticle, WkFrmt, WkTag, WkTemplate, WkText, WkSection, WkIntlink, WkExtlink];

WiktionaryParse::usage = "Parses";

WkArticle::usage = "Node that represents article";
WkFrmt::usage = "Node that represents format";
WkTag::usage = "Node that represents tag";
WkTemplate::usage = "Node that represents template";
WkText::usage = "Node that represents text";
WkSection::usage = "Node that represents section";
WkIntlink::usage = "Node that represents internal link";
WkExtlink::usage = "Node that represents external link";

Begin["`Private`"]



WiktionaryParse[xml_?(MatchQ[#,XMLObject["Document"][___]] &)]:=Module[{},
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

End[] (* `Private` *)

EndPackage[]