xquery version "1.0-ml";

(: 
 : This module provides functions to construct metadata from the MARCXML 
 : record downloaded from the Internet Archive website for Princeton Seminary 
 : digitized resources. They also insert the full MARCXML record into the metadata 
 : element for future use.
 :)
 
module namespace marc = "http://digital.library.ptsem.edu/ia/marc";

import module namespace wc = "http://digital.library.ptsem.edu/ia/ia-worldcat" at "ia-worldcat.xqy";

import module namespace class = "http://digital.library.ptsem.edu/ia/class" at "class.xqy";

import module namespace ia-meta = "http://digital.library.ptsem.edu/ia/ia-meta" at "ia-meta.xqy";

import module namespace marc-lang = "http://digital.library.ptsem.edu/ia/admin/marc-languages" at "marc-languages.xqy";

declare namespace m = "http://www.loc.gov/MARC21/slim";
declare namespace codelist = "info:lc/xmlns/codelist-v1";
declare namespace ia = "http://digital.library.ptsem.edu/ia";

(: This is the main access function of the module. It constructs the metadata for the IA application including the full MARC record from the Internet Archive website. For the title element, the regular expression removes most ending punctuation except dash, bracket, parenthesis, quote or connector. :)

declare function marc:build-metadata($id as xs:string?, $error-rate as element(ia:errorRate)?, $marc as element(m:record)?, $ia-meta as node()*)
as element(ia:metadata)
{
  let $name := marc:get-name($marc)
  let $date := get-date($marc)
  let $call-num := marc:get-call-number($marc)
  return
    <metadata xmlns="http://digital.library.ptsem.edu/ia">
      <id>{ $id }</id>
      { if ($name) then <name>{ $name }</name> else () }
      <title>{ fn:normalize-space(fn:replace(create-brief-title($marc),"(\s*\p{Po}+\s*)$", "")) }</title>
      <sortTitle first-letter="{ create-first-letter($marc) }">{ build-sort-title($marc) }</sortTitle>
      { get-uniform-title($marc) }
      { get-edition($marc) }
      { if (fn:empty($date)) then () else <date>{ $date }</date> }
      { get-volume-info($marc, $id) }
      { get-language($marc) }
      { $error-rate }
      { get-format($marc) }
      { $call-num }
      <class>{ class:map-class(normalize-space($call-num)) }</class>
      { marc:get-topics($marc) }
      <contributor>{ ia-meta:get-contributor($ia-meta) }</contributor>
      { ia-meta:get-sponsor($ia-meta) }
      <marc>{ $marc }</marc>
    </metadata>  
};

(:
 : This function extracts the name value from the MARC record. It eliminates six
 : subfields commonly found in the title portion of name/title headings
 : including the title (subfield t) itself. Removes square brackets surrounding
 : the name, if present. Removes the phrase "[from old catalog]" if present.
 : Normalizes whitespace.
 :)
declare function marc:get-name($marc as node()?)
as xs:string?
{
  let $subfields := $marc/m:datafield[@tag = ("100", "110", "111", "700", "710", "711")][1]/(* except m:subfield[@code = ("f", "k", "l", "p", "s", "t", "6")])
  let $name := fn:string-join(fn:data($subfields), " ")
  let $regex1 := "^\[([^\]]+)\]"
  let $regex2 := "\[from old catalog\]"
  let $temp1 :=
    (: find "[something other than closing square bracket]" and remove the square brackets :)
    if (fn:matches($name, $regex1)) then
      fn:replace($name, $regex1, "$1")
    else $name
  let $temp2 :=
    (: find "[from old catalog]" and remove it altogether :)
    if (fn:matches($temp1, $regex2, "i")) then
      fn:replace($temp1, $regex2, "", "i")
    else $temp1
  (: normalize whitespace :)
  let $fixedNameString := fn:normalize-space($temp2)
  where $subfields
  return marc:normalize-name($fixedNameString)
};

(: This function removes the final period or the final comma from names if present. :)
declare function marc:normalize-name($fixedNameString as xs:string?)
as xs:string?
{

    if (fn:matches($fixedNameString, "[a-zA-Z]\.\s*$")) then   (: keeps final period for names with initials, titles, etc. :)
          $fixedNameString
    else if (fn:matches($fixedNameString, "(\.|,)\s*$")) then
               fn:replace($fixedNameString, "(\.|,)\s*$", "")
         else $fixedNameString
};

(: This function extracts the title value from the MARC record. :)

declare function marc:get-title($marc as node()*) {


let $title := $marc/m:datafield[@tag = "245"]
return $title

};

(: This function takes the title value and eliminates the statement of responsibility (subfield c). :)

declare function marc:create-brief-title($marc as node()*) 
{

let $title := get-title($marc)
let $briefTitle := $title/(* except m:subfield[@code = ("c", "6")])
return fn:string-join(fn:data($briefTitle), " ")

};

(: This function takes the brief title value and creates a normalized title for sorting purposes. It is used for the browse title feature of the Theological Commons application. :)

declare function marc:build-sort-title($marc as node()*) 
{

let $title := get-title($marc)
let $sortTitle := let $ind := $title/@ind2
                  return if ($ind > 0) then 
                  
                    fn:replace(fn:substring(fn:normalize-space(create-brief-title($marc)), fn:data($ind) + 1), "^\W+(.+)$", "$1")                          
                         else fn:replace(fn:normalize-space(create-brief-title($marc)), "^\W+(.+)$", "$1")
return $sortTitle

};

(: This function takes the first letter of the sort title value and creates an attribute value for the browse title feature of the IA application. :)

declare function marc:create-first-letter($marc as node()*)
{

let $sortTitle := build-sort-title($marc)
let $normTitle := fn:normalize-unicode($sortTitle, "NFKD")
let $letter := fn:upper-case(fn:substring($normTitle, 1, 1))

return $letter
};



(: This function extracts the date value from the MARC record using the character position for the first 4-digit date in the 008 fixed field. :)

declare function marc:get-date($marc as node()*) 
{

let $date := fn:substring($marc/m:controlfield[@tag = "008"], 8, 4)
return if ($date castable as xs:gYear)
       then $date
       else ()

};


(:~
 : Extracts the language code from MARC 008 and returns a <language> element
 : indicating both the language code and its corresponding human-readable name.
 :)
declare function marc:get-language($marc as element(m:record)?)
as element(ia:language)?
{
  let $common := "eng|fre|ger|lat|spa|dut|ita"
  
  (: get all MARC language codes from LC list, excluding the most common ones for now :)
  let $codelist := $marc-lang:codelist
  let $codes :=
    for $code-element in $codelist/codelist:languages/codelist:language/codelist:code
    let $value := fn:normalize-space($code-element)
    return
      if (fn:matches($value, fn:concat("(", $common, ")"))) then ()
      else $value
  
  (: make a regular expression containing those codes, but put the most common languages first for better matching accuracy :)
  let $joined := fn:string-join($codes, "|")
  let $regex := fn:concat("^.+(", $common, "|", $joined, ").*$")
  
  (: use that regular expression to extract language code from MARC 008 -- which is supposed to be a fixed-length value, but in MARCXML if the whitespace in the value has been normalized the language code might not occur at the expected character position :)
  let $controlfield := $marc/m:controlfield[@tag = "008"]
  let $controlfield := fn:replace($controlfield, "\|+", "") (: remove pipe characters, if any, because they interfere with regular expression matching :)
  let $code :=
    if (fn:matches($controlfield, $regex)) then
      fn:replace($controlfield, $regex, "$1")
    else ()
  
  (: get corresponding human-readable name :)
  let $name := marc-lang:get-language-name($code)
  
  (: return <language> element :)
  return
    if ($code = "zxx" or $code = "und") then ()  (: exclude <language> element for zxx = "No linguistic content" and und = "Undetermined" :)
    else if ($code and $name) then
      <language xmlns="http://digital.library.ptsem.edu/ia">
        {(attribute code {$code}, $name)}
      </language>
    else ()
};

(:~
 : This function extracts the Library of Congress call number value 
 : from the MARC record (or obtains it from the OCLC WorldCat database). 
 : Because the LC call number fields are repeatable, the function extracts 
 : the first one. The return value is a <callNumber> element.
 :)

declare function marc:get-call-number($marc as node()*)
as element(ia:callNumber)?
{
  let $call-num := ($marc/m:datafield[@tag=("050", "055", "090")])[1]
  return
    if ($marc/m:datafield[@tag = "090"][@ind2 = ("8", "9")]) then 
    <callNumber source="oclc" xmlns="http://digital.library.ptsem.edu/ia">
      { fn:normalize-space(wc:find-call-number($marc)) }
    </callNumber>
  
    else if ($call-num and $call-num != "") then
         <callNumber source="voyager" xmlns="http://digital.library.ptsem.edu/ia">
           { fn:normalize-space($call-num) }
         </callNumber>
       
         else if (fn:not($call-num) or $call-num = "") then 
                <callNumber source="oclc" xmlns="http://digital.library.ptsem.edu/ia">
                  { fn:normalize-space(wc:find-call-number($marc)) }
                </callNumber>
              else ()
};

(:~
 : Constructs and returns a <volumeInfo> element, if appropriate based on the
 : MARC record, using part of the IA identifier for the <volume> value.
 :)
declare function marc:get-volume-info($marc as element(m:record)?, $id as xs:string)
as element(ia:volumeInfo)?
{
  let $volumeInfo := $marc/m:datafield[@tag = "300"]/m:subfield[@code = "a"]
  let $regex := "^(.*)(\d{2})\D+$"
  return
    if (
        fn:not(fn:matches($volumeInfo, "^1 v."))
        and fn:not(fn:matches($volumeInfo, "^1 vol."))
        and fn:not(fn:contains($volumeInfo, "in 1"))
        and fn:contains($volumeInfo, "v.")
        or fn:contains($volumeInfo, "vol.")
    ) then
      let $idNum := fn:replace($id, $regex, "$2")
      let $volumeValue :=
        if ($idNum = "00") then
          "1"
        else if (fn:starts-with($idNum, "0")) then
          fn:substring($idNum, 2)
        else
          $idNum
      let $volumeInteger :=
        if ($idNum castable as xs:positiveInteger) then
          attribute integer {fn:string(xs:positiveInteger($idNum))}
        else ()
      let $volumeInfoId :=
        if (fn:matches($id, $regex)) then
          attribute id { fn:replace($id, $regex, "$1") }
        else ()
      return
        <volumeInfo xmlns="http://digital.library.ptsem.edu/ia">
          { $volumeInfoId }
          <volume>{ ($volumeInteger, $volumeValue) }</volume>
        </volumeInfo>
    else ()
};

(:~
 : Constructs and returns an <edition> element, if the MARC record contains an
 : edition statement.
 :)
declare function marc:get-edition($marc as element(m:record)?)
as element(ia:edition)?
{
let $edition := $marc/m:datafield[@tag = "250"]
return
  if ($edition) then
    <edition xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space($edition) }</edition>
  else ()
};

(:~
 : Constructs and returns a <uniformTitle> element, if the MARC record contains
 : a uniform title.
 :)
declare function marc:get-uniform-title($marc as element(m:record)?)
as element(ia:uniformTitle)?
{
let $uniformTitle := $marc/m:datafield[@tag = ("130", "240")]/m:subfield[@code = ("a", "p")]
let $value := fn:normalize-space( fn:string-join($uniformTitle, " ") )
return
  if ($uniformTitle) then
    <uniformTitle xmlns="http://digital.library.ptsem.edu/ia">{ $value }</uniformTitle>
  else ()
};

(:~ Constructs and returns a <topics> element based on MARC subject fields.
 :
 : The goal here is to derive values from the MARC subject fields for grouping
 : books into topics for a "More like this" feature. Values should be neither
 : too broad nor too specific. Using subfield a combined with other selected
 : subfields, including the first x (general subdivision), seems to work well.
 : Examples:
    <datafield tag="650" ind1=" " ind2="0">
      <subfield code="a">Protestants</subfield>
      <subfield code="z">England</subfield>
      <subfield code="x">Spiritual life</subfield>
    </datafield>
 : becomes "Protestants|Spiritual life", where subfield a alone would be too
 : broad but including z would be too specific.
    <datafield tag="630" ind1="0" ind2="0">
      <subfield code="a">Bible.</subfield>
      <subfield code="p">N.T.</subfield>
      <subfield code="p">Romans</subfield>
      <subfield code="x">Commentaries.</subfield>
    </datafield>
 : becomes "Bible|N.T.|Romans|Commentaries"
    <datafield tag="610" ind1="2" ind2="0">
      <subfield code="a">Catholic Church</subfield>
      <subfield code="x">Doctrinal and controversial works</subfield>
      <subfield code="x">Protestant authors.</subfield>
    </datafield>
 : becomes "Catholic Church|Doctrinal and controversial works"
    <datafield tag="600" ind1="0" ind2="0">
      <subfield code="a">Jesus Christ</subfield>
      <subfield code="v">Biography.</subfield>
    </datafield>
 : becomes "Jesus Christ|Biography"
 :)
declare function marc:get-topics($marc as element(m:record)?)
as element(ia:topics)?
{
  (: get 600-level fields :)
  let $subject-fields := $marc/m:datafield[fn:matches(@tag, "^6\d\d$")]
  let $topics :=
    for $field in $subject-fields
    let $tag := fn:string($field/@tag)
    (: grab value of subfield a along with other subfields we consider significant/useful for this purpose :)
    let $a := $field/m:subfield[@code="a"]       (: main :)
    let $p := $field/m:subfield[@code="p"]       (: part/section :)
    let $v := $field/m:subfield[@code="v"]       (: form subdivision :)
    let $x := ($field/m:subfield[@code="x"])[1]  (: first general subdivision :)
    let $additional-subfields :=
      if (fn:matches($tag, "(600|610|611|662)")) then
        (: personal, corporate, meeting, or place name; get additional name parts/subdivisions :)
        let $b := $field/m:subfield[@code="b"]
        let $c := $field/m:subfield[@code="c"]
        let $d := $field/m:subfield[@code="d"]
        return ($b, $c, $d)
      else ()
    let $all-subfields := ($a, $additional-subfields, $p, $v, $x)
    let $all-values :=
      for $subfield in $all-subfields
      (: convert to string; also remove "[from old catalog]" if present :)
      let $subfield := fn:normalize-space(fn:replace($subfield, "\[from old catalog\]", "", "i"))
      return
        if (fn:matches($subfield, "\W\w\.$")) then
          (: final period looks like an abbreviation, as in "N.T." or "Smith, John Q.", so don't remove final period :)
          $subfield
        else if (fn:matches($subfield, "\setc\.$")) then
          (: don't remove final period for "etc." :)
          $subfield
        else
          (: remove final period or comma, if any :)
          fn:replace($subfield, "^(.+)(\.|,)$", "$1")
    let $value := fn:string-join($all-values, "|")
    where fn:not(fn:matches($tag, "^(653|654|69)"))  (: exclude 653 (uncontrolled), 654 (faceted topical), and 69X (local) :)
    return <topic xmlns="http://digital.library.ptsem.edu/ia">{$value}</topic>
  return
    if ($topics) then
      <topics xmlns="http://digital.library.ptsem.edu/ia">
        {$topics}
      </topics>
    else ()
};

(:~ Returns a <format> element indicating the format (book, thesis, etc.) of the physical item. :)
declare function marc:get-format($marc as element(m:record)?)
as element(ia:format)
{
  let $leader := $marc/m:leader
  let $type := fn:substring($leader, 7, 1)   (: MARC spec says "Type of record" is column 6, but that's zero-based, so get character 7 :)
  let $level := fn:substring($leader, 8, 1)  (: MARC spec says "Bibliographic level" is column 7, but that's zero-based, so get character 8 :)
  let $value :=
    if ($marc/m:datafield[fn:matches(@tag, "502")]) then "Thesis"  (: MARC 502 = dissertation note :)
    else if ($level = "s") then "Periodical"  (: "s" = serial :)
    else if ($type = "t") then "Manuscript"   (: "t" = manuscript :)
    else "Book"
  return <format xmlns="http://digital.library.ptsem.edu/ia">{$value}</format>
};

(:~
 : Takes a MARC record and returns a sequence of strings, each indicating a
 : Library collection to which the item belongs.
 :)
declare function marc:get-collections($marc as element(m:record)?)
as xs:string*
{
  for $datafield in $marc/m:datafield[@tag = "730"]
  return fn:normalize-space($datafield)
};
