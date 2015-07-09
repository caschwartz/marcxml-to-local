xquery version "1.0-ml";

(: 
 : This module provides functions to construct theocom metadata 
 : from MARCXML records that were extracted from the Voyager library 
 : catalog for the Educational Media AV resources. It also insert the 
 : full MARCXML record into the <ia:metadata> element for future use.
 :)
 
module namespace av = "http://digital.library.ptsem.edu/ia/av-marc";

import module namespace marc = "http://digital.library.ptsem.edu/ia/marc" at "marc.xqy"; 
import module namespace wc = "http://digital.library.ptsem.edu/ia/ia-worldcat" at "ia-worldcat.xqy";
import module namespace class = "http://digital.library.ptsem.edu/ia/class" at "class.xqy";

declare namespace m = "http://www.loc.gov/MARC21/slim";
declare namespace ia = "http://digital.library.ptsem.edu/ia";


(: 
 : This function adds theocom XML documents to the ed-media-marc
 : database in the theocom-xml directory, a temporary location 
 : before loading into the theocom3 database. 
 :)

declare function av:add-av-theocom-documents()
as empty-sequence()
{
  for $doc in xdmp:directory("/MARCXML/")
  let $marc := $doc/m:record
  let $rootNode := av:build-av-theocom-document($marc)
  let $uri := fn:concat("/theocom-xml/", av:get-av-identifier($marc), ".xml")
  return xdmp:document-insert($uri, $rootNode)
};

(: 
 : This is the main function of the module. It constructs
 : the theocom document for the Theological Commons application 
 : including the full MARC record. For the title element, the regular 
 : expression removes most ending punctuation except dash, bracket, 
 : parenthesis, quote or connector. The contributor element has the 
 : Theological Commons default value for Princeton Seminary. 
 :)

declare function av:build-av-theocom-document($marc as element(m:record)?)
as element(ia:doc)
{
  let $name := marc:get-name($marc)
  let $date := av:get-av-date($marc)
  let $call-num := marc:get-call-number($marc)
  return
    <doc xmlns="http://digital.library.ptsem.edu/ia">
      <metadata>
        { av:get-av-identifier($marc) }
        { if ($name) then <name>{ $name }</name> else () }
        <title>
          { fn:normalize-space(fn:replace(av:create-brief-av-title($marc),"(\s*\p{Po}+\s*)$", "")) }
        </title>
        <sortTitle first-letter="{ marc:create-first-letter($marc) }">
          { av:build-sort-av-title($marc) }
        </sortTitle>
        { marc:get-uniform-title($marc) }
        { marc:get-edition($marc) }
  	    { av:get-duration($marc) }
  	    { av:get-recording-date($marc) }
        { av:get-av-series($marc) }
  	    { av:get-av-series-from-note($marc) }
        { if (fn:empty($date)) then () else <date>{ $date }</date> }
        { marc:get-language($marc) }
        { av:get-av-format($marc) }
        { av:get-av-notes($marc) }
        { $call-num }
        <class>{ class:map-class(normalize-space($call-num)) }</class>
        { av:get-av-genre($marc) }
        { av:revise-av-topics($marc) }
        <contributor>Princeton Theological Seminary Library</contributor>
        <marc>{ $marc }</marc>
      </metadata>
    </doc>
};

(: This function takes the title value and eliminates the statement of responsibility (subfield c) and the  general material designation (subfield h). It joins the main title and subtitle strings with the standard cataloging punctuation, ISBD punctuation, used in MARC records. :)

declare function av:create-brief-av-title($marc as node()*)
as xs:string?
{

let $title := marc:get-title($marc)
let $briefTitle := $title/(* except m:subfield[@code = ("c", "h", "6")])
where $briefTitle (: Needed for Ed. Media spreadsheet metadata that lack titles :)
return fn:string-join(fn:data($briefTitle), " : ")

};

(: This function takes the brief title value and creates a normalized title for sorting purposes. It is used for the A-Z browse title feature of the Theological Commons application. :)

declare function av:build-sort-av-title($marc as node()*)
as xs:string?
{

let $title := marc:get-title($marc)
let $sortTitle := let $ind := $title/@ind2
                  let $titleString := fn:normalize-space(av:create-brief-av-title($marc))
                  return if ($ind > 0) then 
                              fn:replace(fn:substring($titleString, fn:data($ind) + 1), "^\W+(.+)$", "$1")                          
                         else fn:replace($titleString, "^\W+(.+)$", "$1")
where $sortTitle
return $sortTitle

};

(:~ Returns a <format> element indicating the AV format of the item. The general material designation (subfield h) contains the AV format. :)
declare function av:get-av-format($marc as element(m:record)?)
as element(ia:format)
{
  let $avFormat := $marc/m:datafield[@tag = "245"]/m:subfield[@code = "h"]
  let $value :=
    if (fn:matches($avFormat, "videorecording", "i")) then "Video"  
    else if (fn:matches($avFormat, "sound recording", "i")) then "Audio"
         else ()
  return <format xmlns="http://digital.library.ptsem.edu/ia">{ $value }</format>
};

(:~ Returns an <id> element with a unique identifier for the AV item. Unique identifiers were created for the MARC records extracted from the  Voyager library catalog for the filenames. The Ed. Media audio spreadsheet metadata was handled differently, and as a result, the 028 field was a more efficient way to work with and create a unique identifier. :)
declare function av:get-av-identifier($marc as element(m:record)?)
as element(ia:id)?
{
  (: $newID is for the new MARCXML records derived from the Ed. Media audio spreadsheet :)
  let $newID := ($marc/m:datafield[@tag = "028"]/m:subfield[@code = "a"])[1]
  (: $id is for the MARCXML records extracted from the Voyager library catalog :)
  let $id := fn:replace(fn:substring-after(fn:base-uri($marc), "/MARCXML/"), ".xml", "")
  return 
    if (fn:starts-with(fn:base-uri($marc), "/MARCXML/")) then
            <id xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space($id) }</id>
    else if (fn:starts-with(fn:base-uri($marc), "/new-marcxml/")) then
                 <id xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space($newID) }</id>
         else ()
};

(:~
 : Returns a <series> element containing the series title from the standard fields used for series in the MARC record (there may be more than one series for an individual AV item).
 :)
declare function av:get-av-series($marc as element(m:record)?)
as element(ia:series)*
{
  let $seriesTitles := $marc/m:datafield[@tag = ("410", "440", "490")]
  for $seriesTitle in $seriesTitles
  where $seriesTitle
  return <series xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space($seriesTitle) }</series> 
 };        
  
(:~
 : Returns a <series> element containing the series title from the 500 field of the MARC record (there may be more than one series for an individual AV item).
 :)
declare function av:get-av-series-from-note($marc as element(m:record)?)
as element(ia:series)*
{
  let $seriesTitle := $marc/m:datafield[@tag = ("410", "440", "490")][1]
  let $_500s := $marc/m:datafield[@tag = "500"]
  for $_500 in $_500s
  where fn:not($seriesTitle)
  and  (fn:matches($_500, "lectures?(\s|$)", "i") (: Avoids "lectureship" :) 
             or fn:matches($_500, "symposium", "i")
             or fn:matches($_500, "conference", "i") 
             or fn:matches($_500, "seminar(\s|$)", "i") (: Avoids "Seminary" :)
             or fn:matches($_500, "I\.?O\.?T\.?", "i")
             or fn:matches($_500, "institute", "i")
             or fn:matches($_500, "series(\s|$)", "i")
             or fn:matches($_500, "forum", "i"))
  return <series xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space($_500) }</series>
};

(:~ Returns a <duration> element from the 300 field (subfield a) of the MARC record. :)
declare function av:get-duration($marc as element(m:record)?)
(: as element(ia:duration) :)
{
  let $_300 := $marc/m:datafield[@tag = "300"]/m:subfield[@code = "a"]
  where fn:contains($_300, "min")
  return <duration xmlns="http://digital.library.ptsem.edu/ia">{ fn:normalize-space(fn:replace($_300, ".*\((.*)\).*", "$1")) }</duration>
};

(:~ Returns a <recordingDatePlace> element from the 518 field of the MARC record. This is a note field for date/time and place of an event. Our MARC records primarily use this field for the date of recording. :)
declare function av:get-recording-date($marc as element(m:record)?)
as element(ia:recordingDate)?
{
  let $_518 := $marc/m:datafield[@tag = "518"]
  where $_518
  return <recordingDate xmlns="http://digital.library.ptsem.edu/ia">{ fn:replace(fn:normalize-space($_518), "\.$", "") }</recordingDate>
};

(: This function extracts the date value for the original recording from the MARC record. It uses the character position for the first or second 4-digit date from the 008 fixed field. For the date types coded "p", "r" or "t" the second date is the date of the orignial recording. :)

declare function av:get-av-date($marc as node()*)
as xs:string?
{
  let $dateType := fn:substring($marc/m:controlfield[@tag = "008"], 7, 1)
  let $date1 := fn:substring($marc/m:controlfield[@tag = "008"], 8, 4)
  let $date2 := fn:substring($marc/m:controlfield[@tag = "008"], 12, 4)
  let $_518 := $marc/m:datafield[@tag = "518"]
  return 
    (: Condition added for metadata from Ed. Media audio spreadsheet to get date from 518 field, rather than 008 fixed field. :)
    if ($dateType = "s" and $date1 = "9999" and fn:matches($_518, "\d+")) then
            fn:normalize-space(fn:tokenize($_518, "/")[fn:position() = fn:last()])
    else if ($dateType = "s" and $date1 castable as xs:gYear) then
                 $date1
         else if ($dateType = ("p", "r", "t") and $date2 castable as xs:gYear) then 
                      $date2
              else ()
};

(: This function removes the AV topics that are actually formats, for example, "Compact discs," and provides a revised <topics> element. :)

declare function av:revise-av-topics($marc as node()*)
as element(ia:topics)*
{
  let $topics := marc:get-topics($marc)
  for $position in 1 to fn:count($topics/ia:topic)
  for $topic in $topics/ia:topic[$position]
  return let $revisedTopics := <topics xmlns="http://digital.library.ptsem.edu/ia">
                               {if (fn:matches($topic, "Compact discs", "i") or fn:matches($topic, "Audiocassettes", "i") or fn:matches($topic, "Video", "i")) then
                                     fn:remove($topics/*, $position)
                                else ()}</topics>
         where $revisedTopics != ""
         return $revisedTopics


};

(:~ Returns a <genre> element indicating the genre of the item based on analysis of key words/phrases in the MARC record. :)
declare function av:get-av-genre($marc as element(m:record)?)
as element(ia:genre)?
{
  let $genre :=
(: First, isolate panel discussions. There are no sermons that contain the word "panel" in the MARC record.  :)
           if (fn:matches($marc, "panel", "i"))
           then <genre xmlns="http://digital.library.ptsem.edu/ia">Panel Discussion</genre>
           
(: Second, isolate formal addresses. These are addresses given at either annual or occassional events on campus. :)           
           else 
           if (fn:matches($marc, "commencement", "i") or fn:matches($marc, "convocation", "i") or fn:matches($marc, "baccalaureate", "i"))
           then <genre xmlns="http://digital.library.ptsem.edu/ia">Address</genre>
      
(: Third, isolate sermons. After lectures, sermons are the largest category of AV items. :)      
           else 
           if ((fn:matches($marc, "sermon", "i") or fn:matches($marc, "chapel", "i") or fn:matches($marc, "worship service", "i"))
                (: These four conditions are for lecture titles that contain the word "sermon" :)
                and fn:not(fn:matches($marc, "Sermon on the Mount", "i"))
                and fn:not(fn:matches($marc, "Sermon: Content", "i"))
                and fn:not(fn:matches($marc, "in his nonpublished sermons", "i"))
                and fn:not(fn:matches($marc, "Why Sermons Misfire", "i")))
           then <genre xmlns="http://digital.library.ptsem.edu/ia">Sermon</genre>
           
(: Fourth, isolate interviews. There are some sermons that contain the word "interview," but the sermons are the primary content. :)           
           else 
           if (fn:matches($marc, "interview", "i"))
           then <genre xmlns="http://digital.library.ptsem.edu/ia">Interview</genre>

(: Last, default to lecture, since the majority of AV items are lectures. We are using the genre term "Public Lecture" to distinguish from "Class Lecture." :)
           else <genre xmlns="http://digital.library.ptsem.edu/ia">Public Lecture</genre>
  return $genre
};

(:~
 : Returns <note> elements from the 500 fields of the MARC record (there may be more than one note for an individual AV item).
 : Filters out extraneous notes or notes used for series titles.
 :)
declare function av:get-av-notes($marc as element(m:record)?)
as element(ia:note)*
{
  let $seriesFromNote := av:get-av-series-from-note($marc)
  let $_500s := $marc/m:datafield[@tag = "500"]
  for $_500 in $_500s
  (: Need to normalize $_500 string value for equality comparison with $seriesFromNote string :)
  let $norm_500 := fn:normalize-space($_500) 
  where 
     fn:not(fn:matches($_500, "Compact disc", "i")) 
     and fn:not(fn:matches($_500, "Master ID", "i"))
     and fn:not(fn:matches($_500, "For sale", "i")) (: Will also filter out "Not for sale" :)
     and fn:not(fn:matches($_500, "Digitally encoded", "i")) (: Administrative note not for public display :)
     and fn:not(fn:matches($_500, "VHS format", "i"))
  return
  (: Must test for existence of $seriesFromNote, otherwise, $seriesFromNote equals the empty sequence and $_500 note will not be extracted :)
    if ($seriesFromNote and $norm_500 != $seriesFromNote) then
       <note xmlns="http://digital.library.ptsem.edu/ia">{ $norm_500 }</note>
    else if (fn:not($seriesFromNote)) then
            <note xmlns="http://digital.library.ptsem.edu/ia">{ $norm_500 }</note>
         else ()
              
};


  







